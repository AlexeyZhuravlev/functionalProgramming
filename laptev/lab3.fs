open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "aa_Zhur@mail.ru"

let explode (s:string) =
  [for c in s -> c]

type Token =
  | OpenBrace | CloseBrace
  | OpenBracket | CloseBracket
  | Colon | Comma
  | String of string
  | Number of int
  | Boolean of bool
  | Null

let tokenize source =
  let rec parseString acc = function
    | '\\' :: '"' :: t -> parseString (acc + "\"") t
    | '\\' :: 'n' :: t -> parseString (acc + "\n") t
    | '"' :: t -> acc, t
    | c :: t -> parseString (acc + c.ToString()) t
    | _ -> failwith "Malformed string."
 
  let rec token acc = function
    | (x :: _) as t when List.exists ((=)x) [')'; ':'; ','; ']'] -> acc, t
    | w :: t when Char.IsWhiteSpace(w) -> acc, t
    | [] -> acc, [] // end of list terminates
    | c :: t -> token (acc + (c.ToString())) t

  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t
    | '{' :: t -> tokenize' (OpenBrace :: acc) t
    | '}' :: t -> tokenize' (CloseBrace :: acc) t
    | '[' :: t -> tokenize' (OpenBracket :: acc) t
    | ']' :: t -> tokenize' (CloseBracket :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '"' :: t -> // start of string
      let s, t' = parseString "" t
      tokenize' (String s :: acc) t'    
    | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
    | d :: t -> // остались числа
      let n, t' = token (d.ToString()) t
      tokenize' (Number (try Convert.ToInt32 n with e -> 0)  :: acc) t'
    | [] -> List.rev acc
    | _ -> failwith "Tokinzation error"
  tokenize' [] source

type JSON =
  | Object of (string * JSON) list
  | Array of JSON list
  | Number of int
  | String of string
  | Boolean of bool
  | Null

let rec parseTokens json =
  let rec parse' json =
    let rec parseObject list = function
      | CloseBrace :: t -> (Object (List.rev list)), t
      | Comma :: t -> parseObject list t
      | Token.String s :: Colon :: t ->
        let a, t = parse' t
        parseObject ((s, a) :: list) t
      | _ -> failwith "Incorrect object"
    let rec parseArray list = function
      | CloseBracket :: t -> (Array (List.rev list)), t
      | Comma :: t -> parseArray list t
      | ob -> 
        let a, t = parse' ob
        parseArray (a :: list) t  
    match json with
      | OpenBrace :: t -> parseObject [] t
      | OpenBracket :: t -> parseArray [] t
      | Token.Null :: t -> JSON.Null, t
      | Token.String s :: t -> JSON.String s, t
      | Token.Number s :: t -> JSON.Number s, t
      | Token.Boolean s :: t -> JSON.Boolean s, t
      | _ -> failwith "Incorrect identification"
  match parse' json with
    | res, [] -> res
    | _ -> failwith "Wrong JSON structure"

let parse str = 
    str |> explode |> tokenize |> parseTokens

let lab3 json =
    let rec calculateDepth = function
        | Number _ | String _ | Boolean _ | Null -> 1
        | Object([]) -> 1
        | Object(lst) -> 
            (List.map (snd >> calculateDepth) lst |> List.max) + 1
        | Array([]) -> 1
        | Array(arr) ->
            (List.map calculateDepth arr |> List.max)  + 1
    calculateDepth json

let rec stringify = function
  | Object(lst) ->
        "{" + (List.map (fun pair -> "\"" + fst pair + "\" : " + stringify (snd pair)) lst |> String.concat ", ") + "}"
  | Array(arr) ->
        "[" + ((List.map stringify arr) |> String.concat ", ") + "]"
  | Number(a) -> string a
  | String(a) -> a
  | Boolean(true) -> "true"
  | Boolean(false) -> "false"
  | Null -> "null"

let rec generate() = 
    let rnd = new Random()
    let rec generator() = 
        match rnd.Next(6) with
            | 0 -> Null
            | 1 -> Number(rnd.Next())
            | 2 -> String(rnd.Next().ToString())
            | 3 -> Boolean(if (rnd.Next(2) = 0) then true else false)
            | 4 -> 
                let n = rnd.Next(5)
                let rec generate' acc = function
                    | 0 -> acc
                    | n -> generate' (generator()::acc) (n - 1)
                Array(generate' [] n)
            | _ ->
                let n = rnd.Next(5)
                let rec generate' acc = function
                    | 0 -> acc
                    | n -> generate' ((rnd.Next().ToString(), generator())::acc) (n - 1)
                Object(generate' [] n)
    generator()

let tests() =
    let strmanual = "[false, {\"a\": [[1, 2]]}]"
    let jsonmanual = parse strmanual
    let resmanual = lab3 jsonmanual
    printfn "json string: %s\nGenerated object: %A\nTree depth: %d\n" strmanual jsonmanual resmanual
    let randjson = generate()
    let str = stringify randjson
    let resrand = lab3 randjson
    printfn "json string: %s\nGenerated object: %A\nTree depth: %d\n" str randjson resrand

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
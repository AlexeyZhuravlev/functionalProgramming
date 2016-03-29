#r "../packages/FSharp.Data.2.2.1/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "aa_Zhur@mail.ru"

let site = "http://wikimipt.org"

let startPage =  site + "/index.php?\
                         title=%D0%9A%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D1%8F:\
                         %D0%9F%D1%80%D0%B5%D0%BF%D0%BE%D0%B4%D0%B0%D0%B2%D0%B0%D1%82%D0\
                         %B5%D0%BB%D0%B8_%D0%BF%D0%BE_%D0%B0%D0%BB%D1%84%D0%B0%D0%B2%D0%\
                         B8%D1%82%D1%83"

let isName (str:string) =
    let upperCaseLetters = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЭЮЯ"
    String.exists (fun c -> c = str.[0]) upperCaseLetters

let isFullName (str:string) =
    let words = str.Split(' ') |> Array.toList
    ((List.length words) >= 2) && ((List.length words) <= 3) && (List.forall isName words)
    && not (String.exists (fun c -> c = '.') str)

let getTeachersLinksFromPage (link:string) = async {
  let! html = HtmlDocument.AsyncLoad(link)
  printfn "getting links from %s" link
  return html.Descendants ["ul"]
            |> Seq.collect (fun x -> x.Descendants ["a"])
            |> Seq.filter(fun x -> isFullName (x.AttributeValue("title")))
            |> Seq.map(fun x -> site + x.AttributeValue("href"))
            |> Seq.toList
  }

let rec getAllSourcePages (link:string) =
    let html = HtmlDocument.Load(link)
    let nextPage = html.Descendants ["a"]
                    |> Seq.filter(fun x -> x.InnerText() = "следующие 200")
                    |> Seq.map (fun x -> x.AttributeValue("href")) 
                    |> Seq.toList
    match nextPage with
        | [] -> [link]
        | x::xs -> link::(getAllSourcePages (site + x))

let getInfoFromPage (link:string) = async {
    let! html = HtmlDocument.AsyncLoad(link)
    let name = html.Descendants ["link"]
                |> Seq.filter (fun x -> x.AttributeValue("rel") = "ExportRDF")
                |> Seq.map (fun x -> x.AttributeValue("title"))
                |> Seq.exactlyOne
    let rates = html.Descendants ["span"]
                |> Seq.filter (fun x -> x.AttributeValue("class") = "starrating-avg")
                |> Seq.map (fun x -> (x.InnerText().Split(' ').[0]))
                |> Seq.map (fun x -> if (x = "(") then None else Some(float x))
                |> Seq.toList
    printfn "Inspecting %s" name
    return name, rates.[0], rates.[3]
}

let lab2 () =
  let pages = getAllSourcePages startPage
  let links = pages |> List.map getTeachersLinksFromPage |> Async.Parallel |> Async.RunSynchronously
              |> Array.toList |> List.concat
  let data = links |> List.map getInfoFromPage |> Async.Parallel |> Async.RunSynchronously |> Array.toList
             |> List.choose (fun (x, y, z) -> if (y = None || z = None) then None else Some(x, y.Value, z.Value))
  let cleverest = data |> List.sortBy (fun (a, b, c) -> b) |> List.rev 
                  |> Seq.take 50 |> Seq.map (fun (a, b, c) -> (a, b)) |> Seq.toList
  let hardest = data |> List.sortBy (fun (a, b, c) -> c) |> Seq.take 50 
                    |> Seq.map (fun (a, b, c) -> (a, c)) |> Seq.toList
  cleverest, hardest
 
  
let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab2().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))
    

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

type 'a Tree = Node of 'a * 'a Tree * 'a Tree | Leaf of 'a
let rec numerate' (acc:int) = function 
    | Leaf(a) -> Leaf(a, acc), acc + 1
    | Node(a, L, R) -> let L, number = numerate' acc L
                       let current = (a, number)
                       let R, largestNumber = numerate' (number + 1) R
                       Node(current, L, R), largestNumber + 1
let numerate tree = fst (numerate' 0 tree)

let t = Node(3, Node(6, Leaf(2), Leaf(6)), Leaf(4))
let numbered = numerate t
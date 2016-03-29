open System
open System.IO

type Tree = Node of (int * int * int) * ((string * Tree) list)

let initial = Node((0, 0, 0), [])

let split (x:string) = x.Split ' ' |> Array.toList

let adding_comprehention = function
    | "1-0" -> (fun (a, b, c) -> (a + 1, b, c + 1))
    | "0-1" -> (fun (a, b, c) -> (a, b + 1, c + 1))
    | _ -> (fun (a, b, c) -> (a, b, c + 1))

let rec add_sequence_to_tree f activeMoves = function
    | Node(state, moves) -> 
        if List.isEmpty activeMoves
        then Node(f state, moves)
        else
            let move = List.head activeMoves
            let lasted = List.tail activeMoves
            match List.tryFindIndex (fst >> ((=) move)) moves with
                | None -> Node(f state, (move, add_sequence_to_tree f lasted initial)::moves)
                | Some(index) -> 
                    let newmoves = List.mapi (fun i (str, tr) -> if i <> index then (str, tr)
                                                                 else (str, add_sequence_to_tree f lasted tr)) moves
                    Node(f state, newmoves)

let add_game_to_tree tree = function
    | result::moves -> add_sequence_to_tree (adding_comprehention result) moves tree
    | _ -> failwith "Invalid sequence"

let maxFromSons f moves = List.maxBy (snd >> fst) (List.map (fun (str, tr) -> (str, f tr)) moves)

let rec getLongestDebute = function
    | Node((_, _, c), moves) ->
      if c < 2 || (List.isEmpty moves) then (0.0, [])
      else maxFromSons getLongestDebute moves
           |> (fun (a, (b, c)) -> (b + 1.0, a::c))

let rec getBestDebute criterium = function
    | Node((_, _, c) as state, moves) ->
        if c < 2 then (0.0, [])
        elif List.isEmpty moves then (criterium state, [])
        else let pretendent = maxFromSons (getBestDebute criterium) moves
             if (snd >> fst) pretendent >= (criterium state)
             then ((snd >> fst) pretendent, (fst pretendent)::((snd >> snd) pretendent))
             else (criterium state, [])

let BlackCriterium = function
    | (_, b, c) -> (float b) / (float c)

let WhiteCriterium = function
    | (a, _, c) -> (float a) / (float c)

let getWorstMove = 
    let rec getWorstMove' depth = function
        | Node(state, moves) ->
            if List.isEmpty moves then (0.0, [])
            else let another = maxFromSons (getWorstMove' (depth + 1)) moves
                 let criterium = if (depth % 2 = 0) then WhiteCriterium
                                                    else BlackCriterium
                 let this = List.map (fun (str, Node(newstate, newMoves)) -> 
                                        (str, criterium state - criterium newstate)) moves 
                            |> List.maxBy snd
                 if (snd this) > ((snd >> fst) another) then (snd this, [fst this])
                                                        else (((snd >> fst) another), (fst another)::((snd >> snd) another))
    getWorstMove' 0


let printGameList lst =
    List.iteri (fun i str -> if i % 2 = 0 then printf "%d. %s " (i / 2) str else printfn "%s" str) lst

[<EntryPoint>]
let main args =
    let logs = ["D:\1.txt"]
    let positions = 
        logs |> Seq.map (fun f -> async { return(File.ReadAllLines f) |> Array.toList}) 
        |> Async.Parallel |> Async.RunSynchronously |> Array.toList |> List.concat
    let splittedPositions = List.map split positions
    let gametree = List.fold add_game_to_tree initial splittedPositions
    let tasks = [ async { return (getLongestDebute gametree) };
                  async { return (getBestDebute WhiteCriterium gametree) };
                  async { return (getBestDebute BlackCriterium gametree) };
                  async { return (getWorstMove gametree) } ]
    let results = Async.RunSynchronously (Async.Parallel tasks)
    printfn "Самый длинный дебют:\n"
    printGameList (snd results.[0])
    printfn "\n"
    printfn "Наилучший дебют для белых c вероятностью выигрыша %f:\n" (fst results.[1])
    printGameList (snd results.[1])
    printfn "\n"
    printfn "Наилучший дебют для чёрных c вероятностью выигрыша %f:\n" (fst results.[2])
    printGameList (snd results.[2])
    printfn "\n"
    printfn "Наихудший ход:"
    printGameList (snd results.[3])
    printfn " <<-- После последнего хода вероятность выигрыша упала на %f" (fst results.[3])
    0
// #r "../packages/FSharp.Data.2.2.1/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO

let dampingFactor = 0.85
let eps = 0.00001

let getDomain (site:string) =
    let tokens = site.Split('/')
    tokens.[0] + "//" + tokens.[2]

let isHtmlPage (link:string) =
    let tokens = link.Split('/')
    let lastToken = tokens.[Array.length tokens - 1]
    (link = getDomain link) || lastToken.EndsWith(".html") || lastToken.EndsWith(".htm") || lastToken.EndsWith(".php")
        || (not (lastToken.Contains(".")) && not (lastToken.StartsWith("javascript")) && not (link.Contains("javascript"))
            && not (lastToken.Contains("?")) && not (lastToken.Contains("#")) && not (lastToken.Contains(":")))

let getLinks (link:string) = async {
    try
        printfn "Inspecting %s" link
        let! html = HtmlDocument.AsyncLoad(link)
        let domain = getDomain link
        let links = html.Descendants["a"]
                      |> Seq.map (fun x -> x.AttributeValue("href"))
                      |> Seq.map (fun x -> x.Trim())
                      |> Seq.map (fun x -> x.TrimStart('.').TrimStart('/'))
                      |> Seq.map (fun x -> if (x.StartsWith("http")) then x else domain + "/" + x.TrimStart('.').TrimStart('/'))
                      |> Seq.map (fun x -> x.TrimEnd('/'))
                      |> Seq.filter isHtmlPage
                      |> Seq.filter(fun x -> x.StartsWith(domain))
                      |> Seq.distinct
                      |> Seq.toList
        return link, links
    with
        | :? System.Net.WebException -> return link, []
        | :? System.Exception -> return link, []
}

let addLinks (map, candidates) = fun (link, links) ->
    printfn "adding links from %s" link
    let linksAdder map element = 
        let previousValue = Map.find element map
        let newValue = match previousValue with
                        | (a, b) -> (a, (link::b))
        Map.add element newValue map
    let elementAdder pair element =
        let map = fst pair
        let lst = snd pair
        if (Map.containsKey element map) then map, lst
                                         else Map.add element (0, []) map, element::lst
    let previousValue = Map.find link map
    let newValue = match previousValue with
                    | (a, b) -> (List.length links, b)
    let (newMap, newCandidates) = List.fold elementAdder ((Map.add link newValue map), candidates) links
    (List.fold linksAdder newMap links), newCandidates

let getAllPages startLink = 
    let rec addTasks linkGetTasks = fun (map, allPages) ->
        let newMap, newCandidates = linkGetTasks |> Async.RunSynchronously |> Array.toList |> List.fold addLinks (map, [])
        if (List.length newCandidates = 0) 
            then (newMap, allPages)
            else      
                let tasks = newCandidates |> List.map getLinks |> Async.Parallel
                addTasks tasks (newMap, allPages @ newCandidates)
    addTasks ([getLinks startLink] |> Async.Parallel) (Map.empty.Add(startLink, (0, [])), [startLink])


let localPr map pr page =
    let leavePr = Map.find page map |> snd 
                  |> List.map (fun x ->  (Map.find x pr) / float(fst (Map.find x map)))
                  |> List.sum
    (1. - dampingFactor) + dampingFactor * leavePr


let recalulatePr map links pr numberOfPages =
    let folder = 
        fun (mp, delta) el -> 
            let newPr = localPr map pr el
            let oldPr = Map.find el pr
            (Map.add el (localPr map pr el) mp, min delta (abs (oldPr - newPr)))
    links |> List.fold folder (Map.empty, numberOfPages)
    
let rec calculatePr map links pr numberOfPages =
    let newPr, delta = recalulatePr map links pr numberOfPages
    printfn "Caclucalting pagerank: current delta is %f" delta
    if (delta < eps) then newPr
                     else calculatePr map links newPr numberOfPages 

let pageRank initialPage = 
    let map, list = getAllPages initialPage
    printfn "Scheme was built"
    let numberOfPages = float (List.length list)
    let startPr = list |> List.map (fun x -> (x, 1.)) |> Map.ofList
    calculatePr map list startPr numberOfPages


[<EntryPoint>]
let main args =
    let page = args.[0]
    use file = new StreamWriter(args.[1])
    let pr = pageRank page
    printfn "Pagerank calculated"
    pr |> Map.toList |> List.sortBy snd |> List.rev
    |> List.iter (fun x -> file.WriteLine(sprintf "%s - %f" (fst x) (snd x)))
    0
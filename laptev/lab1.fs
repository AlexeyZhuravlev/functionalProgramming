module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес
let email = "aa_Zhur@mail.ru"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть

let fTailor (x : float) : float = 2.0 * ((cos x) ** 2.0 - 1.0) // функция, которую раскладываем
let n, a, b = 20., 0.0, 0.5 // интервал

let rec factorial = function
    | 0 -> 1L
    | n -> int64(n) * factorial(n - 1)

let rec power x = function 
    | 0 -> 1.0
    | n -> x * power x (n - 1)

let tailor x : Result = 
    let rec tailor' x n acc = 
        let tailorElement x n = 
            (if (n % 2 = 0) then 1.0 else -1.0) 
            * (power (2.0 * x) (2 * n))
            / float((factorial (2 * n)))
        let t1 = tailorElement x n
        if (abs t1 < delta) then (acc, n)
                            else tailor' x (n + 1) (acc + t1)
    tailor' x 1 0.0
    
let tailorA (x : float) : Result = 
    let rec tailor' x n acc previous =
        let next = previous * (-4.0 * x * x) / float((2 * n - 1) * 2 * n)
        if (abs next < delta) then (acc, n)
                              else tailor' x (n + 1) (acc + next) next
    tailor' x 1 0.0 1.0

let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

// *** Вторая часть

let maxN = 100000

let fSolve1, a1, b1 = (fun x -> (sqrt (1.0 - x) - (tan x))), 0., 1.
let fSolve2, a2, b2 = (fun x -> (x + cos(x ** 0.52 + 2.0))), 0.5, 1.
let fSolve3, a3, b3 = (fun x -> (3.0 * (log x) ** 2.0 + 6.0 * (log x) - 5.0)), 1., 3.

let deriv f x = ((f (x + delta)) - (f x)) / delta 

let iter f a b : Result =
    let start = (a + b) / 2.0
    let g = fun x -> (x - (f x) / (deriv f start))
    let rec iter' n point =
        let newPoint = g point
        if (abs(point - newPoint) > (b - a) || n = maxN) then (nan, n)
        elif (abs (point - newPoint) < delta) then (point, n)
                                              else iter' (n + 1) newPoint
    iter' 0 start

let newton f a b : Result = 
    let start = (a + b) / 2.0
    let rec iter' n point =
        let newPoint = point - (f point) / (deriv f point)
        if (abs(point - newPoint) > (b - a) || n = maxN) then (nan, n)
        elif (abs(point - newPoint) < delta) then (point, n)
                                           else iter' (n + 1) newPoint
    iter' 0 start

let dichotomy =
    let rec dichotomy' i f a b : Result = 
        let c = (a + b) / 2.
        if (abs(b - a) < delta) then (c, i)
        elif ((f a) * (f c) <= 0.) then dichotomy' (i + 1) f a c
        elif ((f c) * (f b) <= 0.) then dichotomy' (i + 1) f c b
        else (nan, i)
    dichotomy' 0 // чтобы воспользоваться каррированием

let printSolve fSolve a b =
    [iter; newton; dichotomy] 
    |> List.map (fun f -> f fSolve a b) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

let printSolves() =
    printSolve fSolve1 a1 b1
    printSolve fSolve2 a2 b2
    printSolve fSolve3 a3 b3

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

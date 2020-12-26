open System.IO
open System.Text.RegularExpressions


let adjacentTwins (x:string) = Regex.IsMatch(x, @"(.)\1")
adjacentTwins "123345"

let adjacentTwins2 (x:string) =
    [   for m in Regex.Matches(x, @"((.)(\2+))") do
        yield m.Groups.[1].Value.Length ]
    |> List.contains 2
adjacentTwins2 "12334445"

let neverDecrease = Seq.pairwise >> Seq.exists (fun (a, b) -> a > b) >> not
neverDecrease "1233"

let toTuple = function
    | [a;b] -> a, b
    | x     -> failwithf "unexpected: %A" x

let parse (input:string) =
    input.Split '-'
    |> Array.toList
    |> List.map int
    |> toTuple
    |> fun (a,b) -> seq {a..b}
let input = File.ReadAllText "day04/input.txt"
let range = parse input

let solve =
    parse
    >> Seq.filter (string >> fun x -> adjacentTwins x && neverDecrease x) 
    >> Seq.length
solve input

let solve2 =
    parse
    >> Seq.filter (string >> fun x -> adjacentTwins2 x && neverDecrease x) 
    >> Seq.length
solve2 input
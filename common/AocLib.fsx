module AocLib

let split (delimiter:char) (input:string) =
    input.Split delimiter |> Array.toList

let trimStr (input:string) = input.Trim()

let toTuple = function
    | [a;b] -> a, b
    | x     -> failwithf "unexpected: %A" x

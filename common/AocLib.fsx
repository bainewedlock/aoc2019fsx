module AocLib

let split (delimiter:char) (input:string) =
    input.Split delimiter |> Array.toList

let trimStr (input:string) = input.Trim()

let toTuple = function
    | [a;b] -> a, b
    | x     -> failwithf "unexpected: %A" x

let bfs fAdjacentEdges (start:'a) = seq {
    let mutable queue = [start,[]]
    let discovered = System.Collections.Generic.HashSet<'a>([start])
    while queue <> [] do
        let (v,log)::rest = queue
        queue <- rest
        if log <> [] then yield log
        for edge in fAdjacentEdges v do
            if not (discovered.Contains edge) then
                discovered.Add edge |> ignore
                queue <- (edge,edge::log)::queue }
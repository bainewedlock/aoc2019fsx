open System.IO

let demoinput = "1,9,10,3,2,3,11,0,99,30,40,50"

type Program = {
    Cursor : int
    Code : Map<int, int> }

let parse (input:string) =
    input.Split ','
    |> Array.toList
    |> List.map int
    |> List.indexed
    |> Map
    |> fun m -> { Code = m; Cursor = 0 }
let demoprogram = parse demoinput

let currentInstruction p =
    [ p.Cursor .. p.Cursor + 3]
    |> List.map (fun i -> Map.tryFind i p.Code |> Option.defaultValue 0)

let execute p op a b c =
    let result =
        [a; b]
        |> List.map (fun x -> p.Code.[x])
        |> List.reduce op
    { p with Code = p.Code.Add(c, result); Cursor = p.Cursor + 4 }
execute demoprogram (*) 9 10 3

let rec run (p:Program) =
    match currentInstruction p with
    | 99::_     -> p
    | [1;a;b;c] -> execute p (+) a b c |> run
    | [2;a;b;c] -> execute p (*) a b c |> run
    | x         -> failwithf "unexpected %A" x
run demoprogram

let nounAddress = 1
let verbAddress = 2

let fixState noun verb p =
    { p with Code = p.Code.Add(nounAddress, noun).Add(verbAddress, verb) }

let input = File.ReadAllText "day02/input.txt"
let output p = p.Code.[0]

let solvePart1 = parse >> fixState 12 2 >> run >> output >> sprintf "part1: %d"
solvePart1 input 

let runVariant (noun, verb) =
    parse input
    |> fixState noun verb
    |> run
    |> output

let solvePart2 input =
    [ for noun in [0..99] do for verb in [0..99] do yield noun, verb ]
    |> List.find (runVariant >> ((=)19690720))
    |> fun (a, b) -> sprintf "part2: 100*%d+%d=%d" a b (100*a+b)
solvePart2 input

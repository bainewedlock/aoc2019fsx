open System.IO

#load "../common/AocLib.fsx"
open AocLib

type Tree = Map<string,string Set>

let demoinput = "COM)B
                   B)C
                   C)D
                   D)E
                   E)F
                   B)G
                   G)H
                   D)I
                   E)J
                   J)K
                   K)L"

let parse : string -> Tree =
    split '\n'
    >> List.map (trimStr >> split ')' >> toTuple)
    >> List.map (fun (a,b) -> b, Set [a])
    >> Map
let demotree = parse demoinput

let orbitsOf (t:Tree) =
    List.unfold (
        t.TryFind
        >> Option.map (Seq.exactlyOne >> fun o -> o, o))

let solvePart1 input =
    let t = parse input
    t
    |> Map.toList
    |> List.collect (fst >> orbitsOf t)
    |> List.length
solvePart1 demoinput


let input = File.ReadAllText "day06/input.txt"
solvePart1 input


let reverse (tree:Tree) : Tree =
    tree
    |> Map.toList
    |> List.map (fun (a,b) -> b,a)
    |> List.groupBy fst
    |> List.map (fun (k, vs) -> k |> Seq.exactlyOne, vs |> List.map snd |> Set)
    |> Map
reverse demotree

let demoinput2 = "COM)B
                    B)C
                    C)D
                    D)E
                    E)F
                    B)G
                    G)H
                    D)I
                    E)J
                    J)K
                    K)L
                    K)YOU
                    I)SAN"
let demotree2 = parse demoinput2

let adjacentEdges (tree:Tree) =
    let tree':Tree = reverse tree
    fun edge ->
        [ tree; tree' ]
        |> List.choose (Map.tryFind edge >> Option.map Set.toList)
        |> List.concat
adjacentEdges demotree2 "D"

let solvePart2 input =
    let tree = parse input
    bfs (adjacentEdges tree) "YOU"
    |> Seq.find (List.head >> ((=)"SAN"))
    |> Seq.length
    |> fun l -> l - 2
solvePart2 input
open System.IO
open System.Text.RegularExpressions

#load "../common/AocLib.fsx"
open AocLib

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

let parse =
    split '\n'
    >> List.map (trimStr >> split ')' >> toTuple)
    >> List.map (fun (a,b) -> b,a)
    >> Map
parse demoinput

let orbitsOf (t:Map<string, string>) =
    List.unfold (t.TryFind >> Option.map (fun o' -> o', o'))

let solve input =
    let t = parse input
    t
    |> Map.toList
    |> List.collect (fst >> orbitsOf t)
    |> List.length
solve demoinput
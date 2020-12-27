open System
open System.IO

let width  = 25
let height = 6

let solve =
    Seq.toList
    >> List.chunkBySize (width*height)
    >> List.map (List.countBy id >> Map)
    >> List.minBy (fun x -> x.['0'] )
    >> fun x -> x.['1'] * x.['2']
let input = File.ReadAllText "day08/input.txt"
solve input

let overlay (i:char list) (i2:char list) =
    List.zip i i2
    |> List.map (function
        | '2', x -> x
        |  x , _ -> x)

let replace (c:string) (c':string) (text:string) =text.Replace(c, c')

let solve2 =
    Seq.toList
    >> List.chunkBySize (width*height)
    >> List.reduce overlay
    >> List.chunkBySize width
    >> List.map (List.toArray >> String)
    >> List.map (replace "0" "  " >> replace "1" "##")
solve2 input

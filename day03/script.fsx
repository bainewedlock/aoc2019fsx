open System.IO

let demoinput = "R8,U5,L5,D3
                 U7,R6,D4,L4"

let split (delim:char) (text:string) = text.Split delim |> Array.toList

let trim (text:string) = text.Trim()

type Command = { Direction : int * int; Length : int }

let directions = Map [
        'R', (+1, +0)
        'L', (-1, +0)
        'U', (+0, +1)
        'D', (+0, -1) ]

let parseCommand (line:string) =
    {   Direction = directions.[line.[0]]
        Length    = line.Substring(1) |> int }
parseCommand "U99"

let parse (input:string) =
    input
    |> split '\n'
    |> List.map (trim >> split ',' >> List.map parseCommand)
let demowire = parse demoinput

let calcEdge (x0, y0) { Length=l; Direction=(dx,dy) } =
    [1..l]
    |> List.scan (fun (x,y) _ -> x+dx, y+dy) (x0, y0)  
    |> List.tail
calcEdge (2,10) <| parseCommand "U3"

let calcTrail cmds =
    cmds
    |> List.scan (fun (p,_) c -> 
        let e' = calcEdge p c
        let p' = e' |> List.last
        (p', e')) ((0,0), [])
    |> List.collect snd
let demotrails = demowire |> List.map calcTrail

let toTuple = function
    | [a;b] -> a,b
    | x     -> failwith "unexpected: %A" x

let manhattanDistance (x,y) = abs x + abs y
manhattanDistance (-1,5)

let intersections t1 t2 = Set.intersect (Set t1) (Set t2)

let solvePart1 input =
    let t1,t2 = parse input |> List.map calcTrail |> toTuple
    intersections t1 t2
    |> Seq.map manhattanDistance
    |> Seq.min
solvePart1 demoinput

solvePart1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
            U62,R66,U55,R34,D71,R55,D58,R83"

solvePart1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
            U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

let input = File.ReadAllText "day03/input.txt"
solvePart1 input

let distanceToPos pos trail =
    trail
    |> List.indexed
    |> Seq.pick (fun (i,x) -> if pos=x then Some (i+1) else None)
distanceToPos (3,3) demotrails.[0]

let solvePart2 input =
    let t1,t2 = parse input |> List.map calcTrail |> toTuple
    intersections t1 t2
    |> Seq.map (fun p ->
        distanceToPos p t1 + distanceToPos p t2)
    |> Seq.min
solvePart2 demoinput

solvePart2 "R75,D30,R83,U83,L12,D49,R71,U7,L72
            U62,R66,U55,R34,D71,R55,D58,R83"

solvePart2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
            U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

solvePart2 input
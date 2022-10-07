namespace AoC2019

open Utils

module Day03 =

    type Direction =
        | U of int
        | D of int
        | L of int
        | R of int

    let parseDirection (dir: string) : Direction =
        match Seq.head dir with
        | 'U' -> U(int (dir.Substring 1))
        | 'D' -> D(int (dir.Substring 1))
        | 'L' -> L(int (dir.Substring 1))
        | 'R' -> R(int (dir.Substring 1))
        | _ -> failwith $"invalid direction: {dir}"

    let processInput (inputLine: string) : Direction list =
        inputLine
        |> commas
        |> Array.map parseDirection
        |> List.ofArray

    let followDir (dir: Direction) ((xStart, yStart): int * int) : (int * int) list =
        match dir with
        | (U steps) -> List.map (fun step -> (xStart + step, yStart)) [ 1..steps ]
        | (D steps) -> List.map (fun step -> (xStart - step, yStart)) [ 1..steps ]
        | (L steps) -> List.map (fun step -> (xStart, yStart - step)) [ 1..steps ]
        | (R steps) -> List.map (fun step -> (xStart, yStart + step)) [ 1..steps ]

    let followDirDist (dir: Direction) (dist: int) ((xStart, yStart): int * int) : ((int * int) * int) list =
        match dir with
        | (U steps) -> List.map (fun step -> ((xStart + step, yStart), step + dist)) [ 1..steps ]
        | (D steps) -> List.map (fun step -> ((xStart - step, yStart), step + dist)) [ 1..steps ]
        | (L steps) -> List.map (fun step -> ((xStart, yStart - step), step + dist)) [ 1..steps ]
        | (R steps) -> List.map (fun step -> ((xStart, yStart + step), step + dist)) [ 1..steps ]

    let rec tracePath (pos: int * int) (path: Direction list) : (int * int) list =
        match path with
        | [] -> []
        | d :: ds ->
            let positions = followDir d pos
            List.append (positions) (tracePath (List.last positions) ds)

    let rec tracePathDist (pos: int * int) (dist: int) (path: Direction list) : ((int * int) * int) list =
        match path with
        | [] -> []
        | d :: ds ->
            let positions = followDirDist d dist pos
            let (lastPos, lastDist) = List.last positions
            List.append (positions) (tracePathDist lastPos lastDist ds)

    let findOverlaps ((path1, path2): ((int * int) list) * ((int * int) list)) : (int * int) list =
        let set2 = Set.ofList path2

        path1
        |> Set.ofList
        |> Set.filter (fun pos -> Set.contains pos set2)
        |> Set.toList

    let findOverlapsDist ((path1, path2): (((int * int) * int) list) * (((int * int) * int) list)) : int list =
        let set2 = path2 |> List.map fst |> Set.ofList

        let path1Dict = Map.ofList path1
        let path2Dict = Map.ofList path2

        path1
        |> List.map fst
        |> Set.ofList
        |> Set.filter (fun pos -> Set.contains pos set2)
        |> Set.toList
        |> List.map (fun pos -> path1Dict[pos] + path2Dict[pos])

    let manhattanDistance ((x, y): int * int) : int = abs x + abs y

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map processInput
        |> Array.map (tracePath (0, 0))
        |> twoArrayToTuple
        |> findOverlaps
        |> List.map manhattanDistance
        |> List.min
        |> string

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map processInput
        |> Array.map (tracePathDist (0, 0) 0)
        |> twoArrayToTuple
        |> findOverlapsDist
        |> List.min
        |> string

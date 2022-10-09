namespace AoC2019

open Utils

module Day04 =

    let processInput (input: string) : int array =
        let split = input.Split '-'
        [| (int split[0]) .. (int split[1]) |]

    let containsDouble (password: int) : bool =
        password
        |> string
        |> Seq.pairwise
        |> Seq.exists (fun (c1, c2) -> c1 = c2)

    let rec groupDigits (acc: char seq seq) (currentChar: char) (password: char seq) : char seq seq =
        match List.ofSeq password with
        | [] -> acc
        | c :: cs when c = currentChar ->
            groupDigits (Seq.updateAt 0 (Seq.append (Seq.item 0 acc) (Seq.singleton c)) acc) currentChar cs
        | c :: cs -> groupDigits (Seq.append (Seq.singleton [ c ]) acc) c cs

    let containsDoublePart2 (password: int) : bool =
        password
        |> string
        |> groupDigits (Seq.singleton Seq.empty) (password |> string |> Seq.head)
        |> Seq.exists (fun s -> Seq.length s = 2)

    let nonDecreasing (password: int) : bool =
        password
        |> string
        |> Seq.windowed 2
        |> Seq.forall (fun [| c1; c2 |] -> (int c2) >= (int c1))

    let matchesCriteria (password: int) : bool =
        (containsDouble password)
        && (nonDecreasing password)

    let matchesCriteriaPart2 (password: int) : bool =
        (containsDoublePart2 password)
        && (nonDecreasing password)

    let part1 (input: string) : string =
        input
        |> processInput
        |> Array.filter matchesCriteria
        |> Array.length
        |> string

    let part2 (input: string) : string =
        input
        |> processInput
        |> Array.filter matchesCriteriaPart2
        |> Array.length
        |> string

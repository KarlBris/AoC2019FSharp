namespace AoC2019

open Utils
open IntcodeComputer

module Day08 =

    let countChar (ch: char) (chars: char array) : int =
        chars
        |> Array.map (fun c -> if c = ch then 1 else 0)
        |> Array.sum

    let part1 (input: string) : string =
        let a =
            input
            |> stringTrim
            |> Seq.chunkBySize (25 * 6)
            |> Seq.sortBy (countChar '0')
            |> Seq.head

        let ones = (countChar '1' a)
        let twos = (countChar '2' a)

        ones * twos |> string

    let layer (frontLayer: char) (bottomLayer: char) : char =
        match frontLayer with
        | '0' -> '0'
        | '1' -> '1'
        | _ -> bottomLayer

    let part2 (input: string) : string =
        input
        |> stringTrim
        |> Seq.chunkBySize (25 * 6)
        |> Seq.fold (fun state t -> Array.map2 layer state t) (Array.replicate 150 '2')
        |> Array.map (fun c -> if c = '1' then '█' else ' ')
        |> Array.chunkBySize 25
        |> Array.map System.String
        |> Array.iter (fun a -> printfn $"{a}")

        ""

namespace AoC2019

open Utils
open IntcodeComputer

module Day05 =

    let part1 (input: string) : string =
        input
        |> parseProgram
        |> runProgram
        |> ignore
        ""

    let part2 (input: string) : string =
        input
        |> parseProgram
        |> runProgram
        |> ignore
        ""

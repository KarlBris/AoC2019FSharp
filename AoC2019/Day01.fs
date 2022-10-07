namespace AoC2019

open Utils

module Day01 =
    let findFuelSimple (mass: int) : int = (mass / 3) - 2

    let rec findFuelComplex (mass: int) : int =
        let initalFuelReq = max 0 (findFuelSimple mass)

        if initalFuelReq = 0 then
            initalFuelReq
        else
            initalFuelReq + findFuelComplex initalFuelReq

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map (int >> findFuelSimple)
        |> Array.sum
        |> string

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map (int >> findFuelComplex)
        |> Array.sum
        |> string

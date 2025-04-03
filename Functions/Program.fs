open System

let rec digitalSum num : int =
     if num = 0 then 0
     else (num % 10) + (digitalSum (num / 10))

let tailDigitalSum num : int =
     let rec digitalSubSum num currentSum = 
         if num = 0 then currentSum
         else
             let currentNum = num / 10
             let digital = num % 10
             let accumulator = currentSum + digital
             digitalSubSum currentNum accumulator
     digitalSubSum num 0

let rec factorial n =
    match n with
    | 0 -> 1
    | _ -> n * factorial (n - 1)

let chooseFunction flag =
    match flag with
    | true -> tailDigitalSum
    | false -> factorial

[<EntryPoint>]
let main argv =

    printfn "Введите целое число:"
    let number = Console.ReadLine() |> int

    let uprec = digitalSum number
    Console.WriteLine($"Рекурсия вверх: {uprec}")

    let tailrec = tailDigitalSum number
    Console.WriteLine($"Рекурсия вниз: {tailrec}")

    let f1 = chooseFunction true
    let f2 = chooseFunction false

    Console.WriteLine($"chooseFunction true для {number}: {f1 number}")
    Console.WriteLine($"chooseFunction false для {number}: {f2 number}")

    0

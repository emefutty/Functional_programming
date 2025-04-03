open System

let rec digitalSum num : int =
     if num = 0 then 0
     else (num % 10) + (digitalSum (num / 10))


[<EntryPoint>]
let main argv =

    printfn "Введите целое число:"
    let number = Console.ReadLine() |> int

    let uprec = digitalSum number
    Console.WriteLine($"Рекурсия вверх: {uprec}")

    0

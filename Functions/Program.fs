let rec sumOfDigitsTailRec n acc =
    if n = 0 then acc
    else sumOfDigitsTailRec (n / 10) (acc + (n % 10))

let sumOfDigits n = sumOfDigitsTailRec n 0

open System

[<EntryPoint>]
let main argv =

    printfn "Введите целое число:"
    let number = Console.ReadLine() |> int

    let result = sumOfDigits number

    printfn "Сумма цифр числа: %d" result

    0

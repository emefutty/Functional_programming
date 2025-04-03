﻿open System

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

let main7 digit funct init =
    let rec step digit rez =
        let next_rez = funct rez (digit % 10)
        let next_digit = digit / 10
        match next_digit with
        | n when n > 0 -> step n next_rez
        | _ -> next_rez
    step digit init

let main9 digit funct init cond =
    let rec step digit rez =
        let current = digit % 10
        let rez' =
            match cond current with
            | true -> funct rez current
            | false -> rez

        match digit / 10 with
        | 0 -> rez'
        | next -> step (digit / 10) rez'

    step digit init

let favoriteLangReply lang =
    match lang with
    | "f#" | "fsharp" | "f sharp" | "prolog" -> "Хм, понятно... подлизываешься :)"
    | "python" -> "Ну кто бы сомневался."
    | "c++" -> "Интересный выбор."
    | "java" -> "Хороший выбор, одобряю."
    | "ruby" -> "Кажется ты перепутал дисциплины."
    | _ -> "Интересный выбор! Главное, чтобы тебе нравилось."


[<EntryPoint>]
let main argv =

    printfn "Введите целое число:"
    let number = Console.ReadLine() |> int

    (*let uprec = digitalSum number
    Console.WriteLine($"Рекурсия вверх: {uprec}")

    let tailrec = tailDigitalSum number
    Console.WriteLine($"Рекурсия вниз: {tailrec}")

    let f1 = chooseFunction true
    let f2 = chooseFunction false

    Console.WriteLine($"chooseFunction true для {number}: {f1 number}")
    Console.WriteLine($"chooseFunction false для {number}: {f2 number}")*)

    (*printfn "Тестирование обхода числа:"
    Console.WriteLine($"Сумма цифр: {main7 number (fun x y -> x + y) 0}")
    Console.WriteLine($"Произведение цифр: {main7 number (fun x y -> x * y) 1}")
    Console.WriteLine($"Максимум цифр: {main7 number (fun x y -> match x > y with | true -> x | false -> y) 0}")
    Console.WriteLine($"Минимум цифр: {main7 number (fun x y -> match x < y with | true -> x | false -> y) 9}")

    printf "Тестирование обхода числа с условием:\n"
    Console.WriteLine($"Количество чётных цифр: {main9 number (fun x y -> x + 1) 0 (fun d -> d % 2 = 0)}")
    Console.WriteLine($"Сумма чётных цифр: {main9 number (fun x y -> x + y) 0 (fun d -> d % 2 = 0)}")
    Console.WriteLine($"Произведение нечётных цифр: {main9 number (fun x y -> x * y) 1 (fun d -> d % 2 = 1)}")
*)
    printfn "\nЛюбимый язык программирования"
    printf "Какой у тебя любимый язык программирования? "
    let userLang = Console.ReadLine()
    let reply = favoriteLangReply userLang
    Console.WriteLine(reply)

    0

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
    | "f#" | "prolog" -> "Хм, понятно... подлизываешься :)"
    | "python" -> "Ну кто бы сомневался."
    | "c++" -> "Интересный выбор."
    | "java" -> "Хороший выбор, одобряю."
    | "ruby" -> "Кажется ты перепутал дисциплины."
    | _ -> "Интересный выбор! Главное, чтобы тебе нравилось."

let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

let main13 n f init =
    let rec loop i acc =
        match i with
        | 0 -> acc
        | _ when gcd i n = 1 -> loop (i - 1) (f acc i)
        | _ -> loop (i - 1) acc
    loop (n - 1) init

//Функция Эйлера
let eulerPhi n =
    match n with
    | 1 -> 1
    | _ -> main13 n (fun acc _ -> acc + 1) 0



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
    (*printfn "\nЛюбимый язык программирования"
    printf "Какой у тебя любимый язык программирования? "

    //Суперпозиция
    let userLang = Console.ReadLine >> favoriteLangReply >> Console.WriteLine
    userLang()

    //Каррирование
    let printReply =
        fun input -> Console.WriteLine(favoriteLangReply input)

    let lang = Console.ReadLine()
    printReply lang
*)

    printfn "Тест функции Эйлера"

    // φ(10) = 4 (взаимно простые: 1, 3, 7, 9)
    printfn $"phi(10) = {eulerPhi 10}"

    // φ(13) = 12 (простое число, значит φ(n) = n - 1)
    printfn $"phi(13) = {eulerPhi 13}"

    // φ(1) = 1 по определению
    printfn $"phi(1) = {eulerPhi 1}"

    0

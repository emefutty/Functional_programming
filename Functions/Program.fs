open System
open ListTasks

// Универсальный обход цифр числа
let digitFold cond op init n =
    let rec loop x acc =
        match x with
        | 0 -> acc
        | _ ->
            let d = x % 10
            let acc' = 
                match cond d with
                | true -> op acc d
                | false -> acc
            loop (x / 10) acc'
    loop n init

let rec digitalSum num : int =
     match num with
     | 0 -> 0
     | _ -> (num % 10) + (digitalSum (num / 10))

let tailDigitalSum num : int =
     let rec digitalSubSum num currentSum = 
          match num with
          | 0 -> currentSum
          | _ ->
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
    digitFold (fun _ -> true) funct init digit

let main9 digit funct init cond =
    digitFold cond funct init digit

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

let main15 n f init cond =
    let rec loop i acc =
        match i with
        | 0 -> acc
        | _ when gcd i n = 1 && cond i -> loop (i - 1) (f acc i)
        | _ -> loop (i - 1) acc
    loop (n - 1) init

//количество чисел, взаимно простых с заданным
let countCoprimeWith n = eulerPhi n

//сумма цифр, делящихся на 3
let sumDigitsDivBy3 n =
    digitFold (fun d -> d % 3 = 0) (+) 0 n

//делитель, взаимно простой с наибольшим числом цифр
let maxCoprimeDivisor n =
    let digits =
        let rec extractDigits x acc =
            match x with
            | 0 when acc = [] -> [0]  
            | 0 -> acc
            | _ -> extractDigits (x / 10) ((x % 10) :: acc)
        extractDigits n []

    // Функция для подсчёта количества взаимно простых цифр с числом d
    let countCoprimeDigits d =
        digits
        |> List.filter (fun digit -> gcd d digit = 1)
        |> List.length

    // Перебор всех делителей числа n
    let rec loop i currentMax currentDiv =
        match i with
        | 0 -> currentDiv
        | _ when n % i = 0 ->
            match countCoprimeDigits i > currentMax with
            | true -> loop (i - 1) (countCoprimeDigits i) i
            | false -> loop (i - 1) currentMax currentDiv
        | _ -> loop (i - 1) currentMax currentDiv

    loop n 0 1

let chooseMethod n =
    match n with
    | 1 -> countCoprimeWith
    | 2 -> sumDigitsDivBy3
    | 3 -> maxCoprimeDivisor
    | _ -> fun _ -> failwith "Неверный номер функции (должен быть 1–3)"


[<EntryPoint>]
let main argv =

    (*printfn "Введите целое число:"
    let number = Console.ReadLine() |> int
*)
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

    (*printfn "Тест функции Эйлера"

    // φ(10) = 4 (взаимно простые: 1, 3, 7, 9)
    printfn $"phi(10) = {eulerPhi 10}"

    // φ(13) = 12 (простое число, значит φ(n) = n - 1)
    printfn $"phi(13) = {eulerPhi 13}"

    // φ(1) = 1 по определению
    printfn $"phi(1) = {eulerPhi 1}"*)

    (*printfn "\nОбход взаимно простых с условием"

    let test1 = main15 20 (fun acc x -> acc + x) 0 (fun x -> x % 2 = 0)
    printfn $"Сумма чётных взаимно простых с 20: {test1}"

    let test2 = main15 15 (fun acc x -> acc * x) 1 (fun x -> x % 2 = 1)
    printfn $"Произведение нечётных взаимно простых с 15: {test2}"*)

    (*printfn $"countCoprimeWith(10) = {countCoprimeWith 10}"*)
    (*printfn $"sumDigitsDivBy3(123456) = {sumDigitsDivBy3 123456}"*)
    (*printfn $"maxCoprimeDivisor(231) = {maxCoprimeDivisor 231}"*)

    (*printfn "Введите номер функции и аргумент, например: 2 231"
    let input = Console.ReadLine().Split()
    let funcNumber = int input[0]
    let arg = int input[1]*)

    (*let apply = fun f -> fun x -> f x

    let selectedFunc = chooseMethod funcNumber
    let result = apply selectedFunc arg*)

   (* let result = (chooseMethod >> (fun f -> f arg)) funcNumber

    printfn $"Результат: {result}"*)

   (* Console.WriteLine("Индекс минимального элемента")
    let list = [1; 2; 8; -64; 4; 16; 9]
    let minList = findMinIndexList list
    let minChurch = findMinIndexChurch list
    Console.WriteLine("Metod 1: {0}", minList)
    Console.WriteLine("Metod 2: {0}", minChurch)*)

    (*Console.WriteLine("Reverse между максимальным и минимальным элементом")
    let list = [1; -2; 8; 3; 4; 16; 64]
    Console.WriteLine("Metod 1: {0}", reverseBetweenMinMaxList list)
    Console.WriteLine("Metod 2: {0}", reverseBetweenMinMaxChurch list)*)

    (*Console.WriteLine("количество минимальных элементов в интервале")
    let list = [1; -2; -2; 3; 4; 16; 64]
    Console.WriteLine("Metod 1: {0}", countMinInRangeList list 1 5)
    Console.WriteLine("Metod 2: {0}", countMinInRangeChurch list 1 5)*)

    (*Console.WriteLine("Количество локальных максимумов")
    let list = [1; 3; 2; 5; 4]
    Console.WriteLine("Metod 1: {0}", countLocalMaxList list)
    Console.WriteLine("Metod 2: {0}", countLocalMaxChurch list)*)

   (* Console.WriteLine("элементы меньше среднего арифметического массива.")
    let list = [1; -2; 8; 3; 4; 16; 64]
    Console.WriteLine("Metod 1: {0}", belowAverageList list)
    Console.WriteLine("Metod 2: {0}", belowAverageChurch list)*)

    (*Console.WriteLine("Список простых делителей")
    Console.WriteLine("Metod 1: {0}", primeFactorsList 36)
    Console.WriteLine("Metod 2: {0}", primeFactorsChurch 36)
*)

    (*let input = [2; 3; 4; 6; 9; 12; 15; 18]

    let (list1, list2, list3, list4, list5) = fiveLists input

    printfn $"Исходный список: {input}"
    printfn $"list1 (чётные / 2): {list1}"
    printfn $"list2 (делятся на 3 / 3): {list2}"
    printfn $"list3 (квадраты list2): {list3}"
    printfn $"list4 (элементы из list3, содержащиеся в list1): {list4}"
    printfn $"list5 (объединение list2 + list3 + list4, без повторений): {list5}"
*)

    (*let a = [|1; 2; 3|]
    let b = [|10; 20; 30|]

    let result = appendLastElement a b
    printfn $"Результат: [|{String.Join("; ", result)}|]"*)

    let langFreq =
        [ 'e', 0.127; 't', 0.091; 'a', 0.082; 'o', 0.075;
         'i', 0.070; 'n', 0.067; 's', 0.063; 'h', 0.061;
         'r', 0.060; 'd', 0.043 ]
        |> Map.ofList

    let result = sortByMostFreqCharDeviation "this is a test string" langFreq
    printfn $"Результат: {result}"

    0

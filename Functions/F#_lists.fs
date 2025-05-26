module ListTasks
open System

 
// 1.2.1
let findMinIndexList list =
    list 
    |> List.mapi (fun i x -> (x, i)) 
    |> List.minBy fst 
    |> snd
 
 // 1.2.2
let findMinIndexChurch list =
    let rec findMin idx minVal minIdx list = 
        match list with
        | [] -> minIdx
        | head::tail -> 
            if head < minVal then findMin (idx+1) head idx tail
            else findMin (idx+1) minVal minIdx tail
    findMin 0 System.Int32.MaxValue (-1) list

// 1.12.1
let reverseBetweenMinMaxList list =
    let minIdx = findMinIndexList list
    let maxIdx = list |> List.mapi (fun i x -> (x, i)) |> List.maxBy fst |> snd
    let startIdx, endIdx = min minIdx maxIdx, max minIdx maxIdx
    let before = list |> List.take (startIdx + 1)
    let middle = list |> List.skip (startIdx + 1) |> List.take (endIdx - startIdx - 1) |> List.rev
    let after = list |> List.skip (endIdx)
    before @ middle @ after
 
 // 1.12.2
let reverseBetweenMinMaxChurch list =
    let minIdx = findMinIndexList list
    let maxIdx = list |> List.mapi (fun i x -> (x, i)) |> List.maxBy fst |> snd
    let start, end_ix = min minIdx maxIdx, max minIdx maxIdx
     
    let rec processList index remaining res =
        match remaining with
        | [] -> List.rev res
        | head::tail ->
            if index > start && index < end_ix then
                let mirroredPos = end_ix - (index - start)
                let mirroredValue = list.[mirroredPos]
                processList (index+1) tail (mirroredValue::res)
            else
                 processList (index+1) tail (head::res)
     
    processList 0 list []

// 1.22.1
let countMinInRangeList list a b =
    let sublist = list |> List.skip a |> List.take (b - a + 1)
    let minVal = List.min sublist
    sublist |> List.filter (fun x -> x = minVal) |> List.length
 
 // 1.22.2
let countMinInRangeChurch lst a b =
    let rec takeRange start stop acc list = 
        match list with
        | [] -> acc
        | head::tail ->
            if start > stop then acc
            elif start > 0 then takeRange (start - 1) (stop - 1) acc tail
            else takeRange start (stop - 1) (head :: acc) tail
     
    let sublist = takeRange a b [] lst |> List.rev

    let rec findMin list minVal =
        match list with
        | [] -> minVal
        | h::t ->
            if h < minVal then findMin t h
            else findMin t minVal

    let minVal =
        match sublist with
        | [] -> failwith "Список пуст — нечего сравнивать"
        | h::t -> findMin t h
  
    let rec countMin cnt list = 
        match list with
        | [] -> cnt
        | head::tail ->
            if head = minVal then countMin (cnt + 1) tail
            else countMin cnt tail
     
    countMin 0 sublist

    // 1.32.1
let countLocalMaxList list =
    list
    |> List.windowed 3
    |> List.filter (function 
        | [a; b; c] -> b > a && b > c
        | _ -> false)
    |> List.length
 
 // 1.32.2
let countLocalMaxChurch list =
    let rec count acc list = 
        match list with
        | a::b::c::tail when b > a && b > c -> count (acc + 1) (b::c::tail)
        | _::tail -> count acc tail
        | _ -> acc
    count 0 list

// 1.42.1
let belowAverageList list =
    let avg = List.averageBy float list
    list |> List.filter (fun x -> float x < avg)
 
 // 1.42.2
let belowAverageChurch list =
    let average list =
        let rec calculate sum count = function
            | [] -> 
                if count = 0 then 0.0
                else float sum / float count
            | head::tail -> calculate (sum + head) (count + 1) tail
        calculate 0 0 list
 
    let avg = average list
    let rec filterBelow acc list = 
        match list with
        | [] -> acc
        | head::tail ->
            if float head < avg then
                filterBelow (head::acc) tail
            else
                filterBelow acc tail
    filterBelow [] list |> List.rev

// 1.52.1
let primeFactorsList n =
    let rec factorize n p acc =
        match n with
        | 1 -> acc
        | _ when n % p = 0 -> factorize (n/p) p (p::acc)
        | _ -> factorize n (p + 1) acc
    match n with
    | x when x < 2 -> []
    | _ -> factorize n 2 [] |> List.rev
 
 // 1.52.2
let primeFactorsChurch n =
    let rec reverseList acc = function
        | [] -> acc
        | head::tail -> reverseList (head::acc) tail
 
    let rec factorize n p acc =
        match n with
        | 1 -> acc
        | _ when n % p = 0 -> factorize (n/p) p (p::acc)
        | _ -> factorize n (p + 1) acc
 
    match n with
    | x when x < 2 -> []
    | _ -> 
        let factors = factorize n 2 []
        reverseList [] factors 

// 17.2. Кортеж из пяти списков
let fiveLists (list: int list) =
    let list1 = list |> List.filter (fun x -> x % 2 = 0) |> List.map (fun x -> x / 2)
    let list2 = list1 |> List.filter (fun x -> x % 3 = 0) |> List.map (fun x -> x / 3)
    let list3 = list2 |> List.map (fun x -> x * x)
    let list4 = list3 |> List.filter (fun x -> List.contains x list1)
    let list5 = (list2 @ list3 @ list4) |> List.distinct
    (list1, list2, list3, list4, list5)

// 18.2 Копирование последнего элемента из B в A
let appendLastElement (a: 'a[]) (b: 'a[]) =
    match b with
    | [||] -> a
    | _ -> Array.append a [|b.[b.Length - 1]|]

// 19.2 Упорядоченные строчные символы (по возрастанию)
let areLettersOrdered str =
     let lowercaseLetters = 
         str 
         |> Seq.filter System.Char.IsLower
         |> Seq.toList
     
     let rec isOrdered list = 
         match list with
         | [] | [_] -> true
         | x::y::rest -> x <= y && isOrdered (y::rest)
     
     isOrdered lowercaseLetters

//В порядке увеличения разницы между частотой наиболее часто встречаемого символа в строке и частотой его появления в алфавите
let sortByMostFreqCharDeviation (input: string) (langFreq: Map<char, float>) =
    // Частота каждого символа в строке
    let inputFreq =
        input
        |> Seq.filter Char.IsLetter
        |> Seq.countBy id
        |> Map.ofSeq

    // Самый частый символ в строке
    let mostFreqChar =
        inputFreq
        |> Map.toSeq
        |> Seq.maxBy snd
        |> fst

    // Частота этого символа в языке
    let langFreqValue =
        langFreq
        |> Map.tryFind mostFreqChar
        |> Option.defaultValue 0.0

    // Частота этого символа в строке (в долях)
    let totalLetters = input |> Seq.filter Char.IsLetter |> Seq.length |> float
    let inputFreqValue =
        inputFreq
        |> Map.tryFind mostFreqChar
        |> Option.defaultValue 0
        |> float
        |> fun cnt -> cnt / totalLetters

    // Абсолютная разница
    let deviation = abs (inputFreqValue - langFreqValue)

    // Сортируем символы строки по "похожести" на этот deviation
    input
    |> Seq.sortBy (fun c ->
        let freqC =
            Map.tryFind c inputFreq
            |> Option.map (fun cnt -> float cnt / totalLetters)
            |> Option.defaultValue 0.0

        abs (freqC - langFreqValue))
    |> String.Concat

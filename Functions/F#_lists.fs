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
        | [] -> failwith "—писок пуст Ч нечего сравнивать"
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

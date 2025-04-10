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


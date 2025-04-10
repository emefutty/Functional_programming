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


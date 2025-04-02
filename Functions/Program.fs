open System

let pi = 3.14159

let areaOfCircle r = pi * r * r
let volumeOfCylinder r h = areaOfCircle r * h

[<EntryPoint>]
let main argv =

    let readFloat = Console.ReadLine >> float

    printfn "Введите радиус круга:"
    let r = readFloat()

    printfn "Введите высоту цилиндра:"
    let h = readFloat()

    r |> areaOfCircle |> printfn "Площадь круга: %.2f"
    r |> (fun r -> volumeOfCylinder r h) |> printfn "Объем цилиндра: %.2f"

    0

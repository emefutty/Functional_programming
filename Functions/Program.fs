open System

let pi = 3.14159

let areaOfCircle = fun r -> pi * r * r
let volumeOfCylinder = fun r -> fun h -> (areaOfCircle r) * h

[<EntryPoint>]
let main argv =

    let readFloat = Console.ReadLine >> float

    printfn "Введите радиус круга:"
    let r = readFloat()

    printfn "Введите высоту цилиндра:"
    let h = readFloat()

    let area = areaOfCircle r
    let volume = (volumeOfCylinder r) h

    printfn "Площадь круга: %.2f" area
    printfn "Объем цилиндра: %.2f" volume

    0

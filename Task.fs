namespace Projekt


type Task(name:string,startingTime:float,endingTime:float) =
    let mutable _name = name
    let mutable _startingTime = startingTime
    let mutable _endingTime = endingTime

    member this.name
        with get() = _name
        and set(value) = _name <- value
    member this.startingTime 
        with get() = _startingTime
        and set(value) = _startingTime <- value

    member this.endingTime
        with get() = _endingTime
        and set(value) = _endingTime <- value
    

    member this.CalculateLength() =
        let FullTime = _endingTime-_startingTime
        FullTime

     
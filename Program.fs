open Projekt

let optimiseHarmonogram (works: Task list) =
   
    let workSorted = works |> List.sortBy (fun (t: Task) -> t.startingTime)
    let rec choose previous chosen lista:Task list =
        match lista with
        | [] -> List.rev chosen
        | work:Task :: rest ->
            if work.startingTime >= previous then
                choose work.endingTime (work :: chosen) rest
            else
                choose previous chosen rest
    choose 0 [] workSorted

let allCombinations lst =
    let rec comb accLst elemLst =
        match elemLst with
        | h::t ->
            let next = [h]::List.map (fun el -> h::el) accLst @ accLst
            comb next t
        | _ -> accLst
    comb [] lst

let rec filterAllCombinations acclst lst =
    match lst with
    | h::t -> 
        let uselist = optimiseHarmonogram lst.Head :: acclst
        filterAllCombinations uselist lst.Tail
    | [] -> acclst
let rec getListWithSaidLength lst len =
    match lst with
    | h::t ->
        if List.length lst.Head = len then lst.Head
        else getListWithSaidLength lst.Tail len
    | [] -> lst.Head

let pickBestList lst =
    let mutable lengt = 0
    for i in lst do
        if List.length i > lengt then lengt <- i.Length
        else lengt <- lengt
    getListWithSaidLength lst lengt


[<EntryPoint>]
let main argv =
    let Task1 = Task("Task1",5.6,6.7)
    let Task2 = Task("Task2",5.0,7.0)
    let Task3 = Task("Task3",9.0,9.9)
    let Task4 = Task("Task4",1.3,2.3)
    let Task5 = Task("Task5",4.3,4.7)
    let Task6 = Task("Task6",6.9,8.9)
    let Task7 = Task("Task7",3.0,5.0)
    let TaskList = [Task1;Task2;Task3;Task4;Task5;Task6;Task7]
    let TaskArray = [|Task1;Task2;Task3;Task4;Task5;Task6;Task7|]
    let test = optimiseHarmonogram TaskList
    printfn "Metoda zachłanna:"
    test |> List.iter (fun zad -> 
        printfn "Start: %0.2f, Koniec: %0.2f" zad.startingTime zad.endingTime)
    let acclist5:Task list list = []
    let list25 = allCombinations TaskList
    let list30 = filterAllCombinations acclist5 list25
    let list35 = pickBestList list30
    printfn "Metoda wyczerpująca:"
    list35 |> List.iter (fun zad -> 
        printfn "Start: %0.2f, Koniec: %0.2f" zad.startingTime zad.endingTime)
    0
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    Console.State.stateMachine System.Console.ReadLine <| printfn "%s"
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

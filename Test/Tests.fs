namespace Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Console.State

type private ExptectedString =
    | Any
    | S of string

[<TestClass>]
type UnitTest() =
    let createExpectationWriter list =
        let mutable current = list
        fun written ->
            match current with
            | [] -> ()
            | c ->
                let head = List.head c
                current <- List.tail c
                match head with
                | Any -> ()
                | S s -> 
                    Assert.AreEqual(s, written)
            
    let createReader list =
        let mutable current = list
        fun () ->
            let head = List.head current
            current <- List.tail current
            head

    [<TestMethod>]
    member x.``initially asks for shape`` () =
        let writer = createExpectationWriter [S "(C)ircle or (R)ectangle?"]
        let reader = createReader ["C"; "1"]
        stateMachine reader writer

    [<TestMethod>]
    member x.``notifies user if input for shape was invalid`` () =
        let writer = createExpectationWriter [Any; S "invalid shape"]
        let reader = createReader ["asdasd"; "C"; "1"]
        stateMachine reader writer

    [<TestMethod>]
    member x.``asks for shape again if input for shape was invalid`` () =
        let writer = createExpectationWriter [Any; Any; S "(C)ircle or (R)ectangle?"]
        let reader = createReader ["asdasd"; "C"; "1"]
        stateMachine reader writer

    [<TestMethod>]
    member x.``asks for circle area after user answered shape with "C"`` () =
        let writer = createExpectationWriter [Any; S "Circle radius?"]
        let reader = createReader ["C"; "1"]
        stateMachine reader writer
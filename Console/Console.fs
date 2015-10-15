module Console.State

type State =
    | AskForShape
    | AskForCircleRadius
    | AskForRectangleSideA
    | AskForRectangleSideB of int64
    | PrintAnswer of float

let computeCircleArea radius = System.Math.Pow(float radius, 2.0) * System.Math.PI
let computeRectangleArea a b = float <| a * b

let createWrite writer format = 
    Printf.kprintf writer format

let stateMachine read writer =
    let rec loop state =
        let write = createWrite writer
        match state with
        | AskForShape ->
            write "(C)ircle or (R)ectangle?"
            match read() with
            | "C" -> loop AskForCircleRadius
            | "R" -> loop AskForRectangleSideA
            | _ -> write "invalid shape"; loop state
        | AskForCircleRadius ->
            write "Circle radius?"
            match System.Int64.TryParse <| read() with
            | (true, r) when r >= 0L -> loop <| PrintAnswer (computeCircleArea r)
            | _ -> write "invalid radius"; loop state
        | AskForRectangleSideA ->
            write "Rectangle side A length?"
            match System.Int64.TryParse <| read() with
            | (true, a) when a >= 0L -> loop <| AskForRectangleSideB a
            | _ -> write "invalid length"; loop state
        | AskForRectangleSideB a ->
            write "Rectangle side B length?"
            match System.Int64.TryParse <| read() with
            | (true, b) when b >= 0L -> loop <| PrintAnswer (computeRectangleArea a b)
            | _ -> write "invalid length"; loop state
        | PrintAnswer area ->
            createWrite writer "The area of your shape is: %f" area
    loop AskForShape

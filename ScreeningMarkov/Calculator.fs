namespace Kata
module Calculator =
    exception Empty
    exception Error

    type OperatorKey =
    | Plus
    | Minus
    | Times
    | Divide

    type key = 
    | NotKey
    | AC
    | Number of int
    | Equals 
    | OperatorKey of OperatorKey

    type EnterNumber = int list

    let formatNumber (num:EnterNumber) =
        num
            |> List.fold (fun acc i -> acc + i.ToString()) ""

    let addDigit digit (num:EnterNumber) =
        match digit,num,num.Length < 10 with 
        | 0,[0],_    -> [0]           //No leading zeros can be entered
        | n,[0],_    -> [n]           //Entering a digit removes the zero
        | _,_,false  -> num           //No more than 10 digits
        | _,_,_      -> num @ [digit]

    let float (num:EnterNumber) =
        0
 
    type state = 
    | EnterNumber of EnterNumber
    | Operator of OperatorKey
    | Result of float

    let StartEnterNumber = EnterNumber [0]

    type Calculator = Nil | Cons of (state * Calculator)
    let calculator =  Nil 

    let head calculator = 
        match calculator with
        | Nil -> raise Empty
        | Cons(x,_) -> x

    let tail calculator = 
        match calculator with
        | Nil -> raise Empty
        | Cons(_,x) -> x


    let calculate calculator = 
        
        let opFor operation = 
            match operation with
            | Plus   -> (+)
            | Divide -> (/)
            | Times  -> (*)
            | Minus  -> (-)
            
        match calculator with
        | Cons(EnterNumber rhs, Cons(Operator op,Cons(EnterNumber lhs,tail))) -> Nil
        | _ -> Cons(StartEnterNumber,calculator)

    let press key calculator = 
        match key,calculator with
        | AC,Nil                                -> Cons(StartEnterNumber,calculator)
        | Number n, Cons(EnterNumber n2,_)      -> Cons(EnterNumber (addDigit n n2),(tail calculator))
        | Equals  , Cons(_,_)                   -> calculate calculator 
        | OperatorKey n, Cons(EnterNumber _,_)  -> Cons(StartEnterNumber,Cons(Operator n,(calculator)))
        | _,_                                   -> calculator
               
    let presskeys (keys:key list) calculator = 
        keys 
          |> List.fold (fun acc k -> press k acc) calculator

    let display calculator = 
        match calculator with
        | Nil                     -> ""
        | Cons((EnterNumber n),_) -> sprintf "%s." (formatNumber n)
        

open Calculator
open Xunit
open Xunit.Extensions
open Swensen.Unquote
    


module ``Entering integers``=
    let oncalculator = calculator |> press AC
    let numbers numbers = numbers |> List.map Number 

    [<Fact>]
    let ``Turn on the calculator`` () =
        test <@ (calculator |> press AC) |> display = "0." @> 

    [<Fact>]
    let ``Pressing a single digit displays the digit`` ()=
        test <@ oncalculator
            |> press (Number 1) 
            |> display = "1." @>

    [<Fact>]
    let ``Pressing two digits displays both`` ()=
        test <@ oncalculator 
            |> press (Number 1) 
            |> press (Number 2) 
            |> display = "12." @>

    [<Fact>]
    let ``Pressing 1 to 9 displays all entered digits`` ()=
        test <@  oncalculator 
            |> presskeys (numbers ([1..9] @ [0])) 
            |> display = "1234567890." @>

    [<Fact>]
    let ``The display has a 10 digit limit`` ()=
        test <@  oncalculator 
            |> presskeys (numbers ([1..9] @ [0;1])) 
            |> display = "1234567890." @>

    [<Fact>]
    let ``Pressing equals after a number starts entering a new number`` ()= 
        test <@ oncalculator 
                |> presskeys (numbers [1..3])
                |> press Equals
                |> press (Number 1)
                |> display = "1." @>

    let operators = [Equals;OperatorKey Plus;OperatorKey Minus;OperatorKey Times;OperatorKey Divide] |> List.toSeq
    [<Theory>]
    [<PropertyData("operators")>]
    let ``Pressing an operator or equals after a number starts entering a new number`` (operator)= 
        test <@ oncalculator 
                |> presskeys (numbers [1..3])
                |> press operator
                |> press (Number 1)
                |> display = "1." @>

    [<Fact>]
    let ``Cannot enter leading zeros`` ()=
        test <@ oncalculator 
                    |> presskeys (numbers [0;0;0])
                    |> display = "0." @>
                    

module ``Integer math`` = 
    let oncalculator = calculator |> press AC
    let numbers numbers = numbers |> List.map Number 

    [<Fact>] 
    let ``Basic addition`` ()=
        test <@ oncalculator
                |> presskeys (numbers [1;2;3])
                |> press     (OperatorKey Plus)
                |> presskeys (numbers [4;5;6])
                |> press     (Equals)
                |> display = "579." @>
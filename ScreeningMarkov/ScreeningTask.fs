namespace Screening


module Parser = 
    type symbolMatch<'a> = 
        | NoCapture 
        | Match of ('a * int * int)

    type PEG<'a> = {position:int;input:string;symbol:symbolMatch<'a>;parser:PEG<'a> option}
    type parseResult<'a> = (bool * PEG<'a>)
    type rule<'a> = PEG<'a> -> parseResult<'a>

    let parser<'a> input = {position=0;input=input;symbol=NoCapture;parser=None}

    let cur<'a> parser =
        parser.input.Substring parser.position 

    let consume<'a> (symbol:'a) length parser =
        {position = parser.position + length
         input    = parser.input
         symbol   = Match (symbol,parser.position,length)
         parser   = Some(parser)
         } 

    let skip<'a> length parser =
        {position = parser.position + length
         input    = parser.input
         symbol   = NoCapture
         parser   = Some(parser)
         } 

    let recChar<'a> char (parser:PEG<'a>) = 
        match cur parser with
        | s when s.Length = 0 -> (false,parser)
        | s when s.[0] = char -> (true,skip 1 parser)
        | _ -> (false,parser)
                     
    let zeroOrMore<'a> (rule:rule<'a>) (parser:PEG<'a>) = 
        (true,parser)

    let rec combine<'a> (rules:rule<'a> list) (parser:PEG<'a>) = 
        match parser |> rules.Head,rules.Tail with
        | (true,parser),t when t.IsEmpty -> (true,parser)
        | (true,parser), head::tail -> combine (head::tail) parser
        | (false,parser),_ -> (false,parser)
    
    let capture<'a> (symbol:'a) (rule:rule<'a>) (parser:PEG<'a>) =
        match rule parser with
        | (true,nextparser)  -> (true,consume symbol parser.position nextparser)
        | (false,parser) -> (false,parser)
      
    let rec matches (parser:PEG<'a>) = 
        seq {
            match parser.symbol with
            | Match (sym,length,start) -> yield (sym,parser.input.Substring(start,length))
            | _ -> ()

            match parser.parser with
            | Some(parser) -> yield! matches parser
            | _ -> ()

        } |> Seq.toList
             

module Scheduler = 
    open Parser

    type taskSymbols = 
        | DEPENDENCY
        | TASKNAME
        | TASK
        | TASKS

    let recWs = zeroOrMore (combine [recChar '\r';recChar ' ';recChar '\n'])
    let recDependency = capture DEPENDENCY (combine [recChar '=';recChar '>'])
    let recTaskName = capture TASKNAME (combine [recChar 'a';recChar 'b'])
    let recTask = capture TASK (combine [recWs;recTaskName;recWs;recDependency;recWs;recTaskName;recWs])
    let recTasks = capture TASKS (zeroOrMore recTask)

    type Task = Nil | Cons of (string * Task)

    let executionOrder tasks = 
        []

    let taskNames tasks = 
        []

module ``Task scheduling specifications`` =
    open Xunit
    open Swensen.Unquote
    open Scheduler

    [<Fact>]
    let ``Order of empty tasks is empty`` ()=
        test <@ executionOrder "" = [] @>

    [<Fact>]
    let ``Order of a => b is [a b]`` ()=
        test <@ executionOrder "a => b"
                |> taskNames = ["a";"b"] @>
    

module ``Parser specification`` =
    open Xunit
    open Swensen.Unquote
    open Parser
    open Scheduler

    [<Fact>]
    let ``Match dependency`` ()=
        test<@ parser "=>"
           |> recDependency |> fst = true @>

    [<Fact>]
    let ``Fail to match string`` ()=
        test<@ parser "="
           |> recDependency |> fst = false @>


    [<Fact>]
    let ``Capture string`` ()=
        test<@ parser "ab"
           |> recTaskName 
           |> snd
           |> matches = [(TASKNAME,"ab")] @>
    
    [<Fact>]
    let ``Match sequence`` ()=
        let parseAB = combine [recTaskName;recDependency]
        test<@ parser "ab=>"
                |> parseAB
                |> fst = true @>
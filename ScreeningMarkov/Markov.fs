module ScreeningMarkup
    open Xunit
    open Swensen.Unquote

    type symbol<'a when 'a:comparison> = 
        | Start
        | End
        | Symbol of 'a

    type Model<'a when 'a:comparison> = Map<symbol<'a>,Map<symbol<'a>,float>>

    let DefaultModel = Map.empty

    type Distribution<'a when 'a:comparison> = Map<symbol<'a>,float>
    let distribution<'a when 'a:comparison> symbol (model:Model<'a>) = 
        (match Map.tryFind symbol model with
        | Some(frequencies) -> 
            let k =  1.0 / (frequencies |> Map.toList |> List.sumBy snd)
            frequencies |> Map.toList |> List.map (fun (s,f) -> (s,k*f)) |> Map.ofList
        | None -> Map.empty)

    let select r (distribution:Distribution<'a>) =
        let rec find cur remaining =
            match remaining with
            | (s,f)::tail ->
                if (r >= (cur + f)) then s
                else find (cur + f) remaining.Tail
            | _ -> End
        find 0.0 (distribution |> Map.toList |> List.sortBy fst)

    let defaults symbols =
        [Start] @ symbols @ [End]

    let NoSymbols<'a> = defaults []
    let Symbols list = defaults (list |> List.map Symbol)

    let updateProbabilties firstsymbol secondsymbol (model:Model<'a>) = 
        match Map.tryFind firstsymbol model with
        | Some(frequencies) -> 
            match Map.tryFind secondsymbol frequencies with
            | Some(f) -> model.Add (firstsymbol,frequencies.Add(secondsymbol,f + 1.0))
            | None -> model.Add (firstsymbol,frequencies.Add(secondsymbol,1.0))
        | None -> model.Add (firstsymbol,[(secondsymbol,1.0)] |> Map.ofList) 
    
    let rec train model symbols = 
        match symbols with
        | head::next::_ -> train model symbols.Tail |> updateProbabilties head next
        | _ -> model


    let rec enumerate<'a when 'a: comparison> (random:float seq) (model:Model<'a>)  = 
        let sym = ref Start
    
        random 
            |> Seq.map (fun r -> 
                sym := select r (distribution sym.Value model)
                sym.Value)
            |> Seq.takeWhile ((<>) End)

    [<Fact>]
    let ``When we have no symbols the End symbol always follows the Start symbol`` ()=
        test <@ train DefaultModel NoSymbols
                |> Map.find Start |> Map.toList = [(End,1.0)] @>

    [<Fact>]
    let ``When we have a single symbol the probability it follows the start symbol is one`` ()=
        test <@ train DefaultModel (Symbols ["a"])
                |> Map.find Start |> Map.toList = [(Symbol("a"),1.0)] @>

    [<Fact>]
    let ``When we have a single symbol the probability it preceeds the end symbol is one`` ()=
        test <@ train DefaultModel (Symbols ["a"])
                |> Map.find (Symbol "a") |> Map.toList = [(End,1.0)]@>


    [<Fact>]
    let ``When we have symbols a b a c the probability of b or c following a is 1/2`` ()=
        let trained = train DefaultModel (Symbols ["a";"b";"a";"c"])
        test <@ trained  
                |> distribution (Symbol "a")
                |> Map.find (Symbol "b") = 0.5 @>
        test <@ trained  
                |> distribution (Symbol "a")
                |> Map.find (Symbol "c") = 0.5 @>
        test <@ trained  
                |> distribution (Symbol "b")
                |> Map.find (Symbol "a") = 1.0 @>
        test <@ trained  
                |> distribution (Symbol "c")
                |> Map.find End = 1.0 @>

    [<Fact>]
    let ``Enumerating the model of symbols a b a c with 0 1/2 0 produces a b a`` ()=
        let random = [0.0;0.5;0.0] |> List.toSeq

        let trained = train DefaultModel (Symbols ["a";"b";"a";"c"])
        test <@ trained |> enumerate (random) |> List.ofSeq = [Symbol("a");Symbol("b");Symbol("a")] @>

    [<Fact>]
    let ``Enumerating the model of symbols a b a c with 0 1 0 produces a c a`` ()=
        let random = [0.0;1.0;0.0] |> List.toSeq

        let trained = train DefaultModel (Symbols ["a";"b";"a";"c"])
        test <@ trained |> enumerate (random) |> List.ofSeq = [Symbol("a");Symbol("c");Symbol("a")] @>
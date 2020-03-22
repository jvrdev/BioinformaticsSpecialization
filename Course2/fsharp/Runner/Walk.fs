namespace Course2

type Walk<'t> = Walk of list<'t>

module Walk = 
    let empty : Walk<'t> = Walk []
    let singleton (x : 'u) : Walk<'u> = Walk [x]
    let append (vertex : 'a) (Walk xs) =
        Walk (vertex :: xs)
    let ofStartEdge ((src, dst) : Edge<'u>) : Walk<'u> =
        singleton src |> append dst
    let map (f : 'u -> 'v) (Walk x : Walk<'u>) : Walk<'v> = 
        x |> List.map f |> Walk
    // print with -> as separator
    let print (printElem : 'a -> string) (Walk x) =
        x
        |> Seq.rev
        |> Seq.map printElem
        |> String.concat "->"
    /// print without separator
    let print2 (printElem : 'a -> string) (Walk x) =
        x
        |> Seq.rev
        |> Seq.map printElem
        |> System.String.Concat
    let toSeq (Walk x) =
        x |> Seq.rev
    let toArray<'t> : Walk<'t> -> 't[]  = toSeq >> Seq.toArray
    let parse (parseElem : string -> 'a) (s : string) : Walk<'a> =
        s
        |> splitS " -> "
        |> Seq.map parseElem
        |> Seq.toList
        |> Walk
    //let has (vertex : 'a) (Walk xs) =
    //    List.contains vertex xs
    let rotate (n : int) (Walk xs : Walk<'a>) : Walk<'a> =
        let length = List.length xs
        let nPrime = n % length
        Seq.concat [
            (xs |> Seq.skip nPrime)
            (xs |> Seq.take nPrime)
        ]
        |> Seq.toList
        |> Walk
    let dropFirstIfCycle (x : Walk<'a>) : Walk<'a> =
        match x with
        | (Walk (h::t)) when List.last t = h -> Walk t
        | _ -> x
    let appendLast (Walk ws : Walk<'a> as x) : Walk<'a> =
        let last = List.last ws
        append last x
    let lastNode (Walk ns : Walk<'a>) : option<'a> = 
        match ns with
        | [] -> None
        | h::t -> Some h

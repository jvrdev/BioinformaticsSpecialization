namespace Course2

module Euler =
    let rec walkUntilCycle (graph : DirectedGraph<'a>) (walk : Walk<'a>) : Walk<'a> * DirectedGraph<'a> =
        match walk with
        | Walk [] -> invalidArg "walk" "Walk cannot be empty"
        | Walk (last::_) ->
            match DirectedGraph.walkFirstEdge last graph with
            | Some (_, next), graphPrime -> 
                walkUntilCycle graphPrime (Walk.append next walk)
            | None, graphPrime -> walk, graphPrime

    let rec eulerStep (remainingGraph : DirectedGraph<'a>) (progress : Walk<'a>) : Walk<'a> =
        let walkPrime, graphPrime = walkUntilCycle remainingGraph progress
        if DirectedGraph.isEmpty graphPrime
        then walkPrime
        else 
            let (Walk ws as nocycle) = Walk.dropFirstIfCycle walkPrime
            match ws |> Seq.tryFindIndex (fun n -> DirectedGraph.hasEdgesFor n graphPrime) with
            | Some i ->
                let walkPrimePrime = Walk.rotate i nocycle 
                eulerStep graphPrime walkPrimePrime
            | None -> invalidArg "graph" "Graph is not Eulerian"

    let eulerCycle (graph : DirectedGraph<'a>) : Walk<'a> =
        match DirectedGraph.takeFirst graph with
        | Some first -> Walk.appendLast <| eulerStep graph (Walk [first])
        | None -> invalidArg "graph" "Graph is empty"

    let eulerPath (graph : DirectedGraph<'a>) : Walk<'a> =
        let grades = DirectedGraph.grades graph
        let src = 
            grades
            |> Seq.find (fun kvp -> Grade.balance kvp.Value < 0)
            |> (fun x -> x.Key)
        let dst =
            grades
            |> Seq.find (fun kvp -> Grade.balance kvp.Value > 0)
            |> (fun x -> x.Key)
        let graphPrime = DirectedGraph.add (dst, src) graph
        let (Walk ws as walk) = 
            eulerStep graphPrime (Walk [src])
            |> Walk.dropFirstIfCycle
        let i = ws |> Seq.findIndex ((=)dst)
        Walk.rotate i walk

    // not required or finished
    let allEulerianCycles (graph : DirectedGraph<'a>) : seq<Walk<'a>> = 
        let eulerize (g : DirectedGraph<'a>) =
            let grades = DirectedGraph.grades g
            match grades |> Map.tryFindKey (fun _ v -> v.InGrade > 1) with
            | Some v ->
                DirectedGraph.allEdgesTo v g
                |> Seq.collect (fun (u, _) ->
                    DirectedGraph.allEdgesFrom v g
                    |> Seq.collect (fun (_, w) ->
                        let bypass = DirectedGraph.bypass (u, v, w) g
                        if DirectedGraph.isConnected bypass
                        then [bypass]
                        else []
                    )
                )
            | None -> Seq.singleton g
        let graphs = [graph] |> Seq.collect eulerize |> Seq.toList
        graphs |> Seq.map eulerCycle


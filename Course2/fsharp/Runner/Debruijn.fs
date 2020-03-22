namespace Course2

type DeBruijnable<'t, 'c when 'c : comparison> = {
    Comparer : 't -> 'c
    Prefix : 't -> 't
    Suffix : 't -> 't
}

module Debruijn =

    module Instances =
        let mkSeqChar (k : int) =
            {
                Comparer = Seq.toArray >> System.String
                Prefix = Seq.take (k - 1)
                Suffix = Seq.skip 1
            }
        
        let mkString (k : int) : DeBruijnable<string, string>=
            {
                Comparer = id
                Prefix = fun s -> s.Substring (0, k - 1)
                Suffix = fun s -> s.Substring (1, k - 1)
            }
        
        // easy to have it not be just for strings but it does make things easier
        let mkReadPairString (k : int) : DeBruijnable<ReadPair<string>, string> =
            {
                Comparer = fun (a, b) -> a + b
                Prefix = ReadPair.map (fun s -> s.Substring (0, k - 1))
                Suffix = ReadPair.map (fun s -> s.Substring (1, k - 1))
            }

    let run 
        (ops : DeBruijnable<'t, 'c>)
        (kmers : seq<'t>)
        : DirectedGraph<'t> =
        let entries = 
            kmers
            |> Seq.map (fun x -> ops.Prefix x, ops.Suffix x)
            |> Seq.groupBy (fst >> ops.Comparer)
            |> Seq.map (fun (_, dsts) -> dsts |> Seq.head |> fst, dsts |> Seq.map snd |> Seq.toList)
            |> Seq.toList
        AdjacencyList entries
namespace Course2

type Reconstructable<'container, 'element> = {
    ToSeq : 'container -> seq<'element>
    Last : 'container -> 'element
    OfSeq : seq<'element> -> 'container
}

module Reconstruction =
    open Euler

    module Instances =
        let array = {
            ToSeq = Array.toSeq
            Last = Array.last
            OfSeq = Array.ofSeq
        }
        let string : Reconstructable<string, char> = {
            ToSeq = seq
            Last = fun s -> s.[s.Length - 1]
            OfSeq = Seq.toArray >> System.String
        }

    let pathToGenome (walk : Walk<seq<'a>>) : seq<'a> =
        let kmers = Walk.toArray walk
        Seq.append (Seq.head kmers) (Seq.tail kmers |> Seq.collect (Seq.last >> Seq.singleton))

    let stringReconstruction (k : int) (kmers : seq<string>) : string =
        let db = 
            Debruijn.run (Debruijn.Instances.mkString k) kmers 
            |> DirectedGraph.map (Seq.toArray >> System.String)
        let path = eulerPath db |> Walk.map seq
        pathToGenome path |> Seq.toArray |> System.String

    let cycleReconstruction (k : int) (kmers : seq<string>) : string =
        let db = 
            Debruijn.run (Debruijn.Instances.mkString k) kmers 
            |> DirectedGraph.map (Seq.toArray >> System.String)
        let path = eulerCycle db |> Walk.map seq
        pathToGenome path |> Seq.toArray |> System.String |> (fun s -> s.Substring (0, s.Length - k + 1))
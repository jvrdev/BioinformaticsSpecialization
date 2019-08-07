namespace Bio

open System

module Week3 =
    let genomeContainsPatternWithAtMostDMismatches (d : int) (pattern : Genome) (dna : Genome) =
        Genome.Kmers pattern.Length dna
        |> Seq.map (fun kmer -> Week2.hammingDistance kmer pattern)
        |> Seq.exists (fun distance -> distance <= d)

    let motifEnumeration (k : int) (d : int) (dnas : seq<Genome>) : seq<Genome> =
        dnas
        |> Seq.collect (Genome.Kmers k)
        |> Seq.collect (fun x -> Week2.neighbors x d)
        |> Seq.filter (fun pattern -> dnas |> Seq.forall (genomeContainsPatternWithAtMostDMismatches d pattern))
        |> Seq.distinct
        |> Seq.sort

    let greedyMotifSearch (k : int) (t : int) (dnas : seq<Genome>) : seq<Genome> =
        let bestMotifs0 = dnas |> Seq.map (Genome.Slice 0 k) |> Seq.toArray
        let kmers = dnas |> Seq.head |> Genome.Kmers k
        let folder bestMotifs motif =
            let motifi i = []
            let motifs = Array.init t motifi
            if motifScore motifs < motifScore bestMotifs
            then motifs
            else bestMotifs
        kmers |> Seq.fold folder bestMotifs0 

    let runMotifEnumeration f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let d = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.ofString
        let output = motifEnumeration k d dnas
        output |> Seq.map string |> format

    let runGreedyMotifSearch f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let t = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.ofString
        let output = greedyMotifSearch k t dnas
        output |> Seq.map string |> format
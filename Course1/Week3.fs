namespace Bio

open System

module Week3 =
    let genomeContainsPatternWithAtMostDMismatches (d : int) (pattern : Genome) (dna : Genome) =
        Genome.Kmers pattern.Length dna
        |> Seq.map (fun kmer -> hammingDistance kmer pattern)
        |> Seq.exists (fun distance -> distance <= d)

    let motifEnumeration (k : int) (d : int) (dnas : seq<Genome>) : seq<Genome> =
        dnas
        |> Seq.collect (Genome.Kmers k)
        |> Seq.collect (fun x -> Week2.neighbors x d)
        |> Seq.filter (fun pattern -> dnas |> Seq.forall (genomeContainsPatternWithAtMostDMismatches d pattern))
        |> Seq.distinct
        |> Seq.sort

    let entropy =
        let entropyLog = function 
            | 0.0 -> 0.0
            | x -> Math.Log (x, 2.0)
        fun x -> Seq.sumBy (fun x -> x * entropyLog x) x * -1.0

    let motifEntropy (m : MotifMatrix) : float =
        let count = motifCount m
        let profile = motifProfile count
        seq { 
            for j = 0 to profile.GetLength 1 - 1 do
                yield seq { for i = 0 to profile.GetLength 0 - 1 do yield profile.[i, j] } |> entropy
        }
        |> Seq.sum

    let medianString (k : int) (gs : seq<Genome>) : Genome =
        let m = MotifMatrix.OfRowSeq gs
        Genome.Enumerate k
        |> Seq.minBy (fun pattern -> motifDistance pattern m)

    let greedyMotifSearchWithCount count (k : int) (t : int) (dnas : Genome[]) : seq<Genome> =
        let bestMotifs0 = dnas |> Seq.map (Genome.Slice 0 k)
        let kmers = Genome.Kmers k dnas.[0]
        // generates the motifs for kmer motif in the first string from dnas
        let mkMotifs motif =
            let folder motifs i =
                let profile = Seq.rev motifs |> MotifMatrix.OfRowSeq |> count |> motifProfile
                profileMostProbableKmer dnas.[i] k profile :: motifs
            seq { 1 .. t - 1 }
            |> Seq.fold folder [motif]
            |> Seq.rev
            |> MotifMatrix.OfRowSeq
        [ MotifMatrix.OfRowSeq bestMotifs0 ]
        |> Seq.append (kmers |> Seq.map mkMotifs)
        |> Seq.minBy motifScore
        |> MotifMatrix.ToRows
        |> Seq.ofArray

    let greedyMotifSearch = greedyMotifSearchWithCount motifCount
    let greedyMotifSearchWithPseudoCount = greedyMotifSearchWithCount motifPseudoCount 

    let runMotifEnumeration f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let d = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.OfString
        let output = motifEnumeration k d dnas
        output |> Seq.map string |> format
        
    let runMedianString f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let k = int content.[0]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.OfString
        let output = medianString k dnas
        output |> string

    let runProfileMostProbableKmer f =
        let content = System.IO.File.ReadLines f |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.toArray
        let dna = content.[0] |> Genome.OfString
        let k = int content.[1]
        let profile = content.[2 .. 5] |> Seq.map (fun s -> s.Split(' ') |> Seq.map float) |> array2D
        let output = profileMostProbableKmer dna k profile
        output |> string

    let runGreedyMotifSearchAny search f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let t = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.OfString |> Seq.toArray
        let output = search k t dnas
        output |> Seq.map string |> format

    let runGreedyMotifSearch = runGreedyMotifSearchAny greedyMotifSearch
    let runGreedyMotifSearchWithPseudoCounts = runGreedyMotifSearchAny greedyMotifSearchWithPseudoCount

    let runMotifDistance f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let pattern = Genome.OfString content.[0]
        let dnas = content.[1].Split ' ' |> Seq.map Genome.OfString |> MotifMatrix.OfRowSeq
        let output = motifDistance pattern dnas
        output |> string

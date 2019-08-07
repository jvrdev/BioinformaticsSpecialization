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

    type MotifMatrix = private MotifMatrix of Genome[]
    with 
        static member OfRows : Genome[] ->  MotifMatrix = function
            | [||] -> MotifMatrix [||]
            | rows -> 
                let rowLength = rows.[0].Length
                if rows |> Array.forall (fun r -> r.Length = rowLength) |> not then
                    invalidArg "rows" "Not all rows are of the same length"
                else MotifMatrix rows
        static member OfRowSeq = Seq.toArray >> MotifMatrix.MotifMatrix
        static member ToRows (MotifMatrix rows) = rows
        static member MapRows f = MotifMatrix.ToRows >> Seq.map f >> MotifMatrix.OfRowSeq
        member x.ColCount = 
            match MotifMatrix.ToRows x with
            | [||] -> 0
            | rows -> rows.[0].Length
        member x.RowCount = 
            match MotifMatrix.ToRows x with
            | [||] -> 0
            | rows -> rows.Length
            
    let motifCount (m : MotifMatrix) : int[,] =
        let count = Array2D.create 4 m.ColCount 0
        for row in MotifMatrix.ToRows m do
            for (j, cell) in Genome.ToSeq row |> Seq.indexed do
                let i = Nucleobase.ToInt cell
                count.[i, j] <- count.[i, j] + 1
        count

    let motifProfile (m : int[,]) : float[,] =
        let sums = Array.create (m.GetLength 1) 0.0
        for i = 0 to m.GetLength 0 - 1 do
            for j = 0 to m.GetLength 1 - 1 do
                sums.[j] <- sums.[j] + float m.[i, j]
        Array2D.mapi (fun i j x -> (float x) / sums.[j]) m

    let motifDistance (pattern : Genome) (m : MotifMatrix) : int =
        MotifMatrix.ToRows m
        |> Seq.sumBy (patternDistance pattern)

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

    let kmerProbability (profile : float[,]) (kmer : Genome) : float = 
        let k = kmer.Length
        if profile.GetLength 0 <> 4 then invalidArg "profile" "Profile is expected to have 4 rows"
        elif profile.GetLength 1 <> k then invalidArg "profile" "Profile column count is expected to match length of k-mer"
        else
            seq {
                for j = 0 to k - 1 do
                    let i = Nucleobase.ToInt (Genome.Item j kmer)
                    yield profile.[i, j]
            }
            |> Seq.fold (*) 1.0

    let profileMostProbableKmer (text : Genome) (k : int) (profile : float[,]) : Genome =
        Genome.Kmers k text
        |> Seq.maxBy (kmerProbability profile)

    let greedyMotifSearch (k : int) (t : int) (m : MotifMatrix) : seq<Genome> =
        let bestMotifs = MotifMatrix.MapRows (Genome.Slice 0 k) m
        bestMotifs
        |> MotifMatrix.ToRows
        |> Seq.ofArray

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

    let runGreedyMotifSearch f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let t = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.ofString
        let output = greedyMotifSearch k t dnas
        output |> Seq.map string |> format

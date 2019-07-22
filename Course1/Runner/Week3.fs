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

    type MotifMatrix = private MotifMatrix of Genome[]
    with 
        static member OfRows : Genome[] ->  MotifMatrix = function
            | [||] -> MotifMatrix [||]
            | rows -> 
                let rowLength = rows.[0].Length
                if rows |> Array.forall (fun r -> r.Length = rowLength) |> not then
                    invalidArg "rows" "Not all rows are of the same length"
                else MotifMatrix rows
        static member ToRows (MotifMatrix rows) = rows
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

    //let medianString (k : int) (gs : seq<Genome>) : Genome =
    //    let kmers = Genome.Enumerate k

    let runMotifEnumeration f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let d = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter System.String.IsNullOrWhiteSpace |> Seq.map Genome.OfString
        let output = motifEnumeration k d dnas
        output |> Seq.map string |> format
        
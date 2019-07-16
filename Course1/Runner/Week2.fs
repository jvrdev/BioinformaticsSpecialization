namespace Bio

open System

module Week2 =
    let patternToNumber (pattern : Genome) =
        let mutable a = 0
        for i = 0 to pattern.Length - 1 do
            a <- a * 4 + Nucleobase.ToInt (Genome.Item i pattern)
        a

    let numberToPattern k index =
        let a = Array.create k A
        let mutable x = index
        for i = k - 1 downto 0 do
            a.[i] <- Nucleobase.OfInt (x % 4)
            x <- x / 4
        Genome <| ArraySegment a
        
    let clean (s : string) = s.Replace("\r", "").Replace("\n", "")

    let skew = function
        | A | T -> 0
        | G -> +1
        | C -> -1

    let skews (Genome x) = x |> Seq.scan (fun s n -> s + skew n) 0 |> Seq.toArray

    let minSkewPositions (Genome x as g) : int[] = 
        let skews = skews g
        Seq.findAllMinIndexes skews |> Seq.toArray

    let hammingDistance (Genome a) (Genome b) =
        Seq.zip a b
        |> Seq.sumBy (fun (a, b) -> if a = b then 0 else 1)

    let mkApproximateMatchArray (pattern : Genome) (text: Genome) (d : int) =
        let k = pattern.Length
        seq { 
            for i = 0 to text.Length - k do
                if hammingDistance (Genome.Slice i k text) pattern <= d
                then yield 1
                else yield 0
        }
        |> Seq.toArray

    let approximatePatternMatch (pattern : Genome) (text: Genome) (d : int) : int[] =
        mkApproximateMatchArray pattern text d
        |> Seq.findAllIndexes ((=)1)
        |> Seq.toArray

    let countD (pattern : Genome) (text: Genome) (d : int) : int =
        mkApproximateMatchArray pattern text d
        |> Array.sum  

    let rec neighbors (pattern : Genome) (d : int) : list<Genome> =
        if d = 0 then [ pattern ]
        elif pattern.Length = 1 then Nucleobase.Enumerate |> List.map (fun n -> Genome <| ArraySegment [|n|])
        else 
            let suffix = Genome.Suffix pattern
            let suffixNeighbors = neighbors suffix d
            suffixNeighbors
            |> List.collect (
                fun suffixNeighbor ->
                    if hammingDistance suffix suffixNeighbor < d
                    then Nucleobase.Enumerate |> List.map (fun n -> Genome.Prepend n suffixNeighbor)
                    else [Genome.Prepend (Genome.Item 0 pattern) suffixNeighbor]
            )

    let frequentWordsWithMismatches (text : Genome) (k : int) (d : int) : Genome[] =
        let n = pow 4 k
        let a = Array.create n 0
        for i = 0 to text.Length - k do
            let pattern = Genome.Slice i k text
            for neighbor in neighbors pattern d do
                let index = patternToNumber neighbor
                a.[index] <- a.[index] + 1
        let max = Array.max a
        a
        |> Seq.findAllIndexes ((=)max)
        |> Seq.map (numberToPattern k)
        |> Seq.toArray
                
    let runMinSkewPositions f =
        let content = read f |> clean
        let genome = Genome.ofString content
        let minPositions = minSkewPositions genome
        minPositions |> Seq.map string |> format

    let runHammingDistance f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let a = Genome.ofString content.[0]
        let b = Genome.ofString content.[1]
        let distance = hammingDistance a b
        string distance

    let runApproximatePatternMatch f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let pattern = Genome.ofString content.[0]
        let genome = Genome.ofString content.[1]
        let d = int content.[2]
        let output = approximatePatternMatch pattern genome d
        output |> Seq.map string |> format

    let runCountD f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let pattern = Genome.ofString content.[0]
        let genome = Genome.ofString content.[1]
        let d = int content.[2]
        let output = countD pattern genome d
        output |> string

    let runNeighbors f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let pattern = Genome.ofString content.[0]
        let d = int content.[1]
        let output = neighbors pattern d
        output |> Seq.map string |> format

    let runFrequentWordsWithMismatches f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let pattern = Genome.ofString content.[0]
        let fields = content.[1].Split ' '
        let k = int fields.[0]
        let d = int fields.[1]
        let output = frequentWordsWithMismatches pattern k d
        output |> Seq.map string |> format
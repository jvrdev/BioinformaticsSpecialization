namespace Bio

open System

module Week4 =
    let random = System.Random ()
    let dice maxValue = random.Next maxValue

    let randomKmer k (g : Genome) =
        let startIndex = dice <| g.Length - k + 1
        Genome.Slice startIndex k g

    let randomizedMotifSearchRun (k : int) (t : int) (dnas : Genome[]) =
        let motifs0 = dnas |> Seq.map (randomKmer k)
        let score0 = motifScore (motifs0 |> MotifMatrix.OfRowSeq)
        let rec improveMotifs (motifs, score) =
            let profile = motifs |> MotifMatrix.OfRowSeq |> motifPseudoCount |> motifProfile
            let motifs' = motifsFromMostProbableKmer k profile dnas
            let score' = motifScore (motifs' |> MotifMatrix.OfRowSeq)
            if score' < score
            then improveMotifs (motifs', score')
            else (motifs, score)
        improveMotifs (motifs0, score0)

    let randomizedMotifSearch (k : int) (t : int) (dnas : Genome[]) : seq<Genome> =
        if t <> dnas.Length then failwith "t does not match dnas count"
        Seq.replicate 1000 ()
        |> Seq.map (fun () -> randomizedMotifSearchRun k t dnas)
        |> Seq.minBy snd
        |> fst

    let randomSample (probabilities : seq<float>) (rnd : float) : int =
        let sum = Seq.sum probabilities
        let rndScaled = rnd * sum
        use iterator = probabilities.GetEnumerator ()
        let rec loop (acc : float) (i : int) (probability : float) =
            let acc' = acc + probability
            if acc' > rndScaled || not (iterator.MoveNext ()) then i
            else loop acc' (i + 1) iterator.Current
        if iterator.MoveNext () 
        then loop (0.0) 1 iterator.Current
        else failwith "Probabilities is empty"

    let p (probabilities : seq<float>) : int =
        random.NextDouble () |> randomSample probabilities

    let gibbsSamplerRun (k : int) (t : int) (n : int) (dnas : Genome[]) =
        let motifs0 = dnas |> Seq.map (randomKmer k)
        let score0 = motifScore (motifs0 |> MotifMatrix.OfRowSeq)
        let rec improveMotifs (motifs, score) =
            let profile = motifs |> MotifMatrix.OfRowSeq |> motifPseudoCount |> motifProfile
            let motifs' = motifsFromMostProbableKmer k profile dnas
            let score' = motifScore (motifs' |> MotifMatrix.OfRowSeq)
            if score' < score
            then improveMotifs (motifs', score')
            else (motifs, score)
        improveMotifs (motifs0, score0)

    let gibbsSampler (k : int) (t : int) (n : int) (dnas : Genome[]) : seq<Genome> =
        if t <> dnas.Length then failwith "t does not match dnas count"
        Seq.replicate 20 ()
        |> Seq.map (fun () -> gibbsSamplerRun k t n dnas)
        |> Seq.minBy snd
        |> fst

    let runMotifSearchAny search f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let t = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.OfString |> Seq.toArray
        let output = search k t dnas
        output |> Seq.map string |> format

    let runRandomizedMotifSearch = runMotifSearchAny randomizedMotifSearch
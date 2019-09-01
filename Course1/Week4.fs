namespace Bio

open System
open Bio.Common

module Week4 =
    let random = System.Random ()
    /// Returns a value between 0 and (maxValue - 1)
    let dice maxValue = random.Next maxValue

    let randomKmer k (g : Genome) =
        let startIndex = dice <| g.Length - k + 1
        Genome.Slice startIndex k g

    let randomMotifs (k : int) (dnas : Genome[]) = 
        dnas |> Seq.map (randomKmer k) |> Seq.toArray

    let randomizedMotifSearchRun (k : int) (t : int) (dnas : Genome[]) =
        let motifs0 = randomMotifs k dnas
        let score0 = motifScore (motifs0 |> MotifMatrix.OfRows)
        let rec improveMotifs (motifs, score) =
            let profile = motifs |> MotifMatrix.OfRows |> motifPseudoCount |> motifProfile
            let motifs' = motifsFromMostProbableKmer k profile dnas
            let score' = motifScore (motifs' |> MotifMatrix.OfRows)
            if score' < score
            then improveMotifs (motifs', score')
            else (motifs, score)
        improveMotifs (motifs0, score0)

    let randomizedMotifSearch (runCount) (k : int) (t : int) (dnas : Genome[]) : seq<Genome> =
        if t <> dnas.Length then failwith "t does not match dnas count"
        Seq.replicate runCount ()
        |> Seq.map (fun () -> randomizedMotifSearchRun k t dnas)
        |> Seq.minBy snd
        |> fst
        |> Seq.ofArray

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

    /// Returns a value between 1 and |probabilities|
    let p (probabilities : seq<float>) : int =
        random.NextDouble () |> randomSample probabilities

    let profileRandomlyGeneratedKmerInTheIthSequence (k : int) (profile : float[,]) (text : Genome) =
        let kmers = Genome.Kmers k text
        let probabilities = kmers |> Seq.map (kmerProbability profile)
        let i = p probabilities
        kmers |> Seq.item (i - 1)

    let gibbsSamplerRun motifs0 (k : int) (t : int) (n : int) (dnas : Genome[]) =
        let score0 = motifScore (motifs0 |> MotifMatrix.OfRows)
        let rec improveMotifs (motifs, bestMotifs, bestScore) repeat =
            if repeat = 0
            then bestMotifs, bestScore
            else 
                let i = dice t
                let profile =
                    motifs
                    |> Seq.indexed
                    |> Seq.filter (fun (j, _) -> j <> i)
                    |> Seq.map snd
                    |> MotifMatrix.OfRowSeq
                    |> motifPseudoCount
                    |> motifProfile
                let motifsNext =
                    motifs
                    |> Array.mapi (
                        fun j x ->
                            if j <> i
                            then x
                            else profileRandomlyGeneratedKmerInTheIthSequence k profile (dnas |> Seq.item i))
                let scoreNext = motifScore (motifsNext |> MotifMatrix.OfRows)
                let bestMotifsNext, bestScoreNext = 
                    if scoreNext < bestScore
                    then motifsNext, scoreNext
                    else bestMotifs, bestScore
                improveMotifs (motifsNext, bestMotifsNext, bestScoreNext) (repeat - 1)
        improveMotifs (motifs0, motifs0, score0) n

    let gibbsSampler runCount (k : int) (t : int) (n : int) (dnas : Genome[]) : Genome[] * int =
        if t <> dnas.Length then failwith "t does not match dnas count"
        Seq.replicate runCount ()
        |> Array.ofSeq
        |> Array.Parallel.map (fun () -> gibbsSamplerRun (randomizedMotifSearch 30 k t dnas |> Seq.toArray) k t n dnas)
        |> Seq.minBy snd

    let runMotifSearchAny search f =
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let t = int fields.[1]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.OfString |> Seq.toArray
        let output = search k t dnas
        output |> Seq.map string |> format

    let runRandomizedMotifSearch = runMotifSearchAny (randomizedMotifSearch 1000)

    let runGibbsSampler f = 
        let content = System.IO.File.ReadLines f |> Seq.toArray
        let fields = content.[0].Split ' '
        let k = int fields.[0]
        let t = int fields.[1]
        let n = int fields.[2]
        let dnas = content |> Seq.skip 1 |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Seq.map Genome.OfString |> Seq.toArray
        let output, score = gibbsSampler 20 k t n dnas
        output |> Seq.map string |> Seq.append [sprintf "Score:%d" score ] |> format2
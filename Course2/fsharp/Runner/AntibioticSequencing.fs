namespace Course2

module AntibioticSequencing =
    type Peptideable<'c, 'e> = {
        Mass : 'e -> int
        ToSeq : 'c -> seq<'e>
    }

    module Peptideable =
        module Instances =
            let peptide : Peptideable<Peptide, Aminoacid> = 
                { Mass = Aminoacid.integerMass; ToSeq = id }

            let massPeptide : Peptideable<MassPeptide, int> = 
                { Mass = id; ToSeq = MassPeptide.toSeq }

    let proteinTranslation (pattern : seq<Base>) : Peptide =
        pattern
        |> Seq.chunkBySize 3
        |> Seq.choose (fun a ->
            match Codon.ofArray a with
            | Some codon ->
                Aminoacid.ofCodon codon
            | None -> None
        )

    // returns the substrings of DNA that encode Peptide
    let peptideEncoding (dna : Dna) (peptide : Peptide) : seq<Dna> =
        let peptideLength = Peptide.length peptide
        let substringsEncodingPeptide (Dna d) =
            d 
            |> Seq.windowed (peptideLength * 3)
            |> Seq.choose (fun chunk ->
                let dna = Dna.mk chunk
                let dnaR = Dna.reverseComponent dna
                let rna = Dna.transcribe dna
                let rnaR = Dna.transcribe dnaR
                let p = Peptide.ofRna rna
                let pr = Peptide.ofRna rnaR
                if sequenceEquals peptide p || sequenceEquals peptide pr
                then Some dna
                else None
            )
        substringsEncodingPeptide dna

    let mkPrefixMass mass peptide =
        peptide
        |> Seq.fold
            (fun (h::t) a -> (mass a + h)::h::t)
            [0]
        |> List.rev
        |> Seq.toArray

    let linearSpectrum { Mass = mass ; ToSeq = toSeq } (peptideC : 'c) : Spectrum =
        let peptide = toSeq peptideC
        let peptideLength = Seq.length peptide
        let prefixMass = mkPrefixMass mass peptide
        let spectrum = 
            Seq.singleton 0
            |> Seq.append (
                seq {
                    for i in 0 .. peptideLength - 1 do
                        for j in i + 1 .. peptideLength do
                            yield prefixMass.[j] - prefixMass.[i]
                }
            )            
            |> Seq.toArray
        Array.sortInPlace spectrum
        Spectrum <| seq spectrum

    let linearSpectrumPeptide = linearSpectrum Peptideable.Instances.peptide
    let linearSpectrumMassPeptide = linearSpectrum Peptideable.Instances.massPeptide

    let cyclicSpectrum { Mass = mass ; ToSeq = toSeq } (peptideC : 'c) : Spectrum =
        let peptide = toSeq peptideC
        let peptideLength = Seq.length peptide
        let prefixMass = mkPrefixMass mass peptide
        let peptideMass = Array.last prefixMass
        let spectrum = 
            Seq.singleton 0
            |> Seq.append (
                seq {
                    for i in 0 .. peptideLength - 1 do
                        for j in i + 1 .. peptideLength do
                            yield prefixMass.[j] - prefixMass.[i]
                            if i > 0 && j < peptideLength
                                then yield peptideMass - (prefixMass.[j] - prefixMass.[i])
                }
            )            
            |> Seq.toArray
        Array.sortInPlace spectrum
        Spectrum <| seq spectrum

    let cyclicSpectrumPeptide = cyclicSpectrum Peptideable.Instances.peptide
    let cyclicSpectrumMassPeptide = cyclicSpectrum Peptideable.Instances.massPeptide
    
    let peptideCountWithMass (x : int) =
        let stepCache = Array.zeroCreate (x + 1)
        let masses = 
            Aminoacid.all 
            |> Seq.map Aminoacid.integerMass
            |> Seq.distinct
            |> Seq.toArray

        let rec step (m : int) =
            if stepCache.[m] > 0L
            then stepCache.[m]
            else
                let result = 
                    masses 
                    |> Seq.sumBy (fun n ->
                        if n = m then 1L
                        elif n > m then 0L
                        else step (m - n))
                stepCache.[m] <- result
                result
        step x

    let subpeptideCountLinear (n : int) =
        seq { 1 .. n } |> Seq.fold (+) 1 

    let cyclopeptideSequencing (s : Spectrum) : list<MassPeptide> =
        let isCompatible = Spectrum.isCompatible s
        let isEqual = Spectrum.isEqualToSortedSpectrum s
        let spectrumParentMass = Spectrum.parentMass s
        let masses = 
            Aminoacid.all 
            |> Seq.map Aminoacid.integerMass
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toList

        let candidates0 : list<MassPeptide> = [ MassPeptide.empty ]
        let finals0 : list<MassPeptide> = [ ]

        let rec expand (candidates : list<MassPeptide>) (finals : list<MassPeptide>) : list<MassPeptide> =
            if List.isEmpty candidates
            then finals
            else 
                let candidatesPrime =
                    candidates
                    |> List.collect (fun (MassPeptide c) -> masses |> List.map (fun m -> MassPeptide (m::c)))
                bound candidatesPrime finals
        and bound (candidates : list<MassPeptide>) (finals : list<MassPeptide>) : list<MassPeptide> =
            let candidatesPrime, finalsPrime =
                candidates
                |> List.fold
                    (fun (cs, fs) candidate ->
                        let candidateMass = MassPeptide.totalMass candidate
                        if candidateMass = spectrumParentMass
                        then
                            let candidateCyclicSpectrum = cyclicSpectrumMassPeptide candidate
                            let spectrumsMatch = isEqual candidateCyclicSpectrum
                            if spectrumsMatch then cs, candidate::fs else cs, fs
                        else
                            let linearCandidateSpectrum = linearSpectrumMassPeptide candidate
                            let spectrumsCompatible = isCompatible linearCandidateSpectrum
                            if spectrumsCompatible then candidate::cs, fs else cs, fs
                      )
                    ([], finals)
            expand candidatesPrime finalsPrime

        expand candidates0 finals0

    let linearScore peptideable peptide spectrum =
        let actualSpectrum = linearSpectrum peptideable peptide
        Spectrum.score actualSpectrum spectrum
    
    let linearScorePeptide = linearScore Peptideable.Instances.peptide
    let cyclicScorePeptide (peptide : Peptide) (spectrum : Spectrum) : int =
        let actual = cyclicSpectrumPeptide peptide
        Spectrum.score actual spectrum    

    let trim 
        (peptideable: Peptideable<'peptide,'e>)
        (leaderboard : seq<'peptide>)
        (spectrum : Spectrum)
        (n : int) : seq<'peptide> =
        leaderboard
        |> Seq.map (fun peptide -> peptide, linearScore peptideable peptide spectrum)
        |> Seq.sortByDescending snd
        |> Seq.fold 
            (fun (peptides, length, minScoreWanted) (peptide, score) ->
                if length >= n
                then 
                    if score = minScoreWanted 
                    then peptide :: peptides, length + 1, score
                    else peptides, length, minScoreWanted
                else peptide :: peptides, length + 1, score)
            ([], 0, System.Int32.MaxValue)
        |> fun (peptides, _, _) -> peptides |> List.rev |> Seq.ofList

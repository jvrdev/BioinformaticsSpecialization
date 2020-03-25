namespace Course2

module Week3 =
    let proteinTranslation (pattern : seq<Base>) : Peptide =
        pattern
        |> Seq.chunkBySize 3
        |> Seq.choose (fun a ->
            match Codon.ofArray a with
            | Some codon ->
                Aminoacid.ofCodon codon
            | None -> None
        )

    let proteinTranslationS (s : string) : string =
        let pattern = s |> Seq.choose Base.ofChar
        let peptide = proteinTranslation pattern
        Peptide.toString peptide

    let peptideEncoding (dna : Dna) (peptide : Peptide) : seq<Dna> =
        let peptideLength = Peptide.length peptide
        let substringsEncodingPeptide (Dna d) =
            let (Rna r) = Dna.transcribe (Dna d)
            d 
            |> Seq.zip r
            |> Seq.windowed (peptideLength * 3)
            |> Seq.choose (fun chunk ->
                let dna = Dna.mk (chunk |> Seq.map fst)
                let rna = Rna.mk (chunk |> Seq.map snd)
                let p = Peptide.ofRna rna
                if sequenceEquals peptide p
                then Some dna
                else None
            )
        substringsEncodingPeptide dna
        |> Seq.append (substringsEncodingPeptide (Dna.reverseComponent dna))

    let peptideEncodingS (s : string) : string =
        match splitLines s |> Array.filter (not << System.String.IsNullOrWhiteSpace) with
        | [|dnaString; peptideString|] -> 
            
        | other -> "Invalid number of lines entered ```\n%A\n```" other
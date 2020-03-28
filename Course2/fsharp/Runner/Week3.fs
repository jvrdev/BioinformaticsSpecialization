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

    let peptideEncodingS (s : string) : string =
        match String.splitLines s |> Array.filter (not << System.String.IsNullOrWhiteSpace) with
        | [|dnaString; peptideString|] -> 
            let dna = Dna.parse dnaString
            let peptide = Peptide.parse peptideString
            match dna, peptide with
            | None, _ -> failwithf "DNA cannot be read from '%s'" dnaString
            | Some _, None -> failwithf "Peptide canot be read from '%s'" peptideString
            | Some d, Some p -> 
                let dnas = peptideEncoding d p
                dnas
                |> Seq.map Dna.toString
                |> String.concat System.Environment.NewLine
        | other -> failwithf "Invalid number of lines on input```\n%A\n```" other
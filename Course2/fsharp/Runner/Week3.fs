namespace Course2

module Week3 =
    open AntibioticSequencing

    let proteinTranslationS (s : string) : string =
        let pattern = s |> Seq.choose Base.ofChar
        let peptide = proteinTranslation pattern
        Peptide.toString peptide

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

    let spectrumS f (s : string) : string =
        match Peptide.parse s with
        | Some p -> 
            let spectrum = f p
            spectrum |> Seq.map string |> String.concat " "
        | None -> failwithf "Peptide could not be read from '%s'" s

    let linearSpectrumS = spectrumS linearSpectrum
    let cyclicSpectrumS = spectrumS (cyclicSpectrum Aminoacid.integerMass)

    let cyclopeptideSequencingS (s : string) : string = 
        match Spectrum.parse s with
        | None -> failwithf "Spectrum cannot be read %s'" s
        | Some spectrum -> 
            let candidates = cyclopeptideSequencing spectrum
            candidates
            |> Seq.map (fun c -> c |> Seq.map string |> String.concat "-")
            |> Seq.sortDescending
            |> String.concat " "
namespace Course2

module Week4 =
    open AntibioticSequencing

    let mkScoringProblemS f (s : string) : string =
         match String.splitLines s |> Array.filter (not << System.String.IsNullOrWhiteSpace) with
         | [|peptideString; spectrumString|] -> 
             let peptide = Peptide.parse peptideString
             let spectrum = Spectrum.parse spectrumString
             match peptide, spectrum with
             | None, _ -> failwithf "Peptide canot be read from '%s'" peptideString
             | Some _, None -> failwithf "Spectrum cannot be read from '%s'" spectrumString
             | Some p, Some s -> 
                 let score = f p s
                 string score
         | other -> failwithf "Invalid number of lines on input```\n%A\n```" other
    let cyclopeptideScoringProblemS = mkScoringProblemS cyclicScorePeptide
    let linearpeptideScoringProblemS = mkScoringProblemS linearScorePeptide

    let trimS (s : string) : string =
         match String.splitLines s |> Array.filter (not << System.String.IsNullOrWhiteSpace) with
         | [|peptideStrings; spectrumString; nString |] -> 
             let peptides = String.split " " peptideStrings |> Seq.choose Peptide.parse 
             let spectrum = Spectrum.parse spectrumString
             let (nSuccess, n) = System.Int32.TryParse nString
             match nSuccess, spectrum with
             | false, _ -> failwithf "N cannot be read from '%s'" nString
             | true, None -> failwithf "Spectrum cannot be read from '%s'" spectrumString
             | true, Some s -> 
                 let peptidesPrime = trim Peptideable.Instances.peptide peptides s n
                 peptidesPrime |> Seq.map Peptide.toString |> String.concat " "
         | other -> failwithf "Invalid number of lines on input```\n%A\n```" other



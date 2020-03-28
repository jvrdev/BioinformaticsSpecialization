namespace Course2

module Program =
    open Weeks1And2
    
    let runOnFile f path =
        let input = System.IO.File.ReadAllText path
        let result = f input
        System.IO.File.WriteAllText ("result.txt", result, System.Text.Encoding.ASCII)

    let runStringSpelledByGappedPatterns (s : string) : string =
        let k, d, pairs = readGappedPatterns s

        let result = stringSpelledByReadPairs Reconstruction.Instances.array (k, d) pairs

        result |> Option.map System.String |> Option.defaultValue ""

    let runEuler (x : string) : string =
        let graph = DirectedGraph.parse int x
        let walk = Euler.eulerPath graph
        Walk.print string walk

    let runStringReconstruction (x : string) : string =
        let k, dnas = readStringReconstruction x
        Reconstruction.stringReconstruction k dnas

    let runMaximalNonBranchingPaths (x : string) : string =
        let graph = DirectedGraph.parse int x
        let actual = maximalNonBranchingPaths graph
        let formatted = 
            actual
            |> Seq.map (Walk.print string)
            |> Seq.sort
            |> String.concat "\r\n"
        formatted

    let runContigGeneration (x : string) : string =
        let dnas = 
            x
            |> String.splitLines
            |> Seq.map String.trim
            |> Seq.filter (not << System.String.IsNullOrWhiteSpace)
            |> Seq.toArray
        let output = contigGeneration dnas 
        String.concat " " output

    let runStringReconstructionFromReadPairs (s : string) : string =
        let k, d, pairs = readGappedPatterns s
        let result =
            stringReconstructionFromReadPairs
                (Debruijn.Instances.mkReadPairString k)
                (Reconstruction.Instances.string)
                (k, d) 
                (pairs |> Seq.map (ReadPair.map System.String))
        result

    [<EntryPoint>] 
    let main argv =
        runOnFile Week3.peptideEncodingS """C:\src\BioinformaticsSpecialization\Course2\dataset_96_7.txt"""
        0
namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        let output = Week4.runRandomizedMotifSearch """C:\Users\Javier\Downloads\dataset_161_5(1).txt"""
        //runComputingFrequencies2 
        printfn "%s" output
        0
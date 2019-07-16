namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        let output = Week3.runMotifEnumeration """C:\Users\Javier\Downloads\dataset_156_8(1).txt"""
        //runComputingFrequencies2 
        printfn "%s" output
        0
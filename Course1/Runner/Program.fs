namespace Bio

module Program = 
    [<EntryPoint>]
    let main argv =
        let output = Week2.findOri """C:\Users\Javier\Downloads\Salmonella_enterica.txt"""
        0
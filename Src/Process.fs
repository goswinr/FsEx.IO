namespace FsEx

open System

/// A module to run command line processes via Diagnostics.Process()
[<RequireQualifiedAccess>]
module Process = 

    /// Run a command synchronously.
    /// The error and standard text output is redirected to the delegates 'output' and 'error'.
    let run (output:string->unit) (error:string->unit) processName arguments : unit =         
        //https://stackoverflow.com/questions/1145969/processinfo-and-redirectstandardoutput
        let p = new Diagnostics.Process()
        p.StartInfo.FileName <- processName
        p.StartInfo.Arguments <- arguments
        // for console also see https://stackoverflow.com/a/1427817/969070
        p.StartInfo.StandardOutputEncoding <- Text.Encoding.GetEncoding(Globalization.CultureInfo.CurrentCulture.TextInfo.OEMCodePage) //https://stackoverflow.com/a/48436394/969070
        p.StartInfo.StandardErrorEncoding  <- Text.Encoding.GetEncoding(Globalization.CultureInfo.CurrentCulture.TextInfo.OEMCodePage) //https://stackoverflow.com/a/48436394/969070
        p.EnableRaisingEvents <- true
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.CreateNoWindow <- true //true if the process should be started without creating a new window to contain it
        p.StartInfo.RedirectStandardError <-true
        p.StartInfo.RedirectStandardOutput <-true
        p.OutputDataReceived.Add ( fun d -> if not(isNull d.Data) then output(d.Data) )
        p.ErrorDataReceived.Add  ( fun d -> if not(isNull d.Data) then error (d.Data) )
        //p.Exited.Add( fun _ ->() )    //covered by p.WaitForExit()    
        p.Start() |> ignore
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()    
    
    /// Set this value to a function that prints in red. (not followed by a line return.)
    /// By default this function just does F#'s printf.
    /// It will be used in Process.runWithHighlighting function.
    let mutable redPrinter   : (string -> unit) = printf "%s"

    /// Set this value to a function that prints in green. (not followed by a line return.)
    /// By default this function just does F#'s printf.
    /// It will be used in Process.runWithHighlighting function.
    let mutable greenPrinter : (string -> unit) = printf "%s"

    /// Set this value to a function that prints in gray. (not followed by a line return.)
    /// By default this function just does F#'s printf.
    /// It will be used in Process.runWithHighlighting function.
    let mutable grayPrinter  : (string -> unit) = printf "%s"

    /// Set this value to a function that prints in black. (not followed by a line return.)
    /// By default this function just does F#'s printf.
    /// It will be used in the Process.runWithHighlighting function.
    let mutable blackPrinter  : (string -> unit) = printf "%s"
    
    /// Highlights every occurrence of the given word in the color of first three integers (red, green, blue)
    /// and the rest of the line in next three integers.
    /// Adds a line return at end.    
    let internal printWithHighlightColor colorPrinter (word:string) (fullLine:string)= 
        if String.IsNullOrWhiteSpace word then
            grayPrinter (fullLine)
            grayPrinter Environment.NewLine
        else
            let rec loop (fromIdx:int) = 
                match fullLine.IndexOf(word, fromIdx, StringComparison.Ordinal) with
                | -1 -> 
                    grayPrinter (fullLine.Substring(fromIdx))
                    grayPrinter Environment.NewLine
                | i  ->
                    let beforeLen = i - fromIdx
                    if beforeLen > 0 then grayPrinter (fullLine.Substring(fromIdx,beforeLen))

                    if i + word.Length = fullLine.Length then
                        colorPrinter (fullLine.Substring(i,word.Length)) 
                        colorPrinter Environment.NewLine
                    else
                        colorPrinter (fullLine.Substring(i,word.Length)) // no line return
                        loop (i + word.Length)
            loop 0    


    
    /// Run a command synchronously.
    /// ProcessName, the arguments, output text and errors will be printed to Console.Out
    /// Each process's output is surrounded by a border drawn with ASCII art characters.
    /// If the Process.XXXPrinter functions are set, the words in redHighlights and greenHighlights will be highlighted if found in the output text.      
    /// Before using this method set these mutable functions:
    ///   - Process.redPrinter
    ///   - Process.greenPrinter
    ///   - Process.grayPrinter
    ///   - Process.blackPrinter
    /// Otherwise it will still print to Console.Out stream, but without highlighting.
    let runWithHighlighting (redHighlights:string list) (greenHighlights:string list) processName arguments : unit =               
        blackPrinter ("┌─────────────────────────────────────────────────────────────────" + Environment.NewLine)  
        blackPrinter     "│"
        blackPrinter processName 
        blackPrinter " "
        blackPrinter arguments           
        run            
            (fun txt ->
                let mutable printPending = true
                for r in redHighlights do
                    if printPending && not (String.IsNullOrWhiteSpace r) && txt.Contains(r) then //TODO: add Regex based highlighting
                        blackPrinter "│"
                        printWithHighlightColor redPrinter r txt
                        printPending <- false
                for g in greenHighlights do
                    if  printPending && not (String.IsNullOrWhiteSpace g) && txt.Contains(g) then
                        blackPrinter "│"
                        printWithHighlightColor greenPrinter g txt
                        printPending <- false
                if printPending then
                    blackPrinter "│"
                    grayPrinter txt  )
            (fun eTxt -> 
                blackPrinter "│"    
                redPrinter eTxt )
            processName
            arguments
        blackPrinter ("└─────────────────────────────────────────────────────────────────" + Environment.NewLine)




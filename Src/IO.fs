﻿namespace FsEx

open System
open System.IO
open System.Collections.Generic




/// FsEx.IO  I/O utilities
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide IO namespace in C# assemblies
module IO =    

    type FileNotFoundException with
        /// Raise FileNotFoundException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (FileNotFoundException(s))) msg

    type DirectoryNotFoundException with
        /// Raise DirectoryNotFoundException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (DirectoryNotFoundException(s))) msg




    /// Returns the path to the current Desktop (excluding a trailing slash)
    /// Contains forward slashes only
    /// Environment.GetFolderPath(Environment.SpecialFolder.Desktop).Replace('\\','/') 
    let desktop = 
        Environment.GetFolderPath(Environment.SpecialFolder.Desktop).Replace('\\','/') 

    
    /// Returns the path to the current Desktop(including a trailing slash '/').
    /// Contains forward slashes only
    /// Environment.GetFolderPath(Environment.SpecialFolder.Desktop).Replace('\\','/')  + "/"
    let desktopS = 
        Environment.GetFolderPath(Environment.SpecialFolder.Desktop).Replace('\\','/')  + "/"


    /// Raises an FileNotFoundException if the file path does not exist
    let checkIfFileExists s = 
        if not (IO.File.Exists s) then  raise (FileNotFoundException("File missing or path wrong: '" + s + "'"))

     /// Raises an DirectoryNotFoundException if the directory path does not exist
    let checkIfDirectoryExists s = 
        if not (IO.Directory.Exists s) then  raise (DirectoryNotFoundException("Directory missing or path wrong: '" + s + "'"))

    /// Given the full path to a file. This function creates all directories to this files if they don't exist yet.
    /// However it does not create the file itself.
    let createDirectoriesOfFilePath s = 
        let fi = FileInfo(s)
        Directory.CreateDirectory(fi.DirectoryName)
        |> ignore      


    /// Returns all files in folder and sub-folders.
    /// Returns first the files from this folder then from first child folder with all its grandchildren
    /// then next child folder with all its grandchildren...
    /// May fail with IOExceptions.
    let getAllFiles (directory:string) = 
        if not (Directory.Exists(directory)) then
            raise (DirectoryNotFoundException("IO.getAllFiles: Directory '" + directory + "' not found"))
        let rec get (dir:string) = 
            seq {   yield! Directory.EnumerateFiles(dir)
                    for d in Directory.EnumerateDirectories(dir) do yield! get d }
        get directory

    /// Returns all files in folder and sub-folders that fit pattern (e.g. '*.pdf').
    /// Returns first the files from this folder then from first child folder with all its grandchildren
    /// then next child folder with all its grandchildren...
    /// Wild-card specifiers:
    /// * (asterisk)    Zero or more characters in that position.
    /// ? (question mark)    Zero or one character in that position.
    /// May fail with IOExceptions.
    let getAllFilesByPattern (pattern:string) (directory:string) = 
        if not (Directory.Exists(directory)) then
            raise (DirectoryNotFoundException("IO.getAllFilesByPattern: Directory '" + directory + "' not found for file pattern : '"+pattern+"'"))
        let rec get (dir:string) = 
            seq {   yield! Directory.EnumerateFiles(dir, pattern)
                    for d in Directory.EnumerateDirectories(dir) do yield! get d }
        get directory  
    
    /// Returns all files in folder and sub-folders
    /// Ignores all errors except if the initial directory does not exist and moves on to next folder.
    /// Returns first the files from this folder (sorted) then from first child folder with all its grandchildren
    let getAllFilesSave (directory:string) = 
        if not (Directory.Exists(directory)) then
            raise (DirectoryNotFoundException("IO.getAllFilesSave: Directory '" + directory + "' not found "))
        let rec getAll (dir:string) = 
            seq { if Directory.Exists dir then
                    let files = try Directory.GetFiles(dir) with _ -> [||]
                    Array.Sort(files)// a before Z
                    //Array.sortInPlace files // Z before a
                    yield! files
                    let dirs = try Directory.GetDirectories(dir) with _ -> [||]
                    Array.Sort(dirs)// a before Z
                    //Array.sortInPlace dirs // Z before a
                    for subDir in dirs do
                        yield! getAll subDir }
        getAll(directory)
    
    /// Returns all files in this folder and parent folders that fit pattern (e.g. '*.pdf').
    /// Wild-card specifiers:
    /// * (asterisk)        Zero or more characters in that position.
    /// ? (question mark)   Zero or one character in that position.
    /// May fail with IOExceptions
    let getAllFilesInParentsByPattern (pattern:string) (directory:string) = 
        if not (Directory.Exists(directory)) then
            raise (DirectoryNotFoundException("IO.findFileInParentsAndSiblingsByPattern: Directory '" + directory + "' not found for file pattern : '"+pattern+"'"))        
        let rec find (dir:string) =  
            seq{  
                yield! Directory.EnumerateFiles(dir, pattern)
                let par = Directory.GetParent(dir)
                if isNull par then // reached root 
                    () 
                else  
                    yield! find par.FullName      }
        find directory 
   
    /// For non blocking File IO
    module File  = 
        
        /// Reads all bytes from a file in a non blocking manner
        /// This can avoid exceptions such as 'The process cannot access the file'.
        /// It is using FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        let readAllBytesNonBlocking (path:string) = 
            // using a stream allows for non blocking file access
            use stream  = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use mStream = new MemoryStream(int stream.Length)
            stream.CopyTo(mStream)
            let arr = mStream.ToArray()
            stream.Dispose()
            mStream.Dispose()
            arr

        /// Returns a memory stream, the file stream used is closed and disposed.
        /// This can avoid exceptions such as 'The process cannot access the file'.
        /// It is using FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        let getNonBlockingStream (path:string) = 
            // using a stream allows for non blocking file access
            use stream  = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            let mStream = new MemoryStream(int stream.Length)
            stream.CopyTo(mStream)
            stream.Dispose()
            mStream

        /// A curried version of IO.File.WriteAllText(path,content)
        let writeAllText (path:string) (content:string) = 
            IO.File.WriteAllText(path,content)

        /// A curried version of IO.File.WriteAllLines(path,contentLines)
        let writeAllLines (path:string) (contentLines:seq<string>) = 
            IO.File.WriteAllLines(path,contentLines)

    module internal Help = 
        open System.Collections.Generic
        open System.IO

        let maxCharsInString = 5000

        /// If the input string is longer than maxChars + 20 then
        /// it returns the input string trimmed to maxChars, a count of skipped characters and the last 6 characters (all enclosed in double quotes ")
        /// e.g. "abcde[..20 more Chars..]xyz"
        /// Else, if the input string is less than maxChars + 20, it is still returned in full (enclosed in double quotes ").    
        let truncateString (stringToTrim:string) = 
            if isNull stringToTrim then "-null string-" // add too, just in case this gets called externally
            elif stringToTrim.Length <= maxCharsInString + 20 then sprintf "\"%s\""stringToTrim
            else
                let len   = stringToTrim.Length
                let st    = stringToTrim.Substring(0, maxCharsInString)
                let last20 = stringToTrim.Substring(len-21)
                sprintf "\"%s[<< ... %d more chars ... >>]%s\"" st (len - maxCharsInString - 20) last20

        
        let normalizePath path =  //https://stackoverflow.com/questions/1266674
            Path.GetFullPath(Uri(path).LocalPath).ToUpperInvariant()
        
        let uniqueFilesEnsurer = HashSet<string>()
        
    
    /// Reads and Writes with Lock,
    /// Optionally only once after a delay in which it might be called several times
    /// Uses Text.Encoding.UTF8
    /// Writes Exceptions to errorLogger because it is tricky to catch exceptions form an async thread
    type SaveReadWriter (path:string, errorLogger:string->unit)= 
        // same class also exist in FsEx.Wpf , TODO keep in sync!

        let counter = ref 0L // for atomic writing back to file

        let lockObj = new Object()       
        do
            if Help.uniqueFilesEnsurer.Contains(Help.normalizePath path) then
                errorLogger(sprintf "FsEx.Wpf.SaveReadWriter: path '%s' is used already. Reads and Writes are not threadsafe anymore." path)            
            else
                Help.uniqueFilesEnsurer.Add(Help.normalizePath path) |> ignore


        /// Returns true if file to write to exists.
        member this.FileExists = IO.File.Exists(path)

        /// Returns true if file to write to does not exist yet.
        member this.FileDoesNotExists = not <| IO.File.Exists(path)

        /// The full file path
        member this.Path : string  = path

        /// Creates file with text , only if it does not exist yet.
        /// Writes Exceptions to errorLogger.
        /// Returns true if file exists or was successfully created
        member this.CreateFileIfMissing(text) :bool = 
            if IO.File.Exists(path) then
                true
            else
                try
                    IO.File.WriteAllText(path, text,Text.Encoding.UTF8)
                    true

                with e ->
                    errorLogger(sprintf "FsEx.IO.SaveReadWriter.CreateFileIfMissing for path '%s' :\r\n%A" path e)
                    false


        /// Thread Save reading.
        /// Ensures that no writing happens while reading.
        /// Writes Exceptions to errorLogger
        member this.ReadAllText () : option<string> = 
            // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
            lock lockObj (fun () ->
                try Some <| IO.File.ReadAllText(path, Text.Encoding.UTF8)
                with e ->
                    errorLogger(sprintf "FsEx.IO.SaveReadWriter.ReadAllText from path '%s' :\r\n%A" path e)
                    None  )


        /// Thread Save reading.
        /// Ensures that no writing happens while reading.
        /// Writes Exceptions to errorLogger
        member this.ReadAllLines () : option<string[]> = 
            // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
            lock lockObj (fun () ->
                try Some <| IO.File.ReadAllLines(path, Text.Encoding.UTF8)
                with e ->
                    errorLogger(sprintf "FsEx.IO.SaveReadWriter.ReadAllText from '%s' :\r\n%A" path e)
                    None  )


        /// File will be written async and with a Lock.
        /// Ensures that no reading happens while writing.
        /// Writes Exceptions to errorLogger
        member this.WriteAsync (text) = 
            async{
                lock lockObj (fun () -> // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
                    try  IO.File.WriteAllText(path,text, Text.Encoding.UTF8)
                    // try & with is needed because exceptions on thread-pool cannot be caught otherwise !!
                    with ex ->  errorLogger(sprintf "FsEx.IO.SaveWriter.WriteAsync failed with: %A \r\n while writing to %s:\r\n%s" ex path (Help.truncateString text)) 
                    )
                } |> Async.Start

        /// File will be written async and with a Lock.
        /// Ensures that no reading happens while writing.
        /// Writes Exceptions to errorLogger
        member this.WriteAllLinesAsync (texts) = 
            async{
                lock lockObj (fun () -> // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
                    try  IO.File.WriteAllLines(path,texts, Text.Encoding.UTF8)
                    // try & with is needed because exceptions on thread-pool cannot be caught otherwise !!
                    with ex ->  errorLogger(sprintf "FsEx.IO.SaveWriter.WriteAllLinesAsync failed with: %A \r\n while writing to %s:\r\n%A" ex path (Array.truncate 20 texts)) // use %A to trim long text
                    )
                } |> Async.Start

        /// GetString will be called in sync on calling thread, but file will be written async.
        /// Only if after the delay the counter value is the same as before.
        /// That means no more recent calls to this function have been made during the delay.
        /// If other calls to this function have been made then only the last call will be written as file.
        /// Also ensures that no reading happens while writing.
        /// Writes Exceptions to errorLogger
        member this.WriteIfLast ( getText: unit->string, delayMilliSeconds:int) = 
            async{
                let k = Threading.Interlocked.Increment counter
                do! Async.Sleep(delayMilliSeconds) // delay to see if this is the last of many events (otherwise there is a noticeable lag in dragging window around, for example, when saving window position)
                if counter.Value = k then //k > 2L &&   //do not save on startup && only save last event after a delay if there are many save events in a row ( eg from window size change)(ignore first two event from creating window)
                    try
                        let text = getText()
                        this.WriteAsync (text) // this should never fail since exceptions are caught inside
                    with ex ->
                        // try & with is needed because exceptions on thread-pool cannot be caught otherwise !!
                        errorLogger(sprintf "FsEx.IO.SaveWriter.WriteIfLast: getText() for path '%s' failed with: %A" path ex )
                } |> Async.StartImmediate


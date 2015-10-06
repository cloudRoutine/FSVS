namespace FsSnippets

open System
open System.Collections.Generic



[<AutoOpen>]
module Prelude =
    
    let guid = "6B3BBD66-91B9-4039-9FCD-15FB46D25F9F"

    let func x y z = x + y + z

    type XS() =
        let mutable x ,y ,z = 2,43,5
        member val X :int = func x y z 
        
        


    let x = XS() 

// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause


#load "PandocOutput\Internal\Common.fs"
#load "PandocOutput\Internal\TileDoc.fs"

open PandocOutput.Internal.TileDoc




let test01 () = 
    doubleUnderscores (rawtext "Hollow" <+> rawtext "world!") 
        |> renderText 40 


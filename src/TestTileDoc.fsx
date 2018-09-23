// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause


#load "PandocOutput\Internal\Common.fs"
#load "PandocOutput\Internal\TileDoc.fs"

open PandocOutput.Internal.TileDoc



/// Emphasis
let asterisks (source:HDoc) : HDoc = 
    enclose (character '*') (character '*') source

/// Emphasis
let underscores (source:HDoc) : HDoc = 
    enclose (character '_') (character '_') source

/// Strong emphasis
let doubleAsterisks (source:HDoc) : HDoc = 
    enclose (rawtext "**") (rawtext "**") source

/// Strong emphasis
let doubleUnderscores (source:HDoc) : HDoc = 
    enclose (rawtext "__") (rawtext "__") source

let test01 () = 
    doubleUnderscores (rawtext "Hollow" + space + rawtext "world!") 
        |> renderHDoc 40 


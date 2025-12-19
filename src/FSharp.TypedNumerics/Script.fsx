#load "Internals.fs"
#load "Units.fs"
#load "PreludeOperators.fs"
#load "Vector.fs"
#load "Easing.fs"

open System
open FSharp.TypedNumerics
open FSharp.TypedNumerics.Units
open FSharp.TypedNumerics.PreludeOperators
open FSharp.TypedNumerics.Operators

radToDeg Math.PI
radToDeg Math.Tau

radToDeg 3.14<rad>
radToDeg 6.28<rad>

degToRad 180.0<deg>
degToRad 360.0<deg>

#load "Internals.fs"
#load "Units.fs"
#load "PreludeOperators.fs"
#load "Vector.fs"
#load "Easing.fs"

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.TypedNumerics
open FSharp.TypedNumerics.Units
open FSharp.TypedNumerics.PreludeOperators
open FSharp.TypedNumerics.Operators

type [<Measure>] m
type [<Measure>] s

remap 10.0<m> 20.0<m> 100.0<s> 200.0<s> 12.0<m>

remap 0.0 100.0 32.0 212.0 0.0   // evaluates to 32.0
remap 0.0 100.0 32.0 212.0 100.0 // evaluates to 212.0
remap 0.0 100.0 32.0 212.0 50.0  // evaluates to 122.0

// radToDeg Math.PI
// radToDeg Math.Tau
//
// radToDeg 3.14<rad>
// radToDeg 6.28<rad>
//
// degToRad 180.0<deg>
// degToRad 360.0<deg>
//
// let someVec2 = %(1.0, 2.0) // equivalent to Vector2f(1.0, 2.0)
// let someVec3 = %(1.0f<m>, 2.0f<m>, 3.0f<m>) // equivalent to Vector3f32<m>(1.0f<m>, 2.0f<m>, 3.0f<m>)
// let someVec4 = %(10, 1, 20, 2) // equivalent to Vector4i32(10, 1, 20, 2)
// let anotherVec4 %(someVec2, 20, 2) // equivalent to Vector4i32(10, 1, 20, 2)
//
// let someVec2 = 1.0 @@ 2.0 // equivalent to Vector2f(1.0, 2.0)
// let someVec3 = 1.0f<m> @@ 2.0f<m> @@ 3.0f<m> // equivalent to Vector3f32<m>(1.0f<m>, 2.0f<m>, 3.0f<m>)
// let someVec4 = 10 @@ 1 @@ 20 @@ 2 // equivalent to Vector4i32(10, 1, 20, 2)

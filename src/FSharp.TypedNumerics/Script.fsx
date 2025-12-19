#load "Internals.fs"
#load "Units.fs"
#load "PreludeOperators.fs"
#load "Vector.fs"
#load "Easing.fs"

// open System
// open FSharp.Data.UnitSystems.SI.UnitSymbols
// open FSharp.TypedNumerics
// open FSharp.TypedNumerics.Units
// open FSharp.TypedNumerics.PreludeOperators
// open FSharp.TypedNumerics.Operators
//
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

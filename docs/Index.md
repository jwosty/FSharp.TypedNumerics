# FSharp.TypedNumerics

TODO: flesh the docs out more.

In the meantime, here's some samples to try:

```fsharp
open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.TypedNumerics
open FSharp.TypedNumerics.Units
open FSharp.TypedNumerics.PreludeOperators
open FSharp.TypedNumerics.Operators

// The type annotations are not necessary; they can be correctly inferred.

// Start with an int vector2 with meter units
let pos2Int: Vector2i<m> = vec2i (1<m>, 2<m>)
// Convert to float vector in a type-safe, unit-preserving way
let pos2Float: Vector2f<m> = vec2f pos2Int
// Widen to 3 dimensions, adding a Z coordinate
let pos3Float: Vector3f<m> = vec3f (pos2Float, 3<m>)
// Widen to 4 dimensions, adding a W coordinate
let pos4Float: Vector4f32<m> = vec4f32 (pos3Float, 1<m>)
// OR, we can create vectors of any dimension using the % operator:
let anotherPos3Float: Vector3f<m> = %(1.<m>, 1.<m>, 1.<m>)
// OR with the infix @@ operator (I may remove this one though because it is rather noisy):
let yetAnotherPos3Float: Vector3f<m> = 2.<m> @@ 2.<m> @@ 1.<m>
// Now do some math
let dist: float<m> = Vector.length (pos3Float - anotherPos3Float)
let pos3LenSq: float<m^2> = Vector.lengthSquared pos3Float
let pos3FloatScaled: Vector3f<m> = pos3Float * 2.0

```

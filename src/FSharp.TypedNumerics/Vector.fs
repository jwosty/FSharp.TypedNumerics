namespace FSharp.TypedNumerics
open System.ComponentModel
open System.Diagnostics
open System.Runtime.CompilerServices
open FSharp.TypedNumerics.Units
open LanguagePrimitives

open FSharp.TypedNumerics.Internals

open System
open FSharp.TypedNumerics.PreludeOperators

type IIntVector<'``a<'u>``, '``a<'u^2>``> =
    abstract member LengthSquared : '``a<'u^2>``
    abstract member ManhattanLength : '``a<'u>``

type IFracVector<'``a<'u>``, '``a<'u^2>``, '``Vector<'a<1>>``> =
    inherit IIntVector<'``a<'u>``, '``a<'u^2>``>
    
    abstract member Length : '``a<'u>``
    abstract member Normalize : unit -> '``Vector<'a<1>>``

// -- Vector2 types --

type [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector2i32<[<Measure>] 'u> =
    val X : int32<'u>
    val Y : int32<'u>
    
    new(x: int32<'u>, y: int32<'u>) = { X = x; Y = y }
    new(value: int32<'u>) = { X = value; Y = value }
    new(vector: Vector2f32<'u>) = {
        X = int32m vector.X
        Y = int32m vector.Y
    }
    
    static member Zero : Vector2i32<'u> = Vector2i32<'u>(GenericZero<int32<'u>>)
    static member One = Vector2i32<'u>(Int32WithMeasure<'u> 1)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(int32 this.X).ToString format}; Y = %s{(int32 this.Y).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%d{this.X}  %d{this.Y}"
    
    member v.LengthSquared : int32<'u^2> = (v.X * v.X) + (v.Y * v.Y)
    // member v.Length : int32<'u> = sqrt v.LengthSquared
    member v.ManhattanLength : int32<'u> = abs v.X + abs v.Y
    member v.FloatLength : float32<'u> = sqrt (Float32WithMeasure (float32 v.LengthSquared))
    
    interface IIntVector<int32<'u>, int32<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    // member v.Normalize () : Vector2i32<1> =
    //     let len = v.Length
    //     Vector2i32(v.X / len, v.Y / len)
    
    member v1.Dot (v2: Vector2i32<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y)
    
    member v1.Cross (v2: Vector2i32<'u2>) =
          v1.X * v2.Y
        - v1.Y * v2.X
    
    member v1.Add (v2: Vector2i32<'u>) : Vector2i32<'u> =
        Vector2i32<'u>(v1.X + v2.X, v1.Y + v2.Y)
    member v1.Subtract (v2: Vector2i32<'u>) =
        Vector2i32(v1.X - v2.X, v1.Y - v2.Y)
    member v1.Multiply (v2: Vector2i32<'u2>) : Vector2i32<'u*'u2> =
        Vector2i32(v1.X * v2.X, v1.Y * v2.Y)
    member v1.Multiply (scalar: int32<'u2>) : Vector2i32<'u*'u2> =
        Vector2i32(v1.X * scalar, v1.Y * scalar)
    member v1.Divide (v2: Vector2i32<'u2>) : Vector2i32<'u/'u2> =
        Vector2i32(v1.X / v2.X, v1.Y / v2.Y)
    member v1.Divide (scalar: int32<'u2>) : Vector2i32<'u/'u2> =
        Vector2i32(v1.X / scalar, v1.Y / scalar)
    
    static member Dot (v1: Vector2i32<'u>, v2: Vector2i32<'u2>) : int<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector2i32<'u>, v2: Vector2i32<'u2>) : int<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector2i32<'u>, v2: Vector2i32<'u>) : Vector2i32<'u> = v1.Add v2
    static member ( - ) (v1: Vector2i32<'u>, v2: Vector2i32<'u>) : Vector2i32<'u> = v1.Subtract v2
    static member (~- ) (v: Vector2i32<'u>) : Vector2i32<'u> = Vector2i32(-v.X, -v.Y)
    static member ( * ) (v1: Vector2i32<'u>, v2: Vector2i32<'u2>) : Vector2i32<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector2i32<'u>, s: int32<'u2>) : Vector2i32<'u*'u2> = v.Multiply s
    static member ( * ) (s: int32<'u2>, v: Vector2i32<'u>) : Vector2i32<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector2i32<'u>, v2: Vector2i32<'u2>) : Vector2i32<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector2i32<'u>, s: int32<'u2>) : Vector2i32<'u/'u2> = v.Divide s
    static member ( / ) (s: int32<'u2>, v: Vector2i32<'u>) : Vector2i32<'u2/'u> = Vector2i32(s / v.X, s / v.Y)

    static member Abs (v: Vector2i32<'u>) : Vector2i32<'u> = Vector2i32<'u>(abs v.X, abs v.Y)
    
    static member Swizzle (x: int32<'u>, y: int32<'u>) = Vector2i32<'u>(x, y)
    static member Swizzle (x: int32<'u>, y: int32<'u>, z: int32<'u>) = Vector3i32<'u>(x, y, z)

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector2f32<[<Measure>] 'u> =
    val X : float32<'u>
    val Y : float32<'u>
    
    new(x: float32<'u>, y: float32<'u>) = { X = x; Y = y }
    new(value: float32<'u>) = { X = value; Y = value }
    
    static member Zero = Vector2f32<'u>(GenericZero<float32<'u>>)
    static member One = Vector2f32<'u>(Float32WithMeasure<'u> 1.f)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(float32 this.X).ToString format}; Y = %s{(float32 this.Y).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%f{this.X}  %f{this.Y}"
    
    member v.LengthSquared : float32<'u^2> = (v.X * v.X) + (v.Y * v.Y)
    member v.Length : float32<'u> = sqrt v.LengthSquared
    member v.ManhattanLength : float32<'u> = abs v.X + abs v.Y
    
    interface IIntVector<float32<'u>, float32<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    member v.Normalize () : Vector2f32<1> =
        let len = v.Length
        if len = GenericZero then
            Vector2f32<1>.Zero
        else Vector2f32(v.X / len, v.Y / len)
    
    interface IFracVector<float32<'u>, float32<'u^2>, Vector2f32<1>> with
        member this.Length = this.Length
        member this.Normalize () = this.Normalize ()
    
    member v1.Dot (v2: Vector2f32<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y)
    
    member v1.Cross (v2: Vector2f32<'u2>) =
          v1.X * v2.Y
        - v1.Y * v2.X
    
    member v1.Add (v2: Vector2f32<'u>) : Vector2f32<'u> =
        Vector2f32(v1.X + v2.X, v1.Y + v2.Y)
    member v1.Subtract (v2: Vector2f32<'u>) =
        Vector2f32(v1.X - v2.X, v1.Y - v2.Y)
    member v1.Multiply (v2: Vector2f32<'u2>) : Vector2f32<'u*'u2> =
        Vector2f32(v1.X * v2.X, v1.Y * v2.Y)
    member v1.Multiply (scalar: float32<'u2>) : Vector2f32<'u*'u2> =
        Vector2f32(v1.X * scalar, v1.Y * scalar)
    member v1.Divide (v2: Vector2f32<'u2>) : Vector2f32<'u/'u2> =
        Vector2f32(v1.X / v2.X, v1.Y / v2.Y)
    member v1.Divide (scalar: float32<'u2>) : Vector2f32<'u/'u2> =
        Vector2f32(v1.X / scalar, v1.Y / scalar)
    
    static member Dot (v1: Vector2f32<'u>, v2: Vector2f32<'u2>) : float32<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector2f32<'u>, v2: Vector2f32<'u2>) : float32<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector2f32<'u>, v2: Vector2f32<'u>) : Vector2f32<'u> = v1.Add v2
    static member ( - ) (v1: Vector2f32<'u>, v2: Vector2f32<'u>) : Vector2f32<'u> = v1.Subtract v2
    static member (~- ) (v: Vector2f32<'u>) : Vector2f32<'u> = Vector2f32(-v.X, -v.Y)
    static member ( * ) (v1: Vector2f32<'u>, v2: Vector2f32<'u2>) : Vector2f32<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector2f32<'u>, s: float32<'u2>) : Vector2f32<'u*'u2> = v.Multiply s
    static member ( * ) (s: float32<'u2>, v: Vector2f32<'u>) : Vector2f32<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector2f32<'u>, v2: Vector2f32<'u2>) : Vector2f32<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector2f32<'u>, s: float32<'u2>) : Vector2f32<'u/'u2> = v.Divide s
    static member ( / ) (s: float32<'u2>, v: Vector2f32<'u>) : Vector2f32<'u2/'u> = Vector2f32(s / v.X, s / v.Y)
    
    static member Abs (v: Vector2f32<'u>) : Vector2f32<'u> = Vector2f32<'u>(abs v.X, abs v.Y)
    
    static member Round (v: Vector2f32<'u>) : Vector2f32<'u> =
        Vector2f32<'u>(Float32WithMeasure<'u> (round (float32 v.X)), Float32WithMeasure<'u> (round (float32 v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline RoundOp v = Vector2f32<'u>.Round v
    
    static member Floor (v: Vector2f32<'u>) =
        Vector2f32<'u>(Float32WithMeasure<'u> (floor (float32 v.X)), Float32WithMeasure<'u> (floor (float32 v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline FloorOp v = Vector2f32<'u>.Floor v
    
    static member Ceil (v: Vector2f32<'u>) =
        Vector2f32<'u>(Float32WithMeasure<'u> (ceil (float32 v.X)), Float32WithMeasure<'u> (ceil (float32 v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CeilOp v = Vector2f32<'u>.Ceil v
    
    static member Truncate (v: Vector2f32<'u>) =
        Vector2f32<'u>(Float32WithMeasure<'u> (truncate (float32 v.X)), Float32WithMeasure<'u> (truncate (float32 v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TruncateOp v = Vector2f32<'u>.Truncate v
    
    static member Lerp (a: Vector2f32<'u>, b: Vector2f32<'u>, t: float32) = a + (b - a) * t
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float32) = Vector2f32<'u>.Lerp (a, b, t)
    
    static member CreateDirection (direction: float32<rad>) =
        Vector2f32<1>(cos (direction * 1.f</rad>), sin (direction * 1.f</rad>))
    
    static member Swizzle (x: float32<'u>, y: float32<'u>) = Vector2f32<'u>(x, y)
    static member Swizzle (x: float32<'u>, y: float32<'u>, z: float32<'u>) = Vector3f32<'u>(x, y, z)

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector2f<[<Measure>] 'u> =
    val X : float<'u>
    val Y : float<'u>
    
    new(x: float<'u>, y: float<'u>) = { X = x; Y = y }
    new(value: float<'u>) = { X = value; Y = value }
    
    static member Zero = Vector2f<'u>(GenericZero<float<'u>>)
    static member One = Vector2f<'u>(FloatWithMeasure<'u> 1)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(float this.X).ToString format}; Y = %s{(float this.Y).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%f{this.X}  %f{this.Y}"
    
    member v.LengthSquared : float<'u^2> = (v.X * v.X) + (v.Y * v.Y)
    member v.Length : float<'u> = sqrt v.LengthSquared
    member v.ManhattanLength : float<'u> = abs v.X + abs v.Y
    
    interface IIntVector<float<'u>, float<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    member v.Normalize () : Vector2f<1> =
        let len = v.Length
        if len = GenericZero then
            Vector2f<1>.Zero
        else Vector2f(v.X / len, v.Y / len)
    
    interface IFracVector<float<'u>, float<'u^2>, Vector2f<1>> with
        member this.Length = this.Length
        member this.Normalize () = this.Normalize ()
    
    member v1.Dot (v2: Vector2f<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y)
    
    member v1.Cross (v2: Vector2f<'u2>) =
          v1.X * v2.Y
        - v1.Y * v2.X
    
    member v1.Add (v2: Vector2f<'u>) : Vector2f<'u> =
        Vector2f(v1.X + v2.X, v1.Y + v2.Y)
    member v1.Subtract (v2: Vector2f<'u>) =
        Vector2f(v1.X - v2.X, v1.Y - v2.Y)
    member v1.Multiply (v2: Vector2f<'u2>) : Vector2f<'u*'u2> =
        Vector2f(v1.X * v2.X, v1.Y * v2.Y)
    member v1.Multiply (scalar: float<'u2>) : Vector2f<'u*'u2> =
        Vector2f(v1.X * scalar, v1.Y * scalar)
    member v1.Divide (v2: Vector2f<'u2>) : Vector2f<'u/'u2> =
        Vector2f(v1.X / v2.X, v1.Y / v2.Y)
    member v1.Divide (scalar: float<'u2>) : Vector2f<'u/'u2> =
        Vector2f(v1.X / scalar, v1.Y / scalar)
    
    static member Dot (v1: Vector2f<'u>, v2: Vector2f<'u2>) : float<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector2f<'u>, v2: Vector2f<'u2>) : float<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector2f<'u>, v2: Vector2f<'u>) : Vector2f<'u> = v1.Add v2
    static member ( - ) (v1: Vector2f<'u>, v2: Vector2f<'u>) : Vector2f<'u> = v1.Subtract v2
    static member (~- ) (v: Vector2f<'u>) : Vector2f<'u> = Vector2f(-v.X, -v.Y)
    static member ( * ) (v1: Vector2f<'u>, v2: Vector2f<'u2>) : Vector2f<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector2f<'u>, s: float<'u2>) : Vector2f<'u*'u2> = v.Multiply s
    static member ( * ) (s: float<'u2>, v: Vector2f<'u>) : Vector2f<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector2f<'u>, v2: Vector2f<'u2>) : Vector2f<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector2f<'u>, s: float<'u2>) : Vector2f<'u/'u2> = v.Divide s
    static member ( / ) (s: float<'u2>, v: Vector2f<'u>) : Vector2f<'u2/'u> = Vector2f(s / v.X, s / v.Y)
    
    static member Abs (v: Vector2f<'u>) : Vector2f<'u> = Vector2f<'u>(abs v.X, abs v.Y)
    
    static member Round (v: Vector2f<'u>) : Vector2f<'u> =
        Vector2f<'u>(FloatWithMeasure<'u> (round (float v.X)), FloatWithMeasure<'u> (round (float v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline RoundOp v = Vector2f<'u>.Round v
    
    static member Floor (v: Vector2f<'u>) =
        Vector2f<'u>(FloatWithMeasure<'u> (floor (float v.X)), FloatWithMeasure<'u> (floor (float v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline FloorOp v = Vector2f<'u>.Floor v
    
    static member Ceil (v: Vector2f<'u>) =
        Vector2f<'u>(FloatWithMeasure<'u> (ceil (float v.X)), FloatWithMeasure<'u> (ceil (float v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CeilOp v = Vector2f<'u>.Ceil v
    
    static member Truncate (v: Vector2f<'u>) =
        Vector2f<'u>(FloatWithMeasure<'u> (truncate (float v.X)), FloatWithMeasure<'u> (truncate (float v.Y)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TruncateOp v = Vector2f<'u>.Truncate v
    
    static member Lerp (a: Vector2f<'u>, b: Vector2f<'u>, t: float) = a + (b - a) * t
    static member Lerp (a: Vector2f<'u>, b: Vector2f<'u>, t: float32) = a + (b - a) * (floatm t)
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float) = Vector2f<'u>.Lerp (a, b, t)
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float32) = Vector2f<'u>.Lerp (a, b, t)
    
    static member CreateDirection (direction: float<rad>) =
        Vector2f<1>(cos (direction * 1.</rad>), sin (direction * 1.</rad>))
    
    static member Swizzle (x: float<'u>, y: float<'u>) = Vector2f<'u>(x, y)
    static member Swizzle (x: float<'u>, y: float<'u>, z: float<'u>) = Vector3f<'u>(x, y, z)
    
// -- Vector3 types --

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector3i32<[<Measure>] 'u> =
    val X : int<'u>
    val Y : int<'u>
    val Z : int<'u>
    
    new(x: int<'u>, y: int<'u>, z: int<'u>) = { X = x; Y = y; Z = z }
    new(value: int<'u>) = { X = value; Y = value; Z = value }
    new(vector: Vector2i32<'u>, z: int<'u>) = { X = vector.X; Y = vector.Y; Z = z }
    
    static member Zero = Vector3i32<'u>(GenericZero<int<'u>>)
    static member One = Vector3i32<'u>(Int32WithMeasure<'u> 1)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(int32 this.X).ToString format}; Y = %s{(int32 this.Y).ToString format}; Z = %s{(int32 this.Z).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%d{this.X}  %d{this.Y}  %d{this.Z}"
    
    member v.LengthSquared : int<'u^2> = (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z)
    member v.ManhattanLength : int<'u> = abs v.X + abs v.Y
    member v.FloatLength : float32<'u> = sqrt (float32m v.LengthSquared)
    
    interface IIntVector<int32<'u>, int32<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    // member v.Normalize () : Vector3i32<1> =
    //     let len = v.Length
    //     Vector3i32(v.X / len, v.Y / len, v.Z / len)
    
    member v1.Dot (v2: Vector3i32<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)
    
    member v1.Cross (v2: Vector3i32<'u2>) =
        let x = v1.Y * v2.Z - v1.Z * v2.Y
        let y = v1.Z * v2.X - v1.X * v2.Z
        let z = v1.X * v2.Y - v1.Y * v2.X
        Vector3i32(x, y, z)
    
    member v1.Add (v2: Vector3i32<'u>) : Vector3i32<'u> =
        Vector3i32(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)
    member v1.Subtract (v2: Vector3i32<'u>) =
        Vector3i32(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)
    member v1.Multiply (v2: Vector3i32<'u2>) : Vector3i32<'u*'u2> =
        Vector3i32(v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z)
    member v1.Multiply (scalar: int<'u2>) : Vector3i32<'u*'u2> =
        Vector3i32(v1.X * scalar, v1.Y * scalar, v1.Z * scalar)
    member v1.Divide (v2: Vector3i32<'u2>) : Vector3i32<'u/'u2> =
        Vector3i32(v1.X / v2.X, v1.Y / v2.Y, v1.Z / v2.Z)
    member v1.Divide (scalar: int<'u2>) : Vector3i32<'u/'u2> =
        Vector3i32(v1.X / scalar, v1.Y / scalar, v1.Z / scalar)
    
    static member Dot (v1: Vector3i32<'u>, v2: Vector3i32<'u2>) : int<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector3i32<'u>, v2: Vector3i32<'u2>) : Vector3i32<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector3i32<'u>, v2: Vector3i32<'u>) : Vector3i32<'u> = v1.Add v2
    static member ( - ) (v1: Vector3i32<'u>, v2: Vector3i32<'u>) : Vector3i32<'u> = v1.Subtract v2
    static member (~- ) (v: Vector3i32<'u>) : Vector3i32<'u> = Vector3i32(-v.X, -v.Y, -v.Z)
    static member ( * ) (v1: Vector3i32<'u>, v2: Vector3i32<'u2>) : Vector3i32<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector3i32<'u>, s: int<'u2>) : Vector3i32<'u*'u2> = v.Multiply s
    static member ( * ) (s: int<'u2>, v: Vector3i32<'u>) : Vector3i32<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector3i32<'u>, v2: Vector3i32<'u2>) : Vector3i32<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector3i32<'u>, s: int<'u2>) : Vector3i32<'u/'u2> = v.Divide s
    static member ( / ) (s: int<'u2>, v: Vector3i32<'u>) : Vector3i32<'u2/'u> = Vector3i32(s / v.X, s / v.Y, s / v.Z)
    
    static member Abs (v: Vector3i32<'u>) : Vector3i32<'u> = Vector3i32<'u>(abs v.X, abs v.Y, abs v.Z)

    static member Swizzle (x: int32<'u>, y: int32<'u>) = Vector2i32<'u>(x, y)
    static member Swizzle (x: int32<'u>, y: int32<'u>, z: int32<'u>) = Vector3i32<'u>(x, y, z)

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector3f32<[<Measure>] 'u> =
    val X : float32<'u>
    val Y : float32<'u>
    val Z : float32<'u>
    
    new(x: float32<'u>, y: float32<'u>, z: float32<'u>) = { X = x; Y = y; Z = z }
    new(value: float32<'u>) = { X = value; Y = value; Z = value }
    new(vector: Vector2f32<'u>, z: float32<'u>) = { X = vector.X; Y = vector.Y; Z = z }
    
    static member Zero = Vector3f32<'u>(GenericZero<float32<'u>>)
    static member One = Vector3f32<'u>(Float32WithMeasure<'u> 1.f)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(float32 this.X).ToString format}; Y = %s{(float32 this.Y).ToString format}; Z = %s{(float32 this.Z).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%f{this.X}  %f{this.Y}  %f{this.Z}"
    
    member v.LengthSquared : float32<'u^2> = (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z)
    member v.ManhattanLength : float32<'u> = abs v.X + abs v.Y
    member v.Length : float32<'u> = sqrt v.LengthSquared
    
    interface IIntVector<float32<'u>, float32<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    member v.Normalize () : Vector3f32<1> =
        let len = v.Length
        if len = GenericZero then
            Vector3f32<1>.Zero
        else Vector3f32(v.X / len, v.Y / len, v.Z / len)
    
    interface IFracVector<float32<'u>, float32<'u^2>, Vector3f32<1>> with
        member this.Length = this.Length
        member this.Normalize () = this.Normalize ()
    
    member v1.Dot (v2: Vector3f32<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)
    
    member v1.Cross (v2: Vector3f32<'u2>) =
        let x = v1.Y * v2.Z - v1.Z * v2.Y
        let y = v1.Z * v2.X - v1.X * v2.Z
        let z = v1.X * v2.Y - v1.Y * v2.X
        Vector3f32(x, y, z)
    
    member v1.Add (v2: Vector3f32<'u>) : Vector3f32<'u> =
        Vector3f32(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)
    member v1.Subtract (v2: Vector3f32<'u>) =
        Vector3f32(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)
    member v1.Multiply (v2: Vector3f32<'u2>) : Vector3f32<'u*'u2> =
        Vector3f32(v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z)
    member v1.Multiply (scalar: float32<'u2>) : Vector3f32<'u*'u2> =
        Vector3f32(v1.X * scalar, v1.Y * scalar, v1.Z * scalar)
    member v1.Divide (v2: Vector3f32<'u2>) : Vector3f32<'u/'u2> =
        Vector3f32(v1.X / v2.X, v1.Y / v2.Y, v1.Z / v2.Z)
    member v1.Divide (scalar: float32<'u2>) : Vector3f32<'u/'u2> =
        Vector3f32(v1.X / scalar, v1.Y / scalar, v1.Z / scalar)
    
    static member Dot (v1: Vector3f32<'u>, v2: Vector3f32<'u2>) : float32<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector3f32<'u>, v2: Vector3f32<'u2>) : Vector3f32<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector3f32<'u>, v2: Vector3f32<'u>) : Vector3f32<'u> = v1.Add v2
    static member ( - ) (v1: Vector3f32<'u>, v2: Vector3f32<'u>) : Vector3f32<'u> = v1.Subtract v2
    static member (~- ) (v: Vector3f32<'u>) : Vector3f32<'u> = Vector3f32(-v.X, -v.Y, -v.Z)
    static member ( * ) (v1: Vector3f32<'u>, v2: Vector3f32<'u2>) : Vector3f32<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector3f32<'u>, s: float32<'u2>) : Vector3f32<'u*'u2> = v.Multiply s
    static member ( * ) (s: float32<'u2>, v: Vector3f32<'u>) : Vector3f32<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector3f32<'u>, v2: Vector3f32<'u2>) : Vector3f32<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector3f32<'u>, s: float32<'u2>) : Vector3f32<'u/'u2> = v.Divide s
    static member ( / ) (s: float32<'u2>, v: Vector3f32<'u>) : Vector3f32<'u/'u2> = Vector3f32(s / v.X, s / v.Y, s / v.Z)
    
    static member Abs (v: Vector3f32<'u>) : Vector3f32<'u> = Vector3f32<'u>(abs v.X, abs v.Y, abs v.Z)
    
    static member Round (v: Vector3f32<'u>) =
        Vector3f32<'u>(Float32WithMeasure<'u> (round (float32 v.X)), Float32WithMeasure<'u> (round (float32 v.Y)),
                       Float32WithMeasure<'u> (round (float32 v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline RoundOp v = Vector3f32<'u>.Round v
    
    static member Floor (v: Vector3f32<'u>) =
        Vector3f32<'u>(Float32WithMeasure<'u> (floor (float32 v.X)), Float32WithMeasure<'u> (floor (float32 v.Y)),
                       Float32WithMeasure<'u> (floor (float32 v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline FloorOp v = Vector3f32<'u>.Floor v

    static member Ceil (v: Vector3f32<'u>) =
        Vector3f32<'u>(Float32WithMeasure<'u> (ceil (float32 v.X)), Float32WithMeasure<'u> (ceil (float32 v.Y)),
                       Float32WithMeasure<'u> (ceil (float32 v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CeilOp v = Vector3f32<'u>.Ceil v
    
    static member Truncate (v: Vector3f32<'u>) =
        Vector3f32<'u>(Float32WithMeasure<'u> (truncate (float32 v.X)), Float32WithMeasure<'u> (truncate (float32 v.Y)),
                       Float32WithMeasure<'u> (truncate (float32 v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TruncateOp v = Vector3f32<'u>.Truncate v
    
    static member Lerp (a: Vector3f32<'u>, b: Vector3f32<'u>, t: float32) = a + (b - a) * t
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float32) = Vector3f32<'u>.Lerp (a, b, t)
    
    static member CreateDirection (yaw: float32<rad>, pitch: float32<rad>) =
        let yaw, pitch = yaw * 1.f</rad>, pitch * 1.f</rad>
        Vector3f32<1>(
            cos yaw * cos pitch,
            sin yaw * cos pitch,
            sin pitch)
    
    static member Swizzle (x: float32<'u>, y: float32<'u>) = Vector2f32<'u>(x, y)
    static member Swizzle (x: float32<'u>, y: float32<'u>, z: float32<'u>) = Vector3f32<'u>(x, y, z)

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector3f<[<Measure>] 'u> =
    val X : float<'u>
    val Y : float<'u>
    val Z : float<'u>
    
    new(x: float<'u>, y: float<'u>, z: float<'u>) = { X = x; Y = y; Z = z }
    new(value: float<'u>) = { X = value; Y = value; Z = value }
    new(vector: Vector2f<'u>, z: float<'u>) = { X = vector.X; Y = vector.Y; Z = z }
    
    static member Zero = Vector3f<'u>(GenericZero<float<'u>>)
    static member One = Vector3f<'u>(FloatWithMeasure<'u> 1)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(float this.X).ToString format}; Y = %s{(float this.Y).ToString format}; Z = %s{(float this.Z).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%f{this.X}  %f{this.Y}  %f{this.Z}"
    
    member v.LengthSquared : float<'u^2> = (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z)
    member v.Length : float<'u> = sqrt v.LengthSquared
    member v.ManhattanLength : float<'u> = abs v.X + abs v.Y
    
    interface IIntVector<float<'u>, float<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    member v.Normalize () : Vector3f<1> =
        let len = v.Length
        if len = GenericZero then
            Vector3f<1>.Zero
        else Vector3f(v.X / len, v.Y / len, v.Z / len)
    
    interface IFracVector<float<'u>, float<'u^2>, Vector3f<1>> with
        member this.Length = this.Length
        member this.Normalize () = this.Normalize ()
    
    member v1.Dot (v2: Vector3f<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)
    
    member v1.Cross (v2: Vector3f<'u2>) =
        let x = v1.Y * v2.Z - v1.Z * v2.Y
        let y = v1.Z * v2.X - v1.X * v2.Z
        let z = v1.X * v2.Y - v1.Y * v2.X
        Vector3f(x, y, z)
    
    member v1.Add (v2: Vector3f<'u>) : Vector3f<'u> =
        Vector3f(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)
    member v1.Subtract (v2: Vector3f<'u>) =
        Vector3f(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)
    member v1.Multiply (v2: Vector3f<'u2>) : Vector3f<'u*'u2> =
        Vector3f(v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z)
    member v1.Multiply (scalar: float<'u2>) : Vector3f<'u*'u2> =
        Vector3f(v1.X * scalar, v1.Y * scalar, v1.Z * scalar)
    member v1.Divide (v2: Vector3f<'u2>) : Vector3f<'u/'u2> =
        Vector3f(v1.X / v2.X, v1.Y / v2.Y, v1.Z / v2.Z)
    member v1.Divide (scalar: float<'u2>) : Vector3f<'u/'u2> =
        Vector3f(v1.X / scalar, v1.Y / scalar, v1.Z / scalar)
    
    static member Dot (v1: Vector3f<'u>, v2: Vector3f<'u2>) : float<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector3f<'u>, v2: Vector3f<'u2>) : Vector3f<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector3f<'u>, v2: Vector3f<'u>) : Vector3f<'u> = v1.Add v2
    static member ( - ) (v1: Vector3f<'u>, v2: Vector3f<'u>) : Vector3f<'u> = v1.Subtract v2
    static member (~- ) (v: Vector3f<'u>) : Vector3f<'u> = Vector3f(-v.X, -v.Y, -v.Z)
    static member ( * ) (v1: Vector3f<'u>, v2: Vector3f<'u2>) : Vector3f<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector3f<'u>, s: float<'u2>) : Vector3f<'u*'u2> = v.Multiply s
    static member ( * ) (s: float<'u2>, v: Vector3f<'u>) : Vector3f<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector3f<'u>, v2: Vector3f<'u2>) : Vector3f<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector3f<'u>, s: float<'u2>) : Vector3f<'u/'u2> = v.Divide s
    static member ( / ) (s: float<'u2>, v: Vector3f<'u>) : Vector3f<'u/'u2> = Vector3f(s / v.X, s / v.Y, s / v.Z)
    
    static member Abs (v: Vector3f<'u>) : Vector3f<'u> = Vector3f<'u>(abs v.X, abs v.Y, abs v.Z)
    
    static member Round (v: Vector3f<'u>) =
        Vector3f<'u>(FloatWithMeasure<'u> (round (float v.X)), FloatWithMeasure<'u> (round (float v.Y)),
                       FloatWithMeasure<'u> (round (float v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline RoundOp v = Vector3f<'u>.Round v
    
    static member Floor (v: Vector3f<'u>) =
        Vector3f<'u>(FloatWithMeasure<'u> (floor (float v.X)), FloatWithMeasure<'u> (floor (float v.Y)),
                       FloatWithMeasure<'u> (floor (float v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline FloorOp v = Vector3f<'u>.Floor v

    static member Ceil (v: Vector3f<'u>) =
        Vector3f<'u>(FloatWithMeasure<'u> (ceil (float v.X)), FloatWithMeasure<'u> (ceil (float v.Y)),
                       FloatWithMeasure<'u> (ceil (float v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CeilOp v = Vector3f<'u>.Ceil v
    
    static member Truncate (v: Vector3f<'u>) =
        Vector3f<'u>(FloatWithMeasure<'u> (truncate (float v.X)), FloatWithMeasure<'u> (truncate (float v.Y)),
                       FloatWithMeasure<'u> (truncate (float v.Z)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TruncateOp v = Vector3f<'u>.Truncate v
    
    static member Lerp (a: Vector3f<'u>, b: Vector3f<'u>, t: float) = a + (b - a) * t
    static member Lerp (a: Vector3f<'u>, b: Vector3f<'u>, t: float32) = a + (b - a) * (floatm t)
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float) = Vector3f<'u>.Lerp (a, b, t)
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float32) = Vector3f<'u>.Lerp (a, b, t)
    
    static member CreateDirection (yaw: float<rad>, pitch: float<rad>) =
        let yaw, pitch = yaw * 1.</rad>, pitch * 1.</rad>
        Vector3f<1>(
            cos yaw * cos pitch,
            sin yaw * cos pitch,
            sin pitch)
    
    static member Swizzle (x: float<'u>, y: float<'u>) = Vector2f<'u>(x, y)
    static member Swizzle (x: float<'u>, y: float<'u>, z: float<'u>) = Vector3f<'u>(x, y, z)


// -- Vector4 types --

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector4i32<[<Measure>] 'u> =
    val X : int<'u>
    val Y : int<'u>
    val Z : int<'u>
    val W : int<'u>
    
    new(x: int<'u>, y: int<'u>, z: int<'u>, w: int<'u>) = { X = x; Y = y; Z = z; W = w }
    new(value: int<'u>) = { X = value; Y = value; Z = value; W = value }
    new(vector: Vector2i32<'u>, z: int<'u>, w: int<'u>) = { X = vector.X; Y = vector.Y; Z = z; W = w }
    new(vector: Vector3i32<'u>, w: int<'u>) = { X = vector.X; Y = vector.Y; Z = vector.Z; W = w }
    
    static member Zero = Vector4i32<'u>(GenericZero<int<'u>>)
    static member One = Vector4i32<'u>(Int32WithMeasure<'u> 1)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(int32 this.X).ToString format}; Y = %s{(int32 this.Y).ToString format}; Z = %s{(int32 this.Z).ToString format}; W = %s{(int32 this.W).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%d{this.X}  %d{this.Y}  %d{this.Z}  %d{this.W}"
    
    member v.LengthSquared : int<'u^2> = (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z) + (v.W * v.W)
    member v.ManhattanLength : int<'u> = abs v.X + abs v.Y
    member v.FloatLength : float32<'u> = sqrt (float32m v.LengthSquared)
    
    interface IIntVector<int32<'u>, int32<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    // member v.Normalize () : Vector4i32<1> =
    //     let len = v.Length
    //     Vector4i32(v.X / len, v.Y / len, v.Z / len)
    
    member v1.Dot (v2: Vector4i32<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z) + (v1.W * v2.W)
    
    member v1.Cross (v2: Vector4i32<'u2>) =
        Vector4i32(
            v1.Y * v2.Z - v1.Z * v2.Y,
            v1.Z * v2.X - v1.X * v2.Z,
            v1.X * v2.Y - v1.Y * v2.X,
            v1.W * v2.W
        )
    
    member v1.Add (v2: Vector4i32<'u>) : Vector4i32<'u> =
        Vector4i32(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z, v1.W + v2.W)
    member v1.Subtract (v2: Vector4i32<'u>) =
        Vector4i32(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W)
    member v1.Multiply (v2: Vector4i32<'u2>) : Vector4i32<'u*'u2> =
        Vector4i32(v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z, v1.W * v2.W)
    member v1.Multiply (scalar: int<'u2>) : Vector4i32<'u*'u2> =
        Vector4i32(v1.X * scalar, v1.Y * scalar, v1.Z * scalar, v1.W * scalar)
    member v1.Divide (v2: Vector4i32<'u2>) : Vector4i32<'u/'u2> =
        Vector4i32(v1.X / v2.X, v1.Y / v2.Y, v1.Z / v2.Z, v1.W / v2.W)
    member v1.Divide (scalar: int<'u2>) : Vector4i32<'u/'u2> =
        Vector4i32(v1.X / scalar, v1.Y / scalar, v1.Z / scalar, v1.W / scalar)
    
    static member Dot (v1: Vector4i32<'u>, v2: Vector4i32<'u2>) : int<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector4i32<'u>, v2: Vector4i32<'u2>) : Vector4i32<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector4i32<'u>, v2: Vector4i32<'u>) : Vector4i32<'u> = v1.Add v2
    static member ( - ) (v1: Vector4i32<'u>, v2: Vector4i32<'u>) : Vector4i32<'u> = v1.Subtract v2
    static member (~- ) (v: Vector4i32<'u>) : Vector4i32<'u> = Vector4i32(-v.X, -v.Y, -v.Z, -v.W)
    static member ( * ) (v1: Vector4i32<'u>, v2: Vector4i32<'u2>) : Vector4i32<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector4i32<'u>, s: int<'u2>) : Vector4i32<'u*'u2> = v.Multiply s
    static member ( * ) (s: int<'u2>, v: Vector4i32<'u>) : Vector4i32<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector4i32<'u>, v2: Vector4i32<'u2>) : Vector4i32<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector4i32<'u>, s: int<'u2>) : Vector4i32<'u/'u2> = v.Divide s
    static member ( / ) (s: int<'u2>, v: Vector4i32<'u>) : Vector4i32<'u/'u2> =
        Vector4i32(s / v.X, s / v.Y, s / v.Z, s / v.W)

    static member Abs (v: Vector4i32<'u>) : Vector4i32<'u> = Vector4i32<'u>(abs v.X, abs v.Y, abs v.Z, abs v.W)
    
    static member Swizzle (x: int32<'u>, y: int32<'u>) = Vector2i32<'u>(x, y)
    static member Swizzle (x: int32<'u>, y: int32<'u>, z: int32<'u>) = Vector3i32<'u>(x, y, z)
    static member Swizzle (x: int32<'u>, y: int32<'u>, z: int32<'u>, w: int32<'u>) = Vector4i32<'u>(x, y, z, w)

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector4f32<[<Measure>] 'u> =
    val X : float32<'u>
    val Y : float32<'u>
    val Z : float32<'u>
    val W : float32<'u>
    
    new(x: float32<'u>, y: float32<'u>, z: float32<'u>, w: float32<'u>) = { X = x; Y = y; Z = z; W = w }
    new(value: float32<'u>) = { X = value; Y = value; Z = value; W = value }
    new(vector: Vector2f32<'u>, z: float32<'u>, w: float32<'u>) = { X = vector.X; Y = vector.Y; Z = z; W = w }
    new(vector: Vector3f32<'u>, w: float32<'u>) = { X = vector.X; Y = vector.Y; Z = vector.Z; W = w }
    
    static member Zero = Vector4f32<'u>(GenericZero<float32<'u>>)
    static member One = Vector4f32<'u>(Float32WithMeasure<'u> 1.f)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(float32 this.X).ToString format}; Y = %s{(float32 this.Y).ToString format}; Z = %s{(float32 this.Z).ToString format} }}; W = %s{(float32 this.W).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%f{this.X}  %f{this.Y}  %f{this.Z}  %f{this.W}"
    
    member v.LengthSquared : float32<'u^2> = (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z) + (v.W * v.W)
    member v.Length : float32<'u> = sqrt v.LengthSquared
    member v.ManhattanLength = abs v.X + abs v.Y
    
    interface IIntVector<float32<'u>, float32<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    member v.Normalize () : Vector4f32<1> =
        let len = v.Length
        if len = GenericZero then
            Vector4f32<1>.Zero
        else Vector4f32(v.X / len, v.Y / len, v.Z / len, v.W / len)
    
    interface IFracVector<float32<'u>, float32<'u^2>, Vector4f32<1>> with
        member this.Length = this.Length
        member this.Normalize () = this.Normalize ()
    
    member v1.Dot (v2: Vector4f32<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z) + (v1.W * v2.W)
    

    member v1.Cross (v2: Vector4f32<'u2>) =
        Vector4f32(
            v1.Y * v2.Z - v1.Z * v2.Y,
            v1.Z * v2.X - v1.X * v2.Z,
            v1.X * v2.Y - v1.Y * v2.X,
            v1.W * v2.W
        )
    
    member v1.Add (v2: Vector4f32<'u>) : Vector4f32<'u> =
        Vector4f32(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z, v1.W + v2.W)
    member v1.Subtract (v2: Vector4f32<'u>) =
        Vector4f32(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W)
    member v1.Multiply (v2: Vector4f32<'u2>) : Vector4f32<'u*'u2> =
        Vector4f32(v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z, v1.W * v2.W)
    member v1.Multiply (scalar: float32<'u2>) : Vector4f32<'u*'u2> =
        Vector4f32(v1.X * scalar, v1.Y * scalar, v1.Z * scalar, v1.W * scalar)
    member v1.Divide (v2: Vector4f32<'u2>) : Vector4f32<'u/'u2> =
        Vector4f32(v1.X / v2.X, v1.Y / v2.Y, v1.Z / v2.Z, v1.W / v2.W)
    member v1.Divide (scalar: float32<'u2>) : Vector4f32<'u/'u2> =
        Vector4f32(v1.X / scalar, v1.Y / scalar, v1.Z / scalar, v1.W / scalar)
    
    static member Dot (v1: Vector4f32<'u>, v2: Vector4f32<'u2>) : float32<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector4f32<'u>, v2: Vector4f32<'u2>) : Vector4f32<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector4f32<'u>, v2: Vector4f32<'u>) : Vector4f32<'u> = v1.Add v2
    static member ( - ) (v1: Vector4f32<'u>, v2: Vector4f32<'u>) : Vector4f32<'u> = v1.Subtract v2
    static member (~- ) (v: Vector4f32<'u>) : Vector4f32<'u> = Vector4f32(-v.X, -v.Y, -v.Z, -v.W)
    static member ( * ) (v1: Vector4f32<'u>, v2: Vector4f32<'u2>) : Vector4f32<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector4f32<'u>, s: float32<'u2>) : Vector4f32<'u*'u2> = v.Multiply s
    static member ( * ) (s: float32<'u2>, v: Vector4f32<'u>) : Vector4f32<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector4f32<'u>, v2: Vector4f32<'u2>) : Vector4f32<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector4f32<'u>, s: float32<'u2>) : Vector4f32<'u/'u2> = v.Divide s
    static member ( / ) (s: float32<'u2>, v: Vector4f32<'u>) : Vector4f32<'u/'u2> =
        Vector4f32(s / v.X, s / v.Y, s / v.Z, s / v.W)
    
    static member Abs (v: Vector4f32<'u>) : Vector4f32<'u> = Vector4f32<'u>(abs v.X, abs v.Y, abs v.Z, abs v.W)
    
    static member Round (v: Vector4f32<'u>) =
        Vector4f32<'u>(Float32WithMeasure<'u> (round (float32 v.X)), Float32WithMeasure<'u> (round (float32 v.Y)),
                       Float32WithMeasure<'u> (round (float32 v.Z)), Float32WithMeasure<'u> (round (float32 v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline RoundOp v = Vector4f32<'u>.Round v
    
    static member Floor (v: Vector4f32<'u>) =
        Vector4f32<'u>(Float32WithMeasure<'u> (floor (float32 v.X)), Float32WithMeasure<'u> (floor (float32 v.Y)),
                       Float32WithMeasure<'u> (floor (float32 v.Z)), Float32WithMeasure<'u> (floor (float32 v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline FloorOp v = Vector4f32<'u>.Floor v

    static member Ceil (v: Vector4f32<'u>) =
        Vector4f32<'u>(Float32WithMeasure<'u> (ceil (float32 v.X)), Float32WithMeasure<'u> (ceil (float32 v.Y)),
                       Float32WithMeasure<'u> (ceil (float32 v.Z)), Float32WithMeasure<'u> (ceil (float32 v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CeilOp v = Vector4f32<'u>.Ceil v
    
    static member Truncate (v: Vector4f32<'u>) =
        Vector4f32<'u>(Float32WithMeasure<'u> (truncate (float32 v.X)), Float32WithMeasure<'u> (truncate (float32 v.Y)),
                       Float32WithMeasure<'u> (truncate (float32 v.Z)), Float32WithMeasure<'u> (truncate (float32 v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TruncateOp v = Vector4f32<'u>.Truncate v
    
    static member Lerp (a: Vector4f32<'u>, b: Vector4f32<'u>, t: float32) = a + (b - a) * t
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float32) = Vector4f32<'u>.Lerp (a, b, t)
    
    static member CreateDirection (yaw: float32<rad>, pitch: float32<rad>) =
        let yaw, pitch = yaw * 1.f</rad>, pitch * 1.f</rad>
        Vector4f32<1>(
            cos yaw * cos pitch,
            sin yaw * cos pitch,
            sin pitch,
            0.f)
    
    static member Swizzle (x: float32<'u>, y: float32<'u>) = Vector2f32<'u>(x, y)
    static member Swizzle (x: float32<'u>, y: float32<'u>, z: float32<'u>) = Vector3f32<'u>(x, y, z)
    static member Swizzle (x: float32<'u>, y: float32<'u>, z: float32<'u>, w: float32<'u>) = Vector4f32<'u>(x, y, z, w)

and [<IsReadOnly; Struct>] [<DebuggerDisplay("{DebugDisplayString,nq}")>] Vector4f<[<Measure>] 'u> =
    val X : float<'u>
    val Y : float<'u>
    val Z : float<'u>
    val W : float<'u>
    
    new(x: float<'u>, y: float<'u>, z: float<'u>, w: float<'u>) = { X = x; Y = y; Z = z; W = w }
    new(value: float<'u>) = { X = value; Y = value; Z = value; W = value }
    new(vector: Vector2f<'u>, z: float<'u>, w: float<'u>) = { X = vector.X; Y = vector.Y; Z = z; W = w }
    new(vector: Vector3f<'u>, w: float<'u>) = { X = vector.X; Y = vector.Y; Z = vector.Z; W = w }
    
    static member Zero = Vector4f<'u>(GenericZero<float<'u>>)
    static member One = Vector4f<'u>(FloatWithMeasure<'u> 1)
    
    member this.ToString (format: string) =
        $"{{ X = %s{(float this.X).ToString format}; Y = %s{(float this.Y).ToString format}; Z = %s{(float this.Z).ToString format} }}; W = %s{(float this.W).ToString format} }}"
    
    override this.ToString () = this.ToString null
    
    member internal this.DebugDisplayString = $"%f{this.X}  %f{this.Y}  %f{this.Z}  %f{this.W}"
    
    member v.LengthSquared : float<'u^2> = (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z) + (v.W * v.W)
    member v.Length : float<'u> = sqrt v.LengthSquared
    member v.ManhattanLength = abs v.X + abs v.Y
    
    interface IIntVector<float<'u>, float<'u^2>> with
        member this.LengthSquared = this.LengthSquared
        member this.ManhattanLength = this.ManhattanLength
    
    member v.Normalize () : Vector4f<1> =
        let len = v.Length
        if len = GenericZero then
            Vector4f<1>.Zero
        else Vector4f(v.X / len, v.Y / len, v.Z / len, v.W / len)
    
    interface IFracVector<float<'u>, float<'u^2>, Vector4f<1>> with
        member this.Length = this.Length
        member this.Normalize () = this.Normalize ()
    
    member v1.Dot (v2: Vector4f<'u2>) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z) + (v1.W * v2.W)
    
    member v1.Cross (v2: Vector4f<'u2>) =
        Vector4f(
            v1.Y * v2.Z - v1.Z * v2.Y,
            v1.Z * v2.X - v1.X * v2.Z,
            v1.X * v2.Y - v1.Y * v2.X,
            v1.W * v2.W
        )
    
    member v1.Add (v2: Vector4f<'u>) : Vector4f<'u> =
        Vector4f(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z, v1.W + v2.W)
    member v1.Subtract (v2: Vector4f<'u>) =
        Vector4f(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W)
    member v1.Multiply (v2: Vector4f<'u2>) : Vector4f<'u*'u2> =
        Vector4f(v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z, v1.W * v2.W)
    member v1.Multiply (scalar: float<'u2>) : Vector4f<'u*'u2> =
        Vector4f(v1.X * scalar, v1.Y * scalar, v1.Z * scalar, v1.W * scalar)
    member v1.Divide (v2: Vector4f<'u2>) : Vector4f<'u/'u2> =
        Vector4f(v1.X / v2.X, v1.Y / v2.Y, v1.Z / v2.Z, v1.W / v2.W)
    member v1.Divide (scalar: float<'u2>) : Vector4f<'u/'u2> =
        Vector4f(v1.X / scalar, v1.Y / scalar, v1.Z / scalar, v1.W / scalar)
    
    static member Dot (v1: Vector4f<'u>, v2: Vector4f<'u2>) : float<'u 'u2> = v1.Dot v2
    
    static member Cross (v1: Vector4f<'u>, v2: Vector4f<'u2>) : Vector4f<'u 'u2> = v1.Cross v2
    
    static member ( + ) (v1: Vector4f<'u>, v2: Vector4f<'u>) : Vector4f<'u> = v1.Add v2
    static member ( - ) (v1: Vector4f<'u>, v2: Vector4f<'u>) : Vector4f<'u> = v1.Subtract v2
    static member (~- ) (v: Vector4f<'u>) : Vector4f<'u> = Vector4f(-v.X, -v.Y, -v.Z, -v.W)
    static member ( * ) (v1: Vector4f<'u>, v2: Vector4f<'u2>) : Vector4f<'u*'u2> = v1.Multiply v2
    static member ( * ) (v: Vector4f<'u>, s: float<'u2>) : Vector4f<'u*'u2> = v.Multiply s
    static member ( / ) (v1: Vector4f<'u>, v2: Vector4f<'u2>) : Vector4f<'u/'u2> = v1.Divide v2
    static member ( / ) (v: Vector4f<'u>, s: float<'u2>) : Vector4f<'u/'u2> = v.Divide s
    
    static member Abs (v: Vector4f<'u>) : Vector4f<'u> = Vector4f<'u>(abs v.X, abs v.Y, abs v.Z, abs v.W)
    
    static member Round (v: Vector4f<'u>) =
        Vector4f<'u>(FloatWithMeasure<'u> (round (float v.X)), FloatWithMeasure<'u> (round (float v.Y)),
                     FloatWithMeasure<'u> (round (float v.Z)), FloatWithMeasure<'u> (round (float v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline RoundOp v = Vector4f<'u>.Round v
    
    static member Floor (v: Vector4f<'u>) =
        Vector4f<'u>(FloatWithMeasure<'u> (floor (float v.X)), FloatWithMeasure<'u> (floor (float v.Y)),
                       FloatWithMeasure<'u> (floor (float v.Z)), FloatWithMeasure<'u> (floor (float v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline FloorOp v = Vector4f<'u>.Floor v

    static member Ceil (v: Vector4f<'u>) =
        Vector4f<'u>(FloatWithMeasure<'u> (ceil (float v.X)), FloatWithMeasure<'u> (ceil (float v.Y)),
                       FloatWithMeasure<'u> (ceil (float v.Z)), FloatWithMeasure<'u> (ceil (float v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CeilOp v = Vector4f<'u>.Ceil v
    
    static member Truncate (v: Vector4f<'u>) =
        Vector4f<'u>(FloatWithMeasure<'u> (truncate (float v.X)), FloatWithMeasure<'u> (truncate (float v.Y)),
                       FloatWithMeasure<'u> (truncate (float v.Z)), FloatWithMeasure<'u> (truncate (float v.W)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TruncateOp v = Vector4f<'u>.Truncate v
    
    static member Lerp (a: Vector4f<'u>, b: Vector4f<'u>, t: float) = a + (b - a) * t
    static member Lerp (a: Vector4f<'u>, b: Vector4f<'u>, t: float32) = a + (b - a) * (floatm t)
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float) = Vector4f<'u>.Lerp (a, b, t)
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LerpOp (a, b, t: float32) = Vector4f<'u>.Lerp (a, b, t)
    
    static member CreateDirection (yaw: float<rad>, pitch: float<rad>) =
        let yaw, pitch = yaw * 1.</rad>, pitch * 1.</rad>
        Vector4f<1>(
            cos yaw * cos pitch,
            sin yaw * cos pitch,
            sin pitch,
            0.)
    
    static member Swizzle (x: float<'u>, y: float<'u>) = Vector2f<'u>(x, y)
    static member Swizzle (x: float<'u>, y: float<'u>, z: float<'u>) = Vector3f<'u>(x, y, z)
    static member Swizzle (x: float<'u>, y: float<'u>, z: float<'u>, w: float<'u>) = Vector4f<'u>(x, y, z, w)
    


type Vector2i<[<Measure>] 'u> = Vector2i32<'u>
type Vector3i<[<Measure>] 'u> = Vector3i32<'u>
type Vector4i<[<Measure>] 'u> = Vector4i32<'u>

[<Struct; EditorBrowsable(EditorBrowsableState.Never)>]
type Ops =
    // -- Vector2 --
    static member inline Vector2i32 ((x: ^a, y: ^a)) = Vector2i32<'u>(int32m x, int32m y)
    static member inline Vector2i32 (value: int32<'u>) = Vector2i32<'u>(value, value)
    static member inline Vector2i32 (vector: Vector2i32<'u>) = vector
    static member inline Vector2i32 (vector: Vector2f32<'u>) = Vector2i32<'u>(int32m vector.X, int32m vector.Y)
    static member inline Vector2i32 (vector: Vector2f<'u>) = Vector2i32<'u>(int32m vector.X, int32m vector.Y)
    
    static member inline Vector2f32 ((x: ^a, y: ^a)) = Vector2f32<'u>(float32m x, float32m y)
    static member inline Vector2f32 (value: float32<'u>) = Vector2f32<'u>(float32m value, float32m value)
    static member inline Vector2f32 (vector: Vector2i32<'u>) = Vector2f32<'u>(float32m vector.X, float32m vector.Y)
    static member inline Vector2f32 (vector: Vector2f32<'u>) = vector
    static member inline Vector2f32 (vector: Vector2f<'u>) = Vector2f32<'u>(float32m vector.X, float32m vector.Y)
    
    static member inline Vector2f ((x: ^a, y: ^a)) = Vector2f<'u>(floatm x, floatm y)
    static member inline Vector2f (value: float<'u>) = Vector2f<'u>(floatm value, floatm value)
    static member inline Vector2f (vector: Vector2i32<'u>) = Vector2f<'u>(floatm vector.X, floatm vector.Y)
    static member inline Vector2f (vector: Vector2f32<'u>) = Vector2f<'u>(floatm vector.X, floatm vector.Y)
    static member inline Vector2f (vector: Vector2f<'u>) = vector
    
    static member inline Vector2 ((x: int<'u>, y: int<'u>)) = Vector2i<'u>(x, y)
    static member inline Vector2 ((x: float32<'u>, y: float32<'u>)) = Vector2f32<'u>(x, y)
    static member inline Vector2 ((x: float<'u>, y: float<'u>)) = Vector2f<'u>(x, y)
    static member inline Vector2 (value: int<'u>) = Vector2i<'u>(value)
    static member inline Vector2 (value: float32<'u>) = Vector2f32<'u>(value)
    static member inline Vector2 (value: float<'u>) = Vector2f<'u>(value)
    static member inline Vector2 (vector: Vector2i<'u>) = vector
    static member inline Vector2 (vector: Vector2f32<'u>) = vector
    static member inline Vector2 (vector: Vector2f<'u>) = vector
    
    // -- Vector3 --
    static member inline Vector3i32 ((x: ^a, y: ^a, z: ^a)) = Vector3i32<'u>(int32m x, int32m y, int32m z)
    static member inline Vector3i32 (value: int32<'u>) = Vector3i32<'u>(value, value, value)
    static member inline Vector3i32 ((v: Vector2i32<'u>, z: ^a)) = Vector3i32<'u>(v.X, v.Y, int32m z)
    static member inline Vector3i32 (vector: Vector3i32<'u>) = vector
    static member inline Vector3i32 (vector: Vector3f32<'u>) =
        Vector3i32<'u>(int32m vector.X, int32m vector.Y, int32m vector.Z)
    static member inline Vector3i32 (vector: Vector3f<'u>) =
        Vector3i32<'u>(int32m vector.X, int32m vector.Y, int32m vector.Z)
    
    static member inline Vector3f32 ((x: ^a, y: ^a, z: ^a)) =
        Vector3f32<'u>(float32m x, float32m y, float32m z)
    static member inline Vector3f32 (value: float32<'u>) =
        Vector3f32<'u>(float32m value, float32m value, float32m value)
    static member inline Vector3f32 (vector: Vector3i32<'u>) =
        Vector3f32(float32m vector.X, float32m vector.Y, float32m vector.Z)
    static member inline Vector3f32 (vector: Vector3f32<'u>) = vector
    static member inline Vector3f32 (vector: Vector3f<'u>) =
        Vector3f32<'u>(float32m vector.X, float32m vector.Y, float32m vector.Z)
    static member inline Vector3f32 ((v: Vector2f32<'u>, z: ^a)) = Vector3f32<'u>(v.X, v.Y, float32m z)
    static member inline Vector3f32 ((v: Vector2f<'u>, z: ^a)) = Vector3f32<'u>(float32m v.X, float32m v.Y, float32m z)
    
    static member inline Vector3f ((x: ^a, y: ^a, z: ^a)) = Vector3f<'u>(floatm x, floatm y, floatm z)
    static member inline Vector3f (value: float32<'u>) = Vector3f<'u>(floatm value, floatm value, floatm value)
    static member inline Vector3f (value: float<'u>) = Vector3f<'u>(value, value, value)
    static member inline Vector3f (value: int32<'u>) = Vector3f<'u>(floatm value, floatm value, floatm value)
    static member inline Vector3f ((v: Vector2i32<'u>, z: ^a)) = Vector3f<'u>(floatm v.X, floatm v.Y, floatm z)
    static member inline Vector3f ((v: Vector2f32<'u>, z: ^a)) = Vector3f<'u>(floatm v.X, floatm v.Y, floatm z)
    static member inline Vector3f ((v: Vector2f<'u>, z: ^a)) = Vector3f<'u>(v.X, v.Y, floatm z)
    static member inline Vector3f (vector: Vector3i32<'u>) =
        Vector3f(floatm vector.X, floatm vector.Y, floatm vector.Z)
    static member inline Vector3f (vector: Vector3f32<'u>) =
        Vector3f(floatm vector.X, floatm vector.Y, floatm vector.Z)
    static member inline Vector3f (vector: Vector3f<'u>) = vector
    
    static member inline Vector3 ((x: int<'u>, y: int<'u>, z: int<'u>)) = Vector3i32<'u>(x, y, z)
    static member inline Vector3 ((x: float32<'u>, y: float32<'u>, z: float32<'u>)) = Vector3f32<'u>(x, y, z)
    static member inline Vector3 ((x: float<'u>, y: float<'u>, z: float<'u>)) = Vector3f<'u>(x, y, z)
    static member inline Vector3 (value: int<'u>) = Vector3i<'u>(value)
    static member inline Vector3 (value: float32<'u>) = Vector3f32<'u>(value)
    static member inline Vector3 (value: float<'u>) = Vector3f<'u>(value)
    static member inline Vector3 ((vector: Vector2i32<'u>, z: ^a)) = Vector3i32<'u>(vector.X, vector.Y, int32m z)
    static member inline Vector3 ((vector: Vector2f32<'u>, z: ^a)) = Vector3f32<'u>(vector.X, vector.Y, float32m z)
    static member inline Vector3 ((vector: Vector2f<'u>, z: ^a)) = Vector3f<'u>(vector.X, vector.Y, floatm z)
    static member inline Vector3 (vector: Vector3i32<'u>) = vector
    static member inline Vector3 (vector: Vector3f32<'u>) = vector
    static member inline Vector3 (vector: Vector3f<'u>) = vector
    
    

    // -- Vector4 --
    static member inline Vector4i32 ((x: ^a, y: ^a, z: ^a, w: ^a)) = Vector4i32<'u>(int32m x, int32m y, int32m z, int32m w)
    static member inline Vector4i32 (value: int32<'u>) = Vector4i32<'u>(value, value, value, value)
    static member inline Vector4i32 ((v: Vector2i32<'u>, z: ^a, w: ^a)) = Vector4i32<'u>(v.X, v.Y, int32m z, int32m w)
    static member inline Vector4i32 ((v: Vector3i32<'u>, w: ^a)) = Vector4i32<'u>(v.X, v.Y, v.Z, int32m w)
    static member inline Vector4i32 (vector: Vector4i32<'u>) = vector
    static member inline Vector4i32 (vector: Vector4f32<'u>) =
        Vector4i32<'u>(int32m vector.X, int32m vector.Y, int32m vector.Z, int32m vector.W)
    static member inline Vector4i32 (vector: Vector4f<'u>) =
        Vector4i32<'u>(int32m vector.X, int32m vector.Y, int32m vector.Z, int32m vector.W)
    
    static member inline Vector4f32 ((x: ^a, y: ^a, z: ^a, w: ^a)) =
        Vector4f32<'u>(float32m x, float32m y, float32m z, float32m w)
    static member inline Vector4f32 (value: float32<'u>) =
        Vector4f32<'u>(float32m value, float32m value, float32m value, float32m value)
    static member inline Vector4f32 (vector: Vector4i32<'u>) =
        Vector4f32(float32m vector.X, float32m vector.Y, float32m vector.Z, float32m vector.W)
    static member inline Vector4f32 (vector: Vector4f32<'u>) = vector
    static member inline Vector4f32 (vector: Vector4f<'u>) =
        Vector4f32<'u>(float32m vector.X, float32m vector.Y, float32m vector.Z, float32m vector.W)
    static member inline Vector4f32 ((v: Vector2f32<'u>, z: ^a, w: ^a)) = Vector4f32<'u>(v.X, v.Y, float32m z, float32m w)
    static member inline Vector4f32 ((v: Vector2f<'u>, z: ^a, w: ^a)) = Vector4f32<'u>(float32m v.X, float32m v.Y, float32m z, float32m w)
    static member inline Vector4f32 ((v: Vector3f32<'u>, w: ^a)) = Vector4f32<'u>(v.X, v.Y, v.Z, float32m w)
    static member inline Vector4f32 ((v: Vector3f<'u>, w: ^a)) = Vector4f32<'u>(float32m v.X, float32m v.Y, float32m v.Z, float32m w)
    
    static member inline Vector4f ((x: ^a, y: ^a, z: ^a, w: ^a)) = Vector4f<'u>(floatm x, floatm y, floatm z, floatm w)
    static member inline Vector4f (value: float32<'u>) = Vector4f<'u>(floatm value, floatm value, floatm value, floatm value)
    static member inline Vector4f (value: float<'u>) = Vector4f<'u>(value, value, value, value)
    static member inline Vector4f (value: int32<'u>) = Vector4f<'u>(floatm value, floatm value, floatm value, floatm value)
    static member inline Vector4f ((v: Vector2i32<'u>, z: ^a, w: ^a)) = Vector4f<'u>(floatm v.X, floatm v.Y, floatm z, floatm w)
    static member inline Vector4f ((v: Vector2f32<'u>, z: ^a, w: ^a)) = Vector4f<'u>(floatm v.X, floatm v.Y, floatm z, floatm w)
    static member inline Vector4f ((v: Vector2f<'u>, z: ^a, w: ^a)) = Vector4f<'u>(v.X, v.Y, floatm z, floatm w)
    static member inline Vector4f ((v: Vector3f<'u>, w: ^a)) = Vector4f<'u>(v.X, v.Y, v.Z, floatm w)
    static member inline Vector4f (vector: Vector4i32<'u>) =
        Vector4f(floatm vector.X, floatm vector.Y, floatm vector.Z, floatm vector.W)
    static member inline Vector4f (vector: Vector4f32<'u>) =
        Vector4f(floatm vector.X, floatm vector.Y, floatm vector.Z, floatm vector.W)
    static member inline Vector4f (vector: Vector4f<'u>) = vector
    
    static member inline Vector4 ((x: int<'u>, y: int<'u>, z: int<'u>, w: int<'u>)) = Vector4i32<'u>(x, y, z, w)
    static member inline Vector4 ((x: float32<'u>, y: float32<'u>, z: float32<'u>, w: float32<'u>)) = Vector4f32<'u>(x, y, z, w)
    static member inline Vector4 ((x: float<'u>, y: float<'u>, z: float<'u>, w: float<'u>)) = Vector4f<'u>(x, y, z, w)
    static member inline Vector4 (value: int<'u>) = Vector4i<'u>(value)
    static member inline Vector4 (value: float32<'u>) = Vector4f32<'u>(value)
    static member inline Vector4 (value: float<'u>) = Vector4f<'u>(value)
    static member inline Vector4 ((vector: Vector2i32<'u>, z: ^a, w: ^a)) = Vector4i32<'u>(vector.X, vector.Y, int32m z, int32m w)
    static member inline Vector4 ((vector: Vector2f32<'u>, z: ^a, w: ^a)) = Vector4f32<'u>(vector.X, vector.Y, float32m z, float32m w)
    static member inline Vector4 ((vector: Vector2f<'u>, z: ^a, w: ^a)) = Vector4f<'u>(vector.X, vector.Y, floatm z, floatm w)
    static member inline Vector4 ((vector: Vector3i32<'u>, w: ^a)) = Vector4i32<'u>(vector.X, vector.Y, vector.Z, int32m w)
    static member inline Vector4 ((vector: Vector3f32<'u>, w: ^a)) = Vector4f32<'u>(vector.X, vector.Y, vector.Z, float32m w)
    static member inline Vector4 ((vector: Vector3f<'u>, w: ^a)) = Vector4f<'u>(vector.X, vector.Y, vector.Z, floatm w)
    static member inline Vector4 (vector: Vector4i32<'u>) = vector
    static member inline Vector4 (vector: Vector4f32<'u>) = vector
    static member inline Vector4 (vector: Vector4f<'u>) = vector
    
    
    static member inline InfixVectorOp (x: int<'u>, y: int<'u>) = Vector2i<'u>(x, y)
    static member inline InfixVectorOp (x: float32<'u>, y: float32<'u>) = Vector2f32<'u>(x, y)
    static member inline InfixVectorOp (x: float<'u>, y: float<'u>) = Vector2f<'u>(x, y)
    static member inline InfixVectorOp (vector: Vector2i<'u>, z: int<'u>) = Vector3i<'u>(vector, z)
    static member inline InfixVectorOp (x: int<'u>, vector: Vector2i<'u>) =
        Vector3i<'u>(x, vector.X, vector.Y)
    static member inline InfixVectorOp (vector: Vector2f32<'u>, z: float32<'u>) = Vector3f32<'u>(vector, z)
    static member inline InfixVectorOp (x: float32<'u>, vector: Vector2f32<'u>) =
        Vector3f32<'u>(x, vector.X, vector.Y)
    static member inline InfixVectorOp (vector: Vector2f<'u>, z: float<'u>) = Vector3f<'u>(vector, z)
    static member inline InfixVectorOp (x: float<'u>, vector: Vector2f<'u>) = Vector3f<'u>(x, vector.X, vector.Y)

    static member inline InfixVectorOp (vector: Vector3i<'u>, w: int<'u>) = Vector4i<'u>(vector, w)
    static member inline InfixVectorOp (x: int<'u>, vector: Vector3i<'u>) =
        Vector4i<'u>(x, vector.X, vector.Y, vector.Z)
    static member inline InfixVectorOp (vector: Vector3f32<'u>, w: float32<'u>) = Vector4f32<'u>(vector, w)
    static member inline InfixVectorOp (x: float32<'u>, vector: Vector3f32<'u>) =
        Vector4f32<'u>(x, vector.X, vector.Y, vector.Z)
    static member inline InfixVectorOp (vector: Vector3f<'u>, w: float<'u>) = Vector4f<'u>(vector, w)
    static member inline InfixVectorOp (x: float<'u>, vector: Vector3f<'u>) = Vector4f<'u>(x, vector.X, vector.Y, vector.Z)
    
    
    static member inline PrefixVectorOp ((x: int<'u>, y: int<'u>)) = Vector2i<'u>(x, y)
    static member inline PrefixVectorOp ((x: float32<'u>, y: float32<'u>)) = Vector2f32<'u>(x, y)
    static member inline PrefixVectorOp ((x: float<'u>, y: float<'u>)) = Vector2f<'u>(x, y)
    static member inline PrefixVectorOp ((x: int<'u>, y: int<'u>, z: int<'u>)) = Vector3i<'u>(x, y, z)
    static member inline PrefixVectorOp ((x: float32<'u>, y: float32<'u>, z: float32<'u>)) = Vector3f32<'u>(x, y, z)
    static member inline PrefixVectorOp ((x: float<'u>, y: float<'u>, z: float<'u>)) = Vector3f<'u>(x, y, z)
    static member inline PrefixVectorOp ((x: int<'u>, y: int<'u>, z: int<'u>, w: int<'u>)) = Vector4i<'u>(x, y, z, w)
    static member inline PrefixVectorOp ((x: float32<'u>, y: float32<'u>, z: float32<'u>, w: float32<'u>)) = Vector4f32<'u>(x, y, z, w)
    static member inline PrefixVectorOp ((x: float<'u>, y: float<'u>, z: float<'u>, w: float<'u>)) = Vector4f<'u>(x, y, z, w)
    static member inline PrefixVectorOp ((vector: Vector2i<'u>, z: int<'u>)) = Vector3i<'u>(vector, z)
    static member inline PrefixVectorOp ((vector: Vector2f32<'u>, z: float32<'u>)) = Vector3f32(vector, z)
    static member inline PrefixVectorOp ((vector: Vector2f<'u>, z: float<'u>, w: float<'u>)) = Vector4f(vector, z, w)
    static member inline PrefixVectorOp ((vector: Vector2i<'u>, z: int<'u>, w: int<'u>)) = Vector4i<'u>(vector, z, w)
    static member inline PrefixVectorOp ((vector: Vector2f32<'u>, z: float32<'u>, w: float32<'u>)) = Vector4f32(vector, z, w)
    static member inline PrefixVectorOp ((vector: Vector2f<'u>, z: float<'u>)) = Vector3f(vector, z)
    static member inline PrefixVectorOp ((vector: Vector3i<'u>, w: int<'u>)) = Vector4i<'u>(vector, w)
    static member inline PrefixVectorOp ((vector: Vector3f32<'u>, w: float32<'u>)) = Vector4f32(vector, w)
    static member inline PrefixVectorOp ((vector: Vector3f<'u>, w: float<'u>)) = Vector4f(vector, w)


// [FS0064] This construct causes code to be less generic than indicated by the type annotations. The type variable 'T
// has been constrained to be type 'OverloadedOperators'.
#nowarn "64"
module Operators =
    let inline int32m x = PreludeOperators.int32m x
    let inline intm x = PreludeOperators.intm x
    let inline int64m x = PreludeOperators.int64m x
    let inline float32m x = PreludeOperators.float32m x
    let inline floatm x = PreludeOperators.floatm x
    let inline decimalm x = PreludeOperators.decimalm x
    let inline round x = PreludeOperators.round x
    let inline floor x = PreludeOperators.floor x
    let inline ceil x = PreludeOperators.ceil x
    let inline truncate x = PreludeOperators.truncate x
    let inline clamp min max x = PreludeOperators.clamp min max x
    /// <summary>
    ///     Linearly interpolates between two values.
    /// </summary>
    /// <param name="a">The first value to interpolate from.</param>
    /// <param name="b">The second value to interpolate to.</param>
    /// <param name="t">
    ///     The blending factor, expected (and assumed) to be between 0 and 1 (inclusive). A value of 0
    ///     indicates a 100% weight towards <paramref name="a"/>, while a value of 1 indicates a 100% weight towards
    ///     <paramref name="b"/>.
    /// </param>
    /// <remarks>No clamping is performed on <paramref name="t"/>.</remarks>
    let inline lerp a b t = PreludeOperators.lerp a b t
    
    let inline invLerp a b x = PreludeOperators.invLerp a b x
    
    // -- Vector2 --
    let inline vec2i32 (x: ^a) : Vector2i32<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector2i32 : _ -> _) x)
    
    let inline vec2i x : Vector2i<'u> = vec2i32 x
    
    let inline vec2f32 (x: ^a) : Vector2f32<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector2f32 : _ -> _) x)
    
    let inline vec2f (x: ^a) : Vector2f<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector2f : _ -> _) x)
    
    let inline vec2 (x: ^a) : ^Vector2 =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector2 : _ -> _) x)
    
    // -- Vector3 --
    let inline vec3i32 (x: ^a) : Vector3i32<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector3i32 : _ -> _) x)
        
    let inline vec3i x : Vector3i<'u> = vec3i32 x
    
    let inline vec3f32 (x: ^a) : Vector3f32<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector3f32 : _ -> _) x)
    
    let inline vec3f (x: ^a) : Vector3f<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector3f : _ -> _) x)
    
    let inline vec3 (x: ^a) : ^Vector3 =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector3 : _ -> _) x)
    
    // -- Vector4 --
    let inline vec4i32 (x: ^a) : Vector4i32<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector4i32 : _ -> _) x)
        
    let inline vec4i x : Vector4i<'u> = vec4i32 x
    
    let inline vec4f32 (x: ^a) : Vector4f32<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector4f32 : _ -> _) x)
    
    let inline vec4f (x: ^a) : Vector4f<'u> =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector4f : _ -> _) x)
    
    let inline vec4 (x: ^a) : ^Vector4 =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member Vector4 : _ -> _) x)
    
    
    let inline vec (x: ^a) : ^Vector =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member PrefixVectorOp : _ -> _) x)
    
    let inline (@@) (a: ^a) (b: ^b) : ^Vector =
        let _lemma: ^M->_ = id<Ops>
        ((^M or ^a) : (static member InfixVectorOp : _ * _ -> _) (a, b))
    
    let inline (~%) (x: ^a) : ^Vector = vec x

[<Sealed; AbstractClass>]
type Vector =
    static member withMeasure<[<Measure>] 'u> (vec: Vector2i32<1>) : Vector2i32<'u> = retype vec
    static member withMeasure<[<Measure>] 'u> (vec: Vector2f32<1>) : Vector2f32<'u> = retype vec
    static member withMeasure<[<Measure>] 'u> (vec: Vector2f<1>) : Vector2f<'u> = retype vec
    static member withMeasure<[<Measure>] 'u> (vec: Vector3i32<1>) : Vector3i32<'u> = retype vec
    static member withMeasure<[<Measure>] 'u> (vec: Vector3f32<1>) : Vector3f32<'u> = retype vec
    static member withMeasure<[<Measure>] 'u> (vec: Vector3f<1>) : Vector3f<'u> = retype vec
    
    static member withoutMeasure<[<Measure>] 'u> (vec: Vector2i32<'u>) : Vector2i32<1> = retype vec
    static member withoutMeasure<[<Measure>] 'u> (vec: Vector2f32<'u>) : Vector2f32<1> = retype vec
    static member withoutMeasure<[<Measure>] 'u> (vec: Vector2f<'u>) : Vector2f<1> = retype vec
    static member withoutMeasure<[<Measure>] 'u> (vec: Vector3i32<'u>) : Vector3i32<1> = retype vec
    static member withoutMeasure<[<Measure>] 'u> (vec: Vector3f32<'u>) : Vector3f32<1> = retype vec
    static member withoutMeasure<[<Measure>] 'u> (vec: Vector3f<'u>) : Vector3f<1> = retype vec

module Vector =
    open Operators
    
    let inline lengthSquared (v: #IIntVector<'``a<'u>``, '``a<'u^2>``>) : '``a<'u^2>`` = v.LengthSquared
    let inline length (v: #IFracVector<'``a<'u>``, '``a<'u^2>``, '``Vector<'a<1>>``>) : '``a<'u>`` = v.Length
    let inline normalize (v: #IFracVector<'``a<'u>``, '``a<'u^2>``, '``Vector<'a<1>>``>) : '``Vector<'a<1>>`` = v.Normalize ()
    let inline manhattanLen (v: #IIntVector<'``a<'u>``, '``a<'u^2>``>) : '``a<'u>`` =
        v.ManhattanLength
    let inline manhattanDist (v1: 'v) (v2: 'v) : '``a<'u>`` =
        let v1: #IIntVector<'``a<'u>``, '``a<'u^2>``> = v1
        let v2: #IIntVector<'``a<'u>``, '``a<'u^2>``> = v2
        let diff: 'v = v1 - v2
        diff.ManhattanLength
    let inline distSq (v1: 'v) (v2: 'v) : '``a<'u^2>`` =
        let v1: #IIntVector<'``a<'u>``, '``a<'u^2>``> = v1
        let v2: #IIntVector<'``a<'u>``, '``a<'u^2>``> = v2
        let diff: 'v = v1 - v2
        diff.LengthSquared
    let inline dist (v1: 'v) (v2: 'v) : '``a<'u>`` =
        let v1: #IFracVector<'``a<'u>``, '``a<'u^2>``, '``Vector<'a<1>>``> = v1
        let v2: #IFracVector<'``a<'u>``, '``a<'u^2>``, '``Vector<'a<1>>``> = v2
        let diff: 'v = v1 - v2
        diff.Length
        
    // TODO: improve type signature
    let inline dot (v1: ^v1) (v2: ^v2) =
        ((^v1 or ^v2) : (static member Dot : ^v1 * ^v2 -> ^v3) (v1, v2))
    
    let inline cross (v1: ^v1) (v2: ^v2) =
        ((^v1 or ^v2) : (static member Cross : ^v1 * ^v2 -> ^v3) (v1, v2))

// The rest of this file is generated code. See SwizzleCodegen.fsx.

module Swizzling =
    type Extensions =
        [<Extension>]
        static member inline XX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x))
        
        [<Extension>]
        static member inline XY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y))
        
        [<Extension>]
        static member inline XZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z))
        
        [<Extension>]
        static member inline YX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x))
        
        [<Extension>]
        static member inline YY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y))
        
        [<Extension>]
        static member inline YZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z))
        
        [<Extension>]
        static member inline ZX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x))
        
        [<Extension>]
        static member inline ZY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y))
        
        [<Extension>]
        static member inline ZZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z))
        
        [<Extension>]
        static member inline XXX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x, x))
        
        [<Extension>]
        static member inline XXY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x, y))
        
        [<Extension>]
        static member inline XXZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x, z))
        
        [<Extension>]
        static member inline XYX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y, x))
        
        [<Extension>]
        static member inline XYY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y, y))
        
        [<Extension>]
        static member inline XYZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y, z))
        
        [<Extension>]
        static member inline XZX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z, x))
        
        [<Extension>]
        static member inline XZY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z, y))
        
        [<Extension>]
        static member inline XZZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z, z))
        
        [<Extension>]
        static member inline YXX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x, x))
        
        [<Extension>]
        static member inline YXY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x, y))
        
        [<Extension>]
        static member inline YXZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x, z))
        
        [<Extension>]
        static member inline YYX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y, x))
        
        [<Extension>]
        static member inline YYY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y, y))
        
        [<Extension>]
        static member inline YYZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y, z))
        
        [<Extension>]
        static member inline YZX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z, x))
        
        [<Extension>]
        static member inline YZY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z, y))
        
        [<Extension>]
        static member inline YZZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z, z))
        
        [<Extension>]
        static member inline ZXX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x, x))
        
        [<Extension>]
        static member inline ZXY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x, y))
        
        [<Extension>]
        static member inline ZXZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x, z))
        
        [<Extension>]
        static member inline ZYX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y, x))
        
        [<Extension>]
        static member inline ZYY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y, y))
        
        [<Extension>]
        static member inline ZYZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y, z))
        
        [<Extension>]
        static member inline ZZX (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z, x))
        
        [<Extension>]
        static member inline ZZY (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z, y))
        
        [<Extension>]
        static member inline ZZZ (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z, z))
        

    module Vector =
        let inline xx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x))
        
        let inline xy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y))
        
        let inline xz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z))
        
        let inline yx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x))
        
        let inline yy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y))
        
        let inline yz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z))
        
        let inline zx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x))
        
        let inline zy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y))
        
        let inline zz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z))
        
        let inline xxx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x, x))
        
        let inline xxy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x, y))
        
        let inline xxz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, x, z))
        
        let inline xyx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y, x))
        
        let inline xyy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y, y))
        
        let inline xyz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, y, z))
        
        let inline xzx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z, x))
        
        let inline xzy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z, y))
        
        let inline xzz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (x, z, z))
        
        let inline yxx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x, x))
        
        let inline yxy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x, y))
        
        let inline yxz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, x, z))
        
        let inline yyx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y, x))
        
        let inline yyy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y, y))
        
        let inline yyz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, y, z))
        
        let inline yzx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z, x))
        
        let inline yzy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z, y))
        
        let inline yzz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (y, z, z))
        
        let inline zxx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x, x))
        
        let inline zxy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x, y))
        
        let inline zxz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, x, z))
        
        let inline zyx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y, x))
        
        let inline zyy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y, y))
        
        let inline zyz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, y, z))
        
        let inline zzx (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let x = (^``VectorN<'a<'u>>`` : (member X : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z, x))
        
        let inline zzy (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let y = (^``VectorN<'a<'u>>`` : (member Y : ^``a<'u>``) vec)
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z, y))
        
        let inline zzz (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` =
            let z = (^``VectorN<'a<'u>>`` : (member Z : ^``a<'u>``) vec)
            (^``VectorN<'a<'u>>`` : (static member Swizzle : ^``a<'u>`` * ^``a<'u>`` * ^``a<'u>`` -> ^``Vector2<'a<'u>>``) (z, z, z))
        

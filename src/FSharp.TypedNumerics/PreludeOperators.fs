namespace FSharp.TypedNumerics

open System
open System.ComponentModel
open FSharp.Core.LanguagePrimitives
open FSharp.TypedNumerics.Units

module internal PreludeOperatorConstants =
    [<Literal>]
    let degPerRad = 180.<deg> / (Math.PI * 1.<rad>)
    
    [<Literal>]
    let degPerRadf32 = 180.f<deg> / (MathF.PI * 1.f<rad>)
    
    [<Literal>]
    let radPerDeg = (Math.PI * 1.<rad>) / 180.<deg>
    
    [<Literal>]
    let radPerDegf32 = (MathF.PI * 1.f<rad>) / 180.f<deg>


open PreludeOperatorConstants

[<AbstractClass; Sealed; EditorBrowsable(EditorBrowsableState.Never)>]
type PreOps =
    static member inline Int32m (value: int32<'u>) : int32<'u> = value
    static member inline Int32m (value: int64<'u>) : int32<'u> = Int32WithMeasure (int32 value)
    static member inline Int32m (value: float32<'u>) : int32<'u> = Int32WithMeasure (int32 value)
    static member inline Int32m (value: float<'u>) : int32<'u> = Int32WithMeasure (int32 value)
    static member inline Int32m (value: decimal<'u>) : int32<'u> = Int32WithMeasure (int32 (decimal value))
    
    static member inline Int64m (value: int32<'u>) : int64<'u> = Int64WithMeasure (int64 value)
    static member inline Int64m (value: int64<'u>) : int64<'u> = value
    static member inline Int64m (value: float32<'u>) : int64<'u> = Int64WithMeasure (int64 value)
    static member inline Int64m (value: float<'u>) : int64<'u> = Int64WithMeasure (int64 value)
    static member inline Int64m (value: decimal<'u>) : int64<'u> = Int64WithMeasure (int64 (decimal value))
    
    static member inline Float32m (value: int32<'u>) : float32<'u> = Float32WithMeasure (float32 value)
    static member inline Float32m (value: int64<'u>) : float32<'u> = Float32WithMeasure (float32 value)
    static member inline Float32m (value: float32<'u>) : float32<'u> = value
    static member inline Float32m (value: float<'u>) : float32<'u> = Float32WithMeasure (float32 value)
    static member inline Float32m (value: decimal<'u>) : float32<'u> = Float32WithMeasure (float32 (decimal value))
    
    static member inline Floatm (value: int32<'u>) : float<'u> = FloatWithMeasure (float value)
    static member inline Floatm (value: int64<'u>) : float<'u> = FloatWithMeasure (float value)
    static member inline Floatm (value: float32<'u>) : float<'u> = FloatWithMeasure (float value)
    static member inline Floatm (value: float<'u>) : float<'u> = value
    static member inline Floatm (value: decimal<'u>) : float<'u> = FloatWithMeasure (float (decimal value))
    
    static member inline Decimalm (value: int32<'u>) : decimal<'u> = DecimalWithMeasure (decimal value)
    static member inline Decimalm (value: int64<'u>) : decimal<'u> = DecimalWithMeasure (decimal value)
    static member inline Decimalm (value: float32<'u>) : decimal<'u> = DecimalWithMeasure (decimal value)
    static member inline Decimalm (value: float<'u>) : decimal<'u> = DecimalWithMeasure (decimal value)
    static member inline Decimalm (value: decimal<'u>) : decimal<'u> = value
    
    static member inline RoundOp (n: float<'u>) : float<'u> = n |> float |> round |> FloatWithMeasure
    static member inline RoundOp (n: float32<'u>) : float32<'u> = n |> float32 |> round |> Float32WithMeasure
    static member inline RoundOp (n: decimal<'u>) : decimal<'u> = n |> decimal |> round |> DecimalWithMeasure
    static member inline FloorOp (n: float<'u>) : float<'u> = n |> float |> floor |> FloatWithMeasure
    static member inline FloorOp (n: float32<'u>) : float32<'u> = n |> float32 |> floor |> Float32WithMeasure
    static member inline FloorOp (n: decimal<'u>) : decimal<'u> = n |> decimal |> floor |> DecimalWithMeasure
    static member inline CeilOp (n: float<'u>) : float<'u> = n |> float |> ceil |> FloatWithMeasure
    static member inline CeilOp (n: float32<'u>) : float32<'u> = n |> float32 |> ceil |> Float32WithMeasure
    static member inline CeilOp (n: decimal<'u>) : decimal<'u> = n |> decimal |> ceil |> DecimalWithMeasure
    
    static member inline TruncateOp (n: float<'u>) : float<'u> = n |> float |> truncate |> FloatWithMeasure
    static member inline TruncateOp (n: float32<'u>) : float32<'u> = n |> float32 |> truncate |> Float32WithMeasure
    static member inline TruncateOp (n: decimal<'u>) : decimal<'u> = n |> decimal |> truncate |> DecimalWithMeasure
    
    static member inline ClampOp (n: int32<'u>, min: int32<'u>, max: int32<'u>) : int32<'u> =
        Math.Clamp (int32 n, int32 min, int32 max) |> Int32WithMeasure
    static member inline ClampOp (n: int64<'u>, min: int64<'u>, max: int64<'u>) : int64<'u> =
        Math.Clamp (int64 n, int64 min, int64 max) |> Int64WithMeasure
    static member inline ClampOp (n: float<'u>, min: float<'u>, max: float<'u>) : float<'u> =
        Math.Clamp (float n, float min, float max) |> FloatWithMeasure
    static member inline ClampOp (n: float32<'u>, min: float32<'u>, max: float32<'u>) : float32<'u> =
        Math.Clamp (float32 n, float32 min, float32 max) |> Float32WithMeasure
    static member inline ClampOp (n: decimal<'u>, min: decimal<'u>, max: decimal<'u>) : decimal<'u> =
        Math.Clamp (decimal n, decimal min, decimal max) |> DecimalWithMeasure
    
    static member LerpOp (a: float<'u>, b: float<'u>, t: float) = a + (b - a) * t
    // static member LerpOp (a: float<'u>, b: float<'u>, t: float32) = a + (b - a) * (float t)
    static member LerpOp (a: float32<'u>, b: float32<'u>, t: float32) = a + (b - a) * t
    
    static member InvLerpOp (a: float<'u>, b: float<'u>, x: float<'u>) = (x - a) / (b - a)
    
    static member InvLerpOp (a: float32<'u>, b: float32<'u>, x: float32<'u>) = (x - a) / (b - a)
    
    // static member (%) (x: float<'u>, y: float<'u>) : float<'u> = x % y
    
    static member RadiansToDegrees (r: float<rad>) : float<deg> = r * degPerRad
    static member RadiansToDegrees (r: float32<rad>) : float32<deg> = r * degPerRadf32
    
    
    static member DegreesToRadians (r: float<deg>) : float<rad> = r * radPerDeg
    static member DegreesToRadians (r: float32<deg>) : float32<rad> = r * radPerDegf32


// [FS0064] This construct causes code to be less generic than indicated by the type annotations. The type variable 'T has been constrained to be type 'OverloadedOperators'.
#nowarn "64"
module PreludeOperators =
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.int32``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The converted value.</returns>
    let inline int32m (x: ^a) : int32<'u> =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member Int32m : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.int``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The converted value.</returns>
    let inline intm x : int<'u> = int32m x
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.int64``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The converted value.</returns>
    let inline int64m (x: ^a) : int64<'u> =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member Int64m : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.float32``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The converted value.</returns>
    let inline float32m (x: ^a) : float32<'u> =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member Float32m : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.float``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The converted value.</returns>
    let inline floatm (x: ^a) : float<'u> =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member Floatm : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.decimal``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The converted value.</returns>
    let inline decimalm (x: ^a) : decimal<'u> =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member Decimalm : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.round``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The rounded value.</returns>
    let inline round (x: ^a) : ^a =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member RoundOp : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.floor``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The floor of the input.</returns>
    let inline floor (x: ^a) : ^a =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member FloorOp : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.ceil``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The ceil of the input.</returns>
    let inline ceil (x: ^a) : ^a=
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member CeilOp : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:Microsoft.FSharp.Core.Operators.truncate``1'/>.</summary>
    /// <param name="x">The input value.</param>
    /// <returns>The truncated value.</returns>
    let inline truncate (x: ^a) : ^a =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member TruncateOp : _ -> _) x)
    
    /// <summary>UoM-preserving version of <see cref='M:System.Math.Clamp'/>.</summary>
    /// <param name="min">The lower bound of the result.</param>
    /// <param name="max">The upper bound of the result.</param>
    /// <param name="x">The value to be clamped.</param>
    /// <returns>The clamped value.</returns>
    let inline clamp (min: ^a) (max: ^a) (x: ^a) : ^a =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member ClampOp : _ * _ * _ -> _) (x, min, max))
    
    // FSharp.Core has `exp` but not all of these exponential functions
    
    /// <summary>Computes <c>E</c> raised to a given power and subtracts one</summary>
    /// <param name="x">The power to which <c>E</c> is raised.</param>
    /// <returns><c>exp(x)-1.0</c></returns>
    let inline expm1 (x: ^a) : ^a = (^a : (static member ExpM1 : ^a -> ^a) x)
    /// <summary>Computes <c>2</c> raised to a given power.</summary>
    /// <param name="x">The power to which <c>2</c> is raised.</param>
    /// <returns><c>2.0 ** x</c></returns>
    let inline exp2 (x: ^a) : ^a = (^a : (static member Exp2 : ^a -> ^a) x)
    /// <summary>Computes <c>2</c> raised to a given power and subtracts one.</summary>
    /// <param name="x">The power to which <c>2</c> is raised.</param>
    /// <returns><c>exp2(x)-1.0</c></returns>
    let inline exp2m1 (x: ^a) : ^a = (^a : (static member Exp2M1 : ^a -> ^a) x)
    /// <summary>Computes <c>10</c> raised to a given power.</summary>
    /// <param name="x">The power to which <c>10</c> is raised.</param>
    /// <returns><c>10.0 ** x</c></returns>
    let inline exp10 (x: ^a) : ^a = (^a : (static member Exp10 : ^a -> ^a) x)
    /// <summary>Computes <c>10</c> raised to a given power and subtracts one.</summary>
    /// <param name="x">The power to which <c>10</c> is raised.</param>
    /// <returns><c>exp10(x)-1.0</c></returns>
    let inline exp10m1 (x: ^a) : ^a = (^a : (static member Exp10M1 : ^a -> ^a) x)
    
    // FSharp.Core has `log` and `log10` but not all of these log functions
    
    /// <summary>Computes the natural (base-<c>E</c>) logarithm of a given value plus one.</summary>
    /// <param name="x">The value to which one is added before computing the natural logarithm.</param>
    /// <returns><c>log(x+1.0)</c></returns>
    let inline logp1 (x: ^a) : ^a = (^a : (static member LogP1 : ^a -> ^a) x)
    /// <summary>Computes the base-<c>2</c> logarithm of a given value.</summary>
    /// <param name="x">The value whose base-<c>2</c> logarithm is to be computed.</param>
    /// <returns>The base-<c>2</c> logarithm of <paramref name="x"/>.</returns>
    let inline log2 (x: ^a) : ^a = (^a : (static member Log2 : ^a -> ^a) x)
    /// <summary>Computes the base-<c>2</c> logarithm of a given value plus one.</summary>
    /// <param name="x">The value whose log2 is to be computed.</param>
    /// <returns><c>log2(x)+1</c></returns>
    let inline log2p1 (x: ^a) : ^a = (^a : (static member Log2P1 : ^a -> ^a) x)
    /// <summary>Computes the base-<c>10</c> logarithm of a given value plus one.</summary>
    /// <param name="x">The value whose log10 is to be computed.</param>
    /// <returns><c><c>log10(x)+1</c></c></returns>
    let inline log10p1 (x: ^a) : ^a = (^a : (static member Log10P1 : ^a -> ^a) x)
    
    /// <summary>Computes the n-th root of a value.</summary>
    /// <param name="x">The value whose n-th root is to be computed.</param>
    /// <param name="n">The degree of the root to be computed.</param>
    let inline rootn (x: ^a) (n: int) = (^a : (static member RootN : ^a * int -> ^a) (x, n))
    
    /// <summary>Computes the linear interpolation between <paramref name="a"/> and <paramref name="b"/> with a weight
    /// of <paramref name="t"/>, preserving units of measure.</summary>
    /// <param name="a">The value to interpolate from.</param>
    /// <param name="b">The value to interpolate to.</param>
    /// <param name="t">A value, intended to be between 0 and 1, that indicates the weight of the interpolation.</param>
    /// <returns>A blend of <paramref name="a"/> and <paramref name="b"/> weighted using <paramref name="t"/>.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// lerp 0.0 100.0 0.0 // evaluates to 0.0
    /// lerp 0.0 100.0 1.0 // evaluates to 100.0
    /// lerp 0.0 100.0 0.5 // evaluates to 50.0
    /// </code>
    /// </example>
    let inline lerp (a: ^a) (b: ^a) (t: ^t) : ^a =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a or ^t) : (static member LerpOp : _ -> _ -> _ -> _) (a, b, t))
    
    /// <summary>Computes the inverse of the linear interpolation between two values, preserving units of measure.</summary>
    /// <param name="a">The value which was interpolated from.</param>
    /// <param name="b">The value which was interpolated to.</param>
    /// <param name="x">An already interpolated value between <paramref name="a"/> and <paramref name="b"/>.</param>
    /// <returns>How much <paramref name="x"/> is weighted towards <paramref name="a"/> or <paramref name="b"/>. Or,
    /// the value which, when passed as the weight into <see cref="M:FSharp.TypedNumerics.PreludeOperators.lerp"/>
    /// along with <paramref name="a"/> and <paramref name="b"/>, would produce <paramref name="x"/>.</returns>
    /// <example>
    /// <code lang="fsharp">
    /// invLerp 0.0 100.0 0.0   // evaluates to 0.0
    /// invLerp 0.0 100.0 100.0 // evaluates to 1.0
    /// invLerp 0.0 100.0 50.0  // evaluates to 0.5
    /// </code>
    /// </example>
    let inline invLerp (a: ^a) (b: ^a) (x: ^a) : ^x =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^a) : (static member InvLerpOp : _ -> _ -> _ -> _) (a, b, x))
    
    /// <summary>Converts a value from radians to degrees.</summary>
    /// <param name="radians">A value, in <see cref="T:FSharp.TypedNumerics.Units.rad"/>, to convert.</param>
    /// <example>
    /// <code lang="fsharp">
    /// radToDeg 3.14&lt;rad&gt; // evaluates to 179.9087477&lt;deg&gt;
    /// radToDeg 6.28&lt;rad&gt; // evaluates to 359.8174953&lt;deg&gt;
    /// </code>
    /// </example>
    let inline radToDeg (radians: ^radians) : ^degrees =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^radians) : (static member RadiansToDegrees : ^radians -> ^degrees) radians)
    
    /// <summary>Converts a value in degrees to radians.</summary>
    /// <param name="degrees">A value, in <see cref="T:FSharp.TypedNumerics.Units.deg"/>, to convert.</param>
    /// <example>
    /// <code lang="fsharp">
    /// radToDeg 180.0&lt;deg&gt; // evaluates to 3.141592654&lt;rad&gt;
    /// radToDeg 360.0&lt;deg&gt; // evaluates to 6.283185307&lt;rad&gt;
    /// </code>
    /// </example>
    let inline degToRad (degrees: ^degrees) : ^radians =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^degrees) : (static member DegreesToRadians : ^degrees -> ^radians) degrees)
    
    let inline isNanf (x: float32<'u>) = Single.IsNaN (float32 x)
    let inline isNan (x: float32<'u>) = Double.IsNaN (float x)
    
    let inline isInfinityf (x: float32<'u>) = Single.IsInfinity (float32 x)
    let inline isInfinity (x: float<'u>) = Double.IsInfinity (float x)
    

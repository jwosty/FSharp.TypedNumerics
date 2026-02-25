namespace FSharp.TypedNumerics
open System
open System.ComponentModel
open System.Diagnostics
open System.Runtime.CompilerServices
open FSharp.TypedNumerics.Units
open FSharp.TypedNumerics.Internals
open FSharp.TypedNumerics.PreludeOperators
open LanguagePrimitives

[<EditorBrowsable(EditorBrowsableState.Never)>]
module EasingOpsConstants =
    let c1f = 1.70158
    let c2f = c1f * 1.525
    let c3f = c1f + 1.
    let c4f = (2. * Math.PI) / 3.
    let c5f = (2. * Math.PI) / 4.5
    
    let c1f32 = float32m c1f
    let c2f32 = float32m c2f
    let c3f32 = float32m c3f
    let c4f32 = float32m c4f
    let c5f32 = float32m c5f

open EasingOpsConstants

// [FS0064] This construct causes code to be less generic than indicated by the type annotations. The type variable 'T
// has been constrained to be type 'OverloadedOperators'.
#nowarn "64"
[<Struct>]
type EasingOps =
    // See: https://blog.pkh.me/p/41-fixing-the-iterative-damping-interpolation-in-video-games.html
    // and: https://www.rorydriscoll.com/2016/03/07/frame-rate-independent-damping-using-lerp/

    static member ExpDampBlendOp (smoothRate: float, Δt: float) =
        // An equivalent alternate form is: 1.f - (smoothRate ** float32 deltaTime)
        -expm1(log smoothRate * float Δt)
    static member ExpDampBlendOp (smoothRate: float32, Δt: float32) =
        -expm1(log smoothRate * float32 Δt)
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/qEEEmLP</plot>
    static member EaseInSine (t: float) : float      = 1. - cos ((t * Math.PI) / 2.)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/qEEEmLP</plot>
    static member EaseInSine (t: float32) : float32  = 1.f - cos ((t * MathF.PI) / 2.f)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/qEEEmLP</plot>
    static member inline EaseInSine (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInSine : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/Ggggmaw</plot>
    static member EaseOutSine (t: float) : float     = sin ((t * Math.PI) / 2.)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/Ggggmaw</plot>
    static member EaseOutSine (t: float32) : float32 = sin ((t * MathF.PI) / 2.f)
    /// <summary></summary>
    /// <param name="t"></param>
    /// <plot>https://codepen.io/jwosty/pen/Ggggmaw</plot>
    static member inline EaseOutSine (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutSine : ^t -> ^b) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPgLG</plot>
    static member EaseInOutSine (t: float) : float     = -(cos (Math.PI * t) - 1.) / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPgLG</plot>
    static member EaseInOutSine (t: float32) : float32 = -(cos (MathF.PI * t) - 1.f) / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPgLG</plot>
    static member inline EaseInOutSine (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutSine : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/pvvvwyb</plot>
    static member inline EaseInQuad (t: ^a) : ^b = t * t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/pvvvwyb</plot>
    static member inline EaseInQuad (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInQuad : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/RNNNgGv</plot>
    static member inline EaseOutQuad (t: ^a) : ^a =
        let t' = GenericOne<^a> - t
        GenericOne - (t' * t')
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/RNNNgGv</plot>
    static member inline EaseOutQuad (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutQuad : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxzZP</plot>
    static member EaseInOutQuad (t: float) : float =
        if t < 0.5 then 2. * t * t
        else
            let t' = -2. * t + 2.
            1. - (t' * t') / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxzZP</plot>
    static member EaseInOutQuad (t: float32) : float32 =
        if t < 0.5f then 2.f * t * t
        else
            // 1.f - (pown (-2.f * t + 2.f) 2) / 2.f
            let t' = -2.f * t + 2.f
            1.f - (t' * t') / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxzZP</plot>
    static member inline EaseInOutQuad (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutQuad : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/xbbbrXj</plot>
    static member inline EaseInCubic (t: ^a) : ^b = t * t * t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/xbbbrXj</plot>
    static member inline EaseInCubic (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInCubic : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPgvP</plot>
    static member inline EaseOutCubic (t: ^a) : ^a =
        let t' = GenericOne<^a> - t
        GenericOne<^a> - (t' * t' * t')
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPgvP</plot>
    static member inline EaseOutCubic (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutCubic : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/MYYYoXv</plot>
    static member EaseInOutCubic (t: float) : float =
        if t < 0.5 then 4. * t * t * t
        else
            let t' = -2. * t + 2.
            1. - (t' * t' * t') / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/MYYYoXv</plot>
    static member EaseInOutCubic (t: float32) : float32 =
        if t < 0.5f then 4.f * t * t * t
        else
            let t' = -2.f * t + 2.f
            1.f - (t' * t' * t') / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/MYYYoXv</plot>
    static member inline EaseInOutCubic (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutCubic : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/yyyyXRo</plot>
    static member inline EaseInQuart (t: ^a) : ^b = t * t * t * t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/yyyyXRo</plot>
    static member inline EaseInQuart (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInQuart : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/KwwwqYv</plot>
    static member inline EaseOutQuart (t: ^a) : ^a =
        let t' = GenericOne<^a> - t
        GenericOne<^a> - (t' * t' * t' * t')
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/KwwwqYv</plot>
    static member inline EaseOutQuart (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutQuart : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/LEEELKv</plot>
    static member EaseInOutQuart (t: float) : float =
        if t < 0.5 then 8. * t * t * t * t
        else
            let t' = -2. * t + 2.
            1. - (t' * t' * t' * t') / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/LEEELKv</plot>
    static member EaseInOutQuart (t: float32) : float32 =
        if t < 0.5f then 8.f * t * t * t * t
        else
            let t' = -2.f * t + 2.f
            1.f - (t' * t' * t' * t') / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/LEEELKv</plot>
    static member inline EaseInOutQuart (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutQuart : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/gbbbxww</plot>
    static member inline EaseInQuint (t: ^a) : ^b = t * t * t * t * t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/gbbbxww</plot>
    static member inline EaseInQuint (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInQuint : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/RNNNZpa</plot>
    static member inline EaseOutQuint (t: ^a) : ^a =
        let t' = GenericOne<^a> - t
        GenericOne<^a> - (t' * t' * t' * t' * t')
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/RNNNZpa</plot>
    static member inline EaseOutQuint (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutQuint : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/bNNNrro</plot>
    static member EaseInOutQuint (t: float) : float =
        if t < 0.5 then 16. * t * t * t * t * t
        else
            let t' = -2. * t + 2.
            1. - (t' * t' * t' * t' * t') / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/bNNNrro</plot>
    static member EaseInOutQuint (t: float32) : float32 =
        if t < 0.5f then 16.f * t * t * t * t * t
        else
            let t' = -2.f * t + 2.f
            1.f - (t' * t' * t' * t' * t') / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/bNNNrro</plot>
    static member inline EaseInOutQuint (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutQuint : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/myyyMXO</plot>
    static member inline EaseInCirc (t: ^a) : ^b = GenericOne - sqrt (GenericOne - (t * t))
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/myyyMXO</plot>
    static member inline EaseInCirc (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInCirc : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/LEEEjmV</plot>
    static member inline EaseOutCirc (t: ^a) : ^a = sqrt (GenericOne - pown (t - GenericOne) 2)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/LEEEjmV</plot>
    static member inline EaseOutCirc (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutCirc : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwKBj</plot>
    static member EaseInOutCirc (t: float) : float =
        if t < 0.5 then
            (1. - sqrt (1. - pown (2. * t) 2)) / 2.
        else
            (sqrt (1. - pown (-2. * t + 2.) 2) + 1.) / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwKBj</plot>
    static member EaseInOutCirc (t: float32) : float32 =
        if t < 0.5f then
            (1.f - sqrt (1.f - pown (2.f * t) 2)) / 2.f
        else
            (sqrt (1.f - pown (-2.f * t + 2.f) 2) + 1.f) / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwKBj</plot>
    static member inline EaseInOutCirc (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutCirc : ^t -> ^t) t)
        lerp a b tLin
    
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/KwwwXbK</plot>
    static member EaseInBack (t: float) : float = c3f * t * t * t - c1f * t * t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/KwwwXbK</plot>
    static member EaseInBack (t: float32) : float32 = c3f32 * t * t * t - c1f32 * t * t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/KwwwXbK</plot>
    static member inline EaseInBack (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInBack : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/dPPPVwd</plot>
    static member EaseOutBack (t: float) : float =
        let t' = t - 1.
        1. + c3f * t' * t' * t' + c1f * t' * t'
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/dPPPVwd</plot>
    static member EaseOutBack (t: float32) : float32 =
        let t' = t - 1.f
        1.f + c3f32 * t' * t' * t' + c1f32 * t' * t'
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/dPPPVwd</plot>
    static member inline EaseOutBack (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutBack : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/EaaawJm</plot>
    static member EaseInOutBack (t: float) : float =
        if t < 0.5 then
            let t' = 2. * t
            t' * t' * ((c2f + 1.) * 2. * t - c2f) / 2.
        else
            let t' = 2. * t - 2.
            (t' * t' * ((c2f + 1.) * (t * 2. - 2.) + c2f) + 2.) / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/EaaawJm</plot>
    static member EaseInOutBack (t: float32) : float32 =
        if t < 0.5f then
            let t' = 2.f * t
            t' * t' * ((c2f32 + 1.f) * 2.f * t - c2f32) / 2.f
        else
            let t' = 2.f * t - 2.f
            (t' * t' * ((c2f32 + 1.f) * (t * 2.f - 2.f) + c2f32) + 2.f) / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/EaaawJm</plot>
    static member inline EaseInOutBack (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutBack : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/vEEEWOV</plot>
    static member EaseInElastic (t: float) : float = -Double.Exp2(10. * t - 10.) * sin((t * 10. - 10.75) * c4f)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/vEEEWOV</plot>
    static member EaseInElastic (t: float32) : float32 =
        -Single.Exp2(10.f * t - 10.f) * sin((t * 10.f - 10.75f) * c4f32)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/vEEEWOV</plot>
    static member inline EaseInElastic (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInElastic : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/emmmeBW</plot>
    static member EaseOutElastic (t: float) : float = Double.Exp2 (-10. * t) * sin ((t * 10. - 0.75) * c4f) + 1.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/emmmeBW</plot>
    static member EaseOutElastic (t: float32) : float32 =
        Single.Exp2 (-10.f * t) * sin ((t * 10.f - 0.75f) * c4f32) + 1.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/emmmeBW</plot>
    static member inline EaseOutElastic (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutElastic : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/GgggOMb</plot>
    static member EaseInOutElastic (t: float) : float =
        if t < 0.5 then
            -(Double.Exp2 (20. * t - 10.) * sin ((20. * t - 11.125) * c5f)) / 2.
        else
            (Double.Exp2 (-20. * t + 10.) * sin ((20. * t - 11.125) * c5f)) / 2. + 1.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/GgggOMb</plot>
    static member EaseInOutElastic (t: float32) : float32 =
        if t < 0.5f then
            -(Single.Exp2 (20.f * t - 10.f) * sin ((20.f * t - 11.125f) * c5f32)) / 2.f
        else
            (Single.Exp2 (-20.f * t + 10.f) * sin ((20.f * t - 11.125f) * c5f32)) / 2.f + 1.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/GgggOMb</plot>
    static member inline EaseInOutElastic (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutElastic : ^t -> ^t) t)
        lerp a b tLin
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member BounceOut (t: float) =
        let n1 = 7.5625
        let d1 = 2.75
        if t < 1. / d1 then
            let y = n1 * t * t
            y
        elif t < 2. / d1 then
            let t = t - 1.5 / d1
            let y = n1 * t * t + 0.75
            y
        elif t < 2.5 / d1 then
            let t = t - 2.25 / d1
            let y = n1 * t * t + 0.9375
            y
        else
            let t = 2.625 / d1
            let y = n1 * t * t + 0.984375
            y
    
    static member BounceOut (t: float32) = floatm t |> EasingOps.BounceOut |> float32m
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPOOJ</plot>
    static member EaseInBounce (t: float) : float = 1. - EasingOps.BounceOut (1. - t)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPOOJ</plot>
    static member EaseInBounce (t: float32) : float32 = 1.f - EasingOps.BounceOut (1.f - t)
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPOOJ</plot>
    static member inline EaseInBounce (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInBounce : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwOVP</plot>
    static member EaseOutBounce (t: float) : float = EasingOps.BounceOut t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwOVP</plot>
    static member EaseOutBounce (t: float32) : float32 = EasingOps.BounceOut t
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwOVP</plot>
    static member inline EaseOutBounce (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseOutBounce : ^t -> ^t) t)
        lerp a b tLin
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxPba</plot>
    static member EaseInOutBounce (t: float) : float =
        if t < 0.5 then
            (1. - EasingOps.BounceOut (1. - 2. * t)) / 2.
        else
            (1. + EasingOps.BounceOut (2. * t - 1.)) / 2.
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxPba</plot>
    static member EaseInOutBounce (t: float32) : float32 =
        if t < 0.5f then
            let n = (1.f - EasingOps.BounceOut (1.f - 2.f * t)) / 2.f
            n
        else
            (1.f + EasingOps.BounceOut (2.f * t - 1.f)) / 2.f
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxPba</plot>
    static member inline EaseInOutBounce (a: ^a, b: ^a, t: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let tLin = ((^M or ^t) : (static member EaseInOutBounce : ^t -> ^t) t)
        lerp a b tLin

module Easing =
    /// <summary>
    ///     Computes exponential damping between two values. This function can be used for framerate independent
    ///     purposes.
    /// </summary>
    /// <param name="smoothRate">The proportion of `a` remaining after one time unit.</param>
    /// <param name="a">The first value to blend from.</param>
    /// <param name="b">The second value to blend to.</param>
    /// <param name="Δt">
    ///     A delta time (aka a time step), representing a discrete amount of time to step forward.
    /// </param>
    /// <remarks>
    ///     NOTE: unlike many other interpolation functions, <paramref name="Δt"/> is a <b>time step</b>, <i>not</i> a
    ///     time since start. In other words - use this function when you want to accumulate (integrate) exponential
    ///     decay over a series of discrete time steps. A common example would be a game's <c>Update()</c> method.
    /// </remarks>
    let inline expDamp (smoothRate: ^t) (a: ^a) (b: ^a) (Δt: ^t) : ^a =
        let _lemma: ^M->_ = id<EasingOps>
        let blend = ((^M or ^a or ^t) : (static member ExpDampBlendOp : ^t -> ^t -> ^t) (smoothRate, Δt))
        lerp a b blend
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/qEEEmLP</plot>
    let inline easeInSine (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInSine : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/Ggggmaw</plot>
    let inline easeOutSine (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutSine : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPgLG</plot>
    let inline easeInOutSine (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutSine : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/pvvvwyb</plot>
    let inline easeInQuad (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInQuad : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/RNNNgGv</plot>
    let inline easeOutQuad (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutQuad : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxzZP</plot>
    let inline easeInOutQuad (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutQuad : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/xbbbrXj</plot>
    let inline easeInCubic (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInCubic : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPgvP</plot>
    let inline easeOutCubic (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutCubic : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/MYYYoXv</plot>
    let inline easeInOutCubic (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutCubic : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/yyyyXRo</plot>
    let inline easeInQuart (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInQuart : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/KwwwqYv</plot>
    let inline easeOutQuart (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutQuart : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/LEEELKv</plot>
    let inline easeInOutQuart (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutQuart : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/gbbbxww</plot>
    let inline easeInQuint (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInQuint : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/RNNNZpa</plot>
    let inline easeOutQuint (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutQuint : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/bNNNrro</plot>
    let inline easeInOutQuint (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutQuint : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/myyyMXO</plot>
    let inline easeInCirc (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInCirc : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/LEEEjmV</plot>
    let inline easeOutCirc (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutCirc : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwKBj</plot>
    let inline easeInOutCirc (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutCirc : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/KwwwXbK</plot>
    let inline easeInBack (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInBack : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/dPPPVwd</plot>
    let inline easeOutBack (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutBack : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/EaaawJm</plot>
    let inline easeInOutBack (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutBack : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/vEEEWOV</plot>
    let inline easeInElastic (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInElastic : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/emmmeBW</plot>
    let inline easeOutElastic (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutElastic : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/GgggOMb</plot>
    let inline easeInOutElastic (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutElastic : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/OPPPOOJ</plot>
    let inline easeInBounce (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInBounce : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/PwwwOVP</plot>
    let inline easeOutBounce (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseOutBounce : ^a * ^a * ^b -> ^c) (a, b, t))
    
    /// <summary></summary>
    /// <plot>https://codepen.io/jwosty/pen/zxxxPba</plot>
    let inline easeInOutBounce (a: ^a) (b: ^a) (t: ^b) =
        let _lemma: ^M->_ = id<EasingOps>
        ((^M or ^a) : (static member EaseInOutBounce : ^a * ^a * ^b -> ^c) (a, b, t))
    

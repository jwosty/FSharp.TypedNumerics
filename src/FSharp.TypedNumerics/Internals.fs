namespace FSharp.TypedNumerics

// [FS0042] This construct is deprecated: it is only for use in the F# library
#nowarn "0042"

module internal Internals =
    let inline retype (x: 'T) : 'U = (# "" x: 'U #)

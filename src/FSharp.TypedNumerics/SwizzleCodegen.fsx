
// Codegen for swizzle module and extension methods in Vector.fs

open System.Text

let indent indentLevel = String.replicate indentLevel "    "

let appendLineIndent (sb: StringBuilder) indentLevel (str: string) =
    sb.Append (indent indentLevel) |> ignore
    sb.AppendLine str |> ignore

let mkSwizzleBody (sb: StringBuilder) (il: int) (members: string list) =
    let appendLineIndent = appendLineIndent sb
    
    let memsAndVars = members |> List.map (fun m -> m, m.ToLower ())
    memsAndVars |> List.distinct |> List.sortBy snd |> List.iter (fun (m, mVar) ->
        appendLineIndent il $"let %s{mVar} = (^``VectorN<'a<'u>>`` : (member %s{m} : ^``a<'u>``) vec)")
    let tupTy = memsAndVars |> List.map (fun _ -> "^``a<'u>``") |> String.concat " * "
    let tupElts = memsAndVars |> List.map snd |> String.concat ", "
    appendLineIndent il $"(^``VectorN<'a<'u>>`` : (static member Swizzle : %s{tupTy} -> ^``Vector2<'a<'u>>``) (%s{tupElts}))"

let mkSwizzleExtMethod (sb: StringBuilder) (il: int) (names: {| Members: string list; ExtMethod: string |}) =
    let appendLineIndent = appendLineIndent sb
    
    appendLineIndent il "[<Extension>]"
    appendLineIndent il $"static member inline %s{names.ExtMethod} (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` ="
    mkSwizzleBody sb (il+1) names.Members

let mkSwizzleModuleFunc (sb: StringBuilder) (il: int) (names: {| Members: string list; ModuleFunc: string |}) =
    let appendLineIndent = appendLineIndent sb
    
    appendLineIndent il $"let inline %s{names.ModuleFunc} (vec: ^``VectorN<'a<'u>>``) : ^``Vector2<'a<'u>>`` ="
    mkSwizzleBody sb (il+1) names.Members

let swizzle () =
    let members = [ "X"; "Y"; "Z" ]
    
    let finalSb = StringBuilder()
    let extensionTypeSb = StringBuilder()
    let moduleSb = StringBuilder()
    
    appendLineIndent finalSb 0 "module Swizzling ="
    
    appendLineIndent extensionTypeSb 1 "type Extensions ="
    
    appendLineIndent moduleSb 1 "module Vector ="
    
    for m1 in members do
        for m2 in members do
            let il = 2
            let extMethod = m1 + m2
            let moduleFunc = extMethod.ToLower ()
            
            mkSwizzleExtMethod extensionTypeSb il {| Members = [ m1; m2 ]; ExtMethod = extMethod |}
            appendLineIndent extensionTypeSb il ""
            
            mkSwizzleModuleFunc moduleSb il {| Members = [ m1; m2 ]; ModuleFunc = moduleFunc |}
            appendLineIndent moduleSb il ""
    
    for m1 in members do
        for m2 in members do
            for m3 in members do
                let il = 2
                let extMethod = m1 + m2 + m3
                let moduleFunc = extMethod.ToLower ()
                
                mkSwizzleExtMethod extensionTypeSb il {| Members = [ m1; m2; m3 ]; ExtMethod = extMethod |}
                appendLineIndent extensionTypeSb il ""
                
                mkSwizzleModuleFunc moduleSb il {| Members = [ m1; m2; m3 ]; ModuleFunc = moduleFunc |}
                appendLineIndent moduleSb il ""
    
    finalSb.AppendLine (extensionTypeSb.ToString ()) |> ignore
    finalSb.Append (moduleSb.ToString ()) |> ignore
    
    finalSb.ToString ()

printfn "%s" (swizzle ())

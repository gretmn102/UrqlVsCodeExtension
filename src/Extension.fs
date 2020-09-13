[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ionide.VSCode.FSharp
open Fable.Core
open Fable.Import
// open Fable.Core.JS
// open Fable.Core.JsInterop
open Ionide.VSCode.FSharp
// open Node.Api

let activate (context : vscode.ExtensionContext) =
    printfn "sdf1"
    LanguageService.start context
    |> Promise.catch (fun error ->
            vscode.window.showErrorMessage (sprintf "error %A" error) |> ignore
        ) // prevent unhandled rejected promises
    |> Promise.map (fun () ->
        // QuickInfo.activate context
        HighlightingProvider.activate context
        // Project.activate context
        ()
    )
let deactivate(disposables : vscode.Disposable[]) =
    LanguageService.stop ()

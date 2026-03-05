import LeanFormalize.Query
import LeanFormalize.ProviderOption
import Lean
open Lean Meta Elab Command Tactic

namespace LeanFormalize

syntax (name := formalize_command) "#formalize" str : command

set_option formalize.key "AIzaSyCvanvHzlrDYICDjTThsQzUPA82SQl5uW8"

private def tryCatchRuntimeEx (tryBody : CommandElabM α) (catchBody : Exception → CommandElabM α) := do
  liftCoreM <|
  Lean.Core.tryCatchRuntimeEx (do tryBody |> liftCommandElabM) (fun e => do (catchBody e) |> liftCommandElabM)

private def mkSuggestionsMessage
  (suggestions : Array Hint.Suggestion)
  (ref : Syntax)
  (codeActionPrefix? : Option String)
  (forceList : Bool) : CommandElabM MessageData :=
    liftCoreM <| do
      Hint.mkSuggestionsMessage suggestions ref codeActionPrefix? forceList

open Parser in
private partial def parseState
  (env : Environment)
  (p : ParserFn)
  (ictx : InputContext)
  (s : ParserState) : Except String (List Syntax) :=
  let s' := p.run ictx { env, options := {} } (getTokenTable env) s
  if !s'.allErrors.isEmpty then
    do return []
  else if ictx.atEnd s'.pos then do
    return [s'.stxStack.back]
  else do
    return ([s'.stxStack.back] ++ (← parseState env p ictx s'))

open Parser in
private def runParserCategory (env : Environment) (catName : Name) (input : String) (fileName := "<input>") : Except String (List Syntax) :=
  let p := andthenFn whitespace (categoryParserFnImpl catName)
  let ictx := mkInputContext input fileName
  parseState env p ictx (mkParserState input)

private def generateFormalization (prompt : String) (previousCode : Option String := none) (errorLog : Option String := none) (attempts : Nat := 2) : CommandElabM String := do
  withIncRecDepth <| do
  let opts ← getOptions
  let key := formalize.key.get opts
  let provider := formalize.provider.get opts
  let (some provider) := Provider.fromString provider | throwError "Provider {provider} is not valid. Please set formalize.provider to a valid value"
  let prompt := match previousCode, errorLog with
  | .none, _ => prompt
  | _, .none => prompt
  | .some previousCode, .some errorLog => s!"Previous Code: {previousCode} Previous Error: {errorLog}  Query: {prompt}"
  let code ← request provider key prompt
  let stxs := runParserCategory (← getEnv) `command code
  let throwOrAttempt (e : String) : CommandElabM String := if attempts ≤ 1 then throwError e else generateFormalization prompt (some code) (some e) (attempts - 1)
  let logOrAttempt (e : String) : CommandElabM String := if attempts ≤ 1 then (do
    if let .none := errorLog then
      logError e
    return code) else generateFormalization prompt (some code) (some e) (attempts - 1)
  let (.ok stxs) := stxs | s!"{stxs}" |> throwOrAttempt
  if stxs.length = 0 then
    throwError "Failed to parse"
  let tryBody := do
    for stx in stxs do
      elabCommand stx
    return code
  let exceptBody : Exception → CommandElabM String := fun e => do
    let e ← e.toMessageData.toString
    s!"{e}" |> logOrAttempt
  tryCatchRuntimeEx tryBody exceptBody

@[command_elab formalize_command]
meta def elabFormalizeCommand : CommandElab := fun stx =>
  match stx with
  | `(command|#formalize $prompt:str) => do
    let code ← generateFormalization prompt.getString
    let suggestion : Hint.Suggestion := {
      suggestion := TryThis.SuggestionText.string code
    }
    let message ← mkSuggestionsMessage #[suggestion] stx none false
    logInfo m!"Try This: {message}"
  | _ =>
    throwUnsupportedSyntax

end LeanFormalize

import Lean
open Lean Elab Meta Command

def CommandElabM.tryCatchRuntimeEx (tryBody : CommandElabM α) (catchBody : Exception → CommandElabM α) := do
  liftCoreM <|
  Lean.Core.tryCatchRuntimeEx (do tryBody |> liftCommandElabM) (fun e => do (catchBody e) |> liftCommandElabM)

syntax (name := test_cmd) "#test" : command

elab "#test" : command => do
  let code := "\ndef add (a b : Nat) : Nat :=\na + b\n#eval add 2 3\n"
  let stx := Parser.runParserCategory (← getEnv) `command code
  let (.ok stx) := stx | throwError m!"{stx}"
  CommandElabM.tryCatchRuntimeEx (do elabCommand stx) (fun e => do throwError m!"{e.toMessageData}")

#test


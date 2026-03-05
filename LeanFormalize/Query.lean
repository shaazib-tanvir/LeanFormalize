import Lean.Data.Json
import Lean.Data.Json.Parser
open Lean Json IO

namespace LeanFormalize

inductive Provider where
  | gemini

def Provider.fromString (s : String) : Option Provider :=
  match s with
  | "gemini" => some Provider.gemini
  | _ => none

private def SYSTEM_PROMPT := "You are an expert in Lean4 Programming. The user will ask queries about formalizing and writing Lean4 programs. You have to give only the program as the response without any additional text. Be sure to wrap code around ```lean and end with ```."

private def extractProgram (response : String) : Option String := do
  let CODE_START := "```lean"
  let startPos ← response.find? CODE_START
  let substr := response.sliceFrom startPos |>.drop CODE_START.length |>.toString
  let CODE_END := "```"
  let endPos ← substr.find? CODE_END
  return substr.sliceTo endPos |>.toString

private def requestGemini (key : String) (prompt : String) : IO String := do
  let url := "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.1-flash-lite-preview:generateContent"
  let jsonPayload := s!"\{\"system_instruction\": \{\"parts\": [\{\"text\": \"{SYSTEM_PROMPT}\"}]}, \"contents\": [\{\"parts\": [\{\"text\": \"{prompt}\"}]}]}"
  
  let args : Process.SpawnArgs :=
  {
    cmd := "curl",
    args :=
    #[
      url,
      "-H", s!"x-goog-api-key: {key}",
      "-H", "Content-Type: application/json",
      "-X", "POST",
      "-d", jsonPayload
    ]
  }
 
  let jsonString ← Process.run args
  let jsonE := parse jsonString
  match jsonE with
  | .ok json =>
    let code := do
      let candidates ← json.getObjVal? "candidates"
      let candidate ← candidates.getArrVal? 0
      let content ← candidate.getObjVal? "content"
      let parts ← content.getObjVal? "parts"
      let part ← parts.getArrVal? 0
      let code ← part.getObjVal? "text"
      return ← code.getStr?
    match code with
    | .ok code =>
      let program := extractProgram code
      match program with
      | .some program =>
        return program
      | .none =>
        throw (IO.Error.userError "No valid program in response")
    | .error e =>
      throw (IO.Error.userError e)
  | .error e =>
    throw (IO.Error.userError e)

def request (provider : Provider) (key : String) (prompt : String) : IO String :=
  match provider with
  | .gemini =>
    requestGemini key prompt

private def GEMINI_API_KEY := "AIzaSyCvanvHzlrDYICDjTThsQzUPA82SQl5uW8"

end LeanFormalize

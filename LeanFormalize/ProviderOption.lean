import Lean

register_option formalize.key : String := {
  defValue := ""
  descr := "the api key to use"
}

register_option formalize.provider : String := {
  defValue := "gemini"
  descr := "which api provider to use for the commands"
}

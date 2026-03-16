import Lean

register_option formalize.attempts : Nat := {
  defValue := 2
  descr := "the maximum number of attempts to do in #formalize command"
}


import
  types

cop :
  type
    CaseEquality* = ref object of Cop
  const
    MSG = "Avoid the use of the case equality operator `===`."
  nodeMatcher isCaseEquality, "(send _ :=== _)"
  method onSend*(self: CaseEquality; node: Node): void =
    isCaseEquality node:
      addOffense(node, location = "selector")


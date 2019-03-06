
type
  CaseEquality* = ref object of Cop
    options*: Hash
    offenses*: Array
    processedSource*: NilClass
    corrections*: Array
    correctedNodes*: Hash
    config*: Config

const
  MSG = "Avoid the use of the case equality operator `===`."
nodeMatcher isCaseEquality, "(send _ :=== _)"
proc onSend*(self: CaseEquality; node: Node): void =
  isCaseEquality(node, proc (): void =
    addOffense(node, location = "selector"))


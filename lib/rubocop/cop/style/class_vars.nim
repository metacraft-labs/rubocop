
type
  ClassVars* = object
proc onCvasgn*(self: Class; node: void): void =
  addOffense(node, location = "name")

proc message*(self: Class; node: void): void =
  var  = node[0]
  format(classVar = classVar)


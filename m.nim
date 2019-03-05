import macros, strformat

const msg = "Replace class var %<class_var>s with a class \n" &
              "instance var."

macro format(msg: static[string], args: varargs[untyped]): untyped =
  result = quote do: &""
  var newMsg = ""
  var i = 0
  var inToken = false
  while i < msg.len:
    var m = msg[i]
    if m == '%' and i < msg.len - 1 and msg[i + 1] == '<' and not inToken:
      newMsg.add('{')
      i += 2
      inToken = true
    elif m == '>' and inToken:
      newMsg.add('}')
      i += 1
      inToken = false
    else:
      newMsg.add(m)
      i += 1

  result[1] = newLit(newMsg)
  let msgNode = result
  result = nnkBlockStmt.newTree(newEmptyNode(), nnkStmtList.newTree())
  for arg in args:
    var left = arg[0]
    var right = arg[1]
    var q = quote:
      let `left` = `right`
    result[1].add(q)
  result[1].add(msgNode)
  echo msgNode.repr
  echo result.repr

var b = "@@g"

echo format(msg, classVar=b)





# frozen_string_literal: true

  # This class performs a pattern-matching operation on an AST node.
  #
  # Initialize a new `NodePattern` with `NodePattern.new(pattern_string)`, then
  # pass an AST node to `NodePattern#match`. Alternatively, use one of the class
  # macros in `NodePattern::Macros` to define your own pattern-matching method.
  #
  # If the match fails, `nil` will be returned. If the match succeeds, the
  # return value depends on whether a block was provided to `#match`, and
  # whether the pattern contained any "captures" (values which are extracted
  # from a matching AST.)
  #
  # - With block: #match yields the captures (if any) and passes the return
  #               value of the block through.
  # - With no block, but one capture: the capture is returned.
  # - With no block, but multiple captures: captures are returned as an array.
  # - With no block and no captures: #match returns `true`.
  #
  # ## Pattern string format examples
  #
  #     ':sym'              # matches a literal symbol
  #     '1'                 # matches a literal integer
  #     'nil'               # matches a literal nil
  #     'send'              # matches (send ...)
  #     '(send)'            # matches (send)
  #     '(send ...)'        # matches (send ...)
  #     '(op-asgn)'         # node types with hyphenated names also work
  #     '{send class}'      # matches (send ...) or (class ...)
  #     '({send class})'    # matches (send) or (class)
  #     '(send const)'      # matches (send (const ...))
  #     '(send _ :new)'     # matches (send <anything> :new)
  #     '(send $_ :new)'    # as above, but whatever matches the $_ is captured
  #     '(send $_ $_)'      # you can use as many captures as you want
  #     '(send !const ...)' # ! negates the next part of the pattern
  #     '$(send const ...)' # arbitrary matching can be performed on a capture
  #     '(send _recv _msg)' # wildcards can be named (for readability)
  #     '(send ... :new)'   # you can specifically match against the last child
  #                         # (this only works for the very last)
  #     '(send $...)'       # capture all the children as an array
  #     '(send $... int)'   # capture all children but the last as an array
  #     '(send _x :+ _x)'   # unification is performed on named wildcards
  #                         # (like Prolog variables...)
  #                         # (#== is used to see if values unify)
  #     '(int odd?)'        # words which end with a ? are predicate methods,
  #                         # are are called on the target to see if it matches
  #                         # any Ruby method which the matched object supports
  #                         # can be used
  #                         # if a truthy value is returned, the match succeeds
  #     '(int [!1 !2])'     # [] contains multiple patterns, ALL of which must
  #                         # match in that position
  #                         # in other words, while {} is pattern union (logical
  #                         # OR), [] is intersection (logical AND)
  #     '(send %1 _)'       # % stands for a parameter which must be supplied to
  #                         # #match at matching time
  #                         # it will be compared to the corresponding value in
  #                         # the AST using #==
  #                         # a bare '%' is the same as '%1'
  #                         # the number of extra parameters passed to #match
  #                         # must equal the highest % value in the pattern
  #                         # for consistency, %0 is the 'root node' which is
  #                         # passed as the 1st argument to #match, where the
  #                         # matching process starts
  #     '^^send'            # each ^ ascends one level in the AST
  #                         # so this matches against the grandparent node
  #     '#method'           # we call this a 'funcall'; it calls a method in the
  #                         # context where a pattern-matching method is defined
  #                         # if that returns a truthy value, the match succeeds
  #     'equal?(%1)'        # predicates can be given 1 or more extra args
  #     '#method(%0, 1)'    # funcalls can also be given 1 or more extra args
  #
  # You can nest arbitrarily deep:
  #
  #     # matches node parsed from 'Const = Class.new' or 'Const = Module.new':
  #     '(casgn nil? :Const (send (const nil? {:Class :Module}) :new))'
  #     # matches a node parsed from an 'if', with a '==' comparison,
  #     # and no 'else' branch:
  #     '(if (send _ :== _) _ nil?)'
  #
  # Note that patterns like 'send' are implemented by calling `#send_type?` on
  # the node being matched, 'const' by `#const_type?`, 'int' by `#int_type?`,
  # and so on. Therefore, if you add methods which are named like
  # `#prefix_type?` to the AST node class, then 'prefix' will become usable as
  # a pattern.
  #
  # Also note that if you need a "guard clause" to protect against possible nils
  # in a certain place in the AST, you can do it like this: `[!nil <pattern>]`
  #
  # The compiler code is very simple; don't be afraid to read through it!

import macros, strformat

type
  NodePattern* = ref object

  Compiler* = ref object
    text*: string
    root*: string
    temp*: int
    captures*: int
    unify*: Table[string, int]
    params*: int
    tokens*: seq[string]
    matchCode*: NimNode

const
  SYMBOL       = re"{:(?:[\w+@*/?!<>=~|%^-]+|\[\]=?)}"
  IDENTIFIER   = re"[a-zA-Z_-]"
  META         = re"\(|\)|\{|\}|\[|\]|\$\.\.\.|\$|!|\^|\.\.\."
  NUMBER       = re"-?\d+(?:\.\d+)?"
  # STRING       = /".+?"/.freeze
  #   METHOD_NAME  = /\#?#{IDENTIFIER}+[\!\?]?\(?/.freeze
  #   PARAM_NUMBER = /%\d*/.freeze

  #   SEPARATORS = /[\s]+/.freeze
  #   TOKENS     = Regexp.union(META, PARAM_NUMBER, NUMBER,
  #                             METHOD_NAME, SYMBOL, STRING)

  #   TOKEN = /\G(?:#{SEPARATORS}|#{TOKENS}|.)/.freeze

  #   NODE      = /\A#{IDENTIFIER}+\Z/.freeze
  #   PREDICATE = /\A#{IDENTIFIER}+\?\(?\Z/.freeze
  #   WILDCARD  = /\A_#{IDENTIFIER}*\Z/.freeze
  #   FUNCALL   = /\A\##{METHOD_NAME}/.freeze
  #   LITERAL   = /\A(?:#{SYMBOL}|#{NUMBER}|#{STRING})\Z/.freeze
  #   PARAM     = /\A#{PARAM_NUMBER}\Z/.freeze
  #   CLOSING   = /\A(?:\)|\}|\])\Z/.freeze

proc compileExpr(compiler: Compiler, node: NimNode, head: bool): NimNode
  
macro nodeMatcher*(methodName: static[string], pattern: static[string]): untyped =
  result = generateMatch(pattern, "node")
  let name = ident(methodName)
  let node = ident("node")
  result = quote:
    template `name`(`node`: untyped, bl: untyped): untyped =
      if `result`:
        `bl`

proc tokenize(text: string): seq[string] =
  @["(", "send", "_", ":===", "_", ")"]

proc generateMatch(text: string, root: string): NimNode =
  var compiler = Compiler(text: text, root: root,  temp: 0, captures: 0, unify: initTable[string, int](), params: 0)
  compiler.tokens = tokenize(compiler.text)
  result = compileExpr(compiler, ident(compiler.root), false)

  

using
  compiler: Compiler
  node: Node

proc compileSeq(compiler, node; head: bool): NimNode =
  if compiler.tokens.len == 0 or compiler.tokens[0] == ")":
    error("empty")
  elif head:
    error("head")
    
  # 'cur_node' is a Ruby expression which evaluates to an AST node,
  # but we don't know how expensive it is
  # to be safe, cache the node in a temp variable and then use the
  # temp variable as 'cur_node'
  let temp = ident("temp" & $compiler.temp)
  compiler.temp += 1
  result = quote:
    let `temp` = `node`; true
  var index = -1
  var terms: seq[NimNode] = @[]
  var size = 0
  while compiler.tokens.len > 0 and compiler.tokens[0] != ")":
    let token = compiler.tokens[0]
    compiler.tokens = compiler.tokens[1 .. ^1]
    if index == -1:
      terms.add(compileExpr(compiler, node, true))
    else:
      let childNode = quote do: `node`[`index`]
      terms.add(compileExpr(compiler, childNode, false))
      size += 1
  if compiler.tokens.len > 0:
    compiler.tokens = compiler.tokens[1 .. ^1]
  let sizeNode = newLit(size)
  terms.add(quote do: `node`.len == `sizeNode`)

  for term in terms:
    result = quote do: `result` and `term`

      
      # def compile_capt_ellip(tokens, cur_node, terms, index)
      #   capture = next_capture
      #   if (term = compile_seq_tail(tokens, "#{cur_node}.children.last"))
      #     terms << "(#{cur_node}.children.size > #{index})"
      #     terms << term
      #     terms << "(#{capture} = #{cur_node}.children[#{index}..-2])"
      #   else
      #     terms << "(#{cur_node}.children.size >= #{index})" if index > 0
      #     terms << "(#{capture} = #{cur_node}.children[#{index}..-1])"
      #   end
      #   terms
      # end

      # def compile_seq_tail(tokens, cur_node)
      #   tokens.shift
      #   if tokens.first == ')'
      #     tokens.shift
      #     nil
      #   else
      #     expr = compile_expr(tokens, cur_node, false)
      #     fail_due_to('missing )') unless tokens.shift == ')'
      #     expr
      #   end
      # end

proc compileUnion(compiler, node; head: bool): NimNode =
  if compiler.tokens.len == 0 or compiler.tokens[0] == "}":
    error("empty")
  let temp = ident("temp" & $compiler.temp)
  compiler.temp += 1
  var init = quote:
    let `temp` = `node`; true
  
  var terms: seq[NimNode] = @[]
  while compiler.tokens.len > 0 and compiler.tokens[0] != "}":
    let token = compiler.tokens[0]
    compiler.tokens = compiler.tokens[1 .. ^1]
    if index == -1:
      terms.add(compileExpr(compiler, node, true))
    else:
      let childNode = quote do: `node`[`index`]
      terms.add(compileExpr(compiler, childNode, false))
  if compiler.tokens.len > 0:
    compiler.tokens = compiler.tokens[1 .. ^1]
    
  result = quote do: false
  for term in terms:
    result = quote do: `result` or `term`
  result = quote do: `init` and `result`

proc compileIntersect(compiler, node; head: bool): NimNode =
  if compiler.tokens.len == 0 or compiler.tokens[0] == "]":
    error("empty")
  let temp = ident("temp" & $compiler.temp)
  compiler.temp += 1
  var init = quote:
    let `temp` = `node`; true
  
  var terms: seq[NimNode] = @[]
  while compiler.tokens.len > 0 and compiler.tokens[0] != "}":
    let token = compiler.tokens[0]
    compiler.tokens = compiler.tokens[1 .. ^1]
    if index == -1:
      terms.add(compileExpr(compiler, node, true))
    else:
      let childNode = quote do: `node`[`index`]
      terms.add(compileExpr(compiler, childNode, false))
  if compiler.tokens.len > 0:
    compiler.tokens = compiler.tokens[1 .. ^1]
    
   result = init
   for term in terms:
    result = quote do: `result` and `term`
          

proc compileCapture(compiler, node; head: bool): NimNode =
  let nextNode = compiler.nextCapture

  if head:
    result = quote:
      let `nextNode` = `node`.typ; true
  else:
    result = quote:
      let `nextNode` = `node`
  let n0 = compileExpr(compiler, node, head)
  result = quote do: `result` and `n0`

proc compileNegation(compiler, node; head: bool): NimNode =
  let n0 = compileExpr(compiler, node, head)
  result = quote do: not `n0`

proc compileAscend(compiler, node; head: bool): NimNode =
  let n0 = quote do: not `node`.parent.isNil
  let n1 = compileExpr(compiler, quote do: `n0`.parent, head)
  result = quote do: `n0` and `n1`

proc compileWildcard(compiler; name: string, head: bool): NimNode =
  if name == "":
    ident("true")
  elif compiler.unify.hasKey(name):
    # (from ruby):
    # we have already seen a wildcard with this name before
    # so the value it matched the first time will already be stored
    # in a temp. check if this value matches the one stored in the temp
    let n0 = if head: (quote do: `node`.typ) else: (node)
    let n1 = ident("temp" & $compiler.unify[name])
    result = quote do: `n0` == `n1`
  else:
    let temp = ident("temp" & $compiler.temp)
    compiler.temp += 1
    compiler.unify[name] = $temp
    let n0 = if head: (quote do: `node`.typ) else: (node)
    result  = quote do: let `temp` = `n0`; true


# proc compileLiteral(node; literal: string, head: bool): cur_node, literal, seq_head)

proc compileExpr(compiler: Compiler, node: NimNode, head: bool): NimNode =
  # read a single pattern-matching expression from the token stream,
  # return Ruby code which performs the corresponding matching operation
  # on 'cur_node' (which is Ruby code which evaluates to an AST node)
  #
  # the 'pattern-matching' expression may be a composite which
  # contains an arbitrary number of sub-expressions
  let token = compiler.tokens[0]
  compiler.tokens = compiler.tokens[1 .. ^1]
  case token:
  of "(":       compileSeq(compiler, node, head)
  of "{":       compileUnion(compiler, node, head)
  of "[":       compileIntersect(compiler, node, head)
  of "!":       compileNegation(compiler, node, head)
  of "$":       compileCapture(compiler, node, head)
  of "^":       compileAscend(compiler, node, head)
  of WILDCARD:  compileWildcard(compiler, token[1..^1], head)
  of FUNCALL:   compileFuncall(compiler, node, token, head)
  of LITERAL:   compileLiteral(compiler, token, head)
  of PREDICATE: compilePredicate(compiler, node, token, head)
  of NODE:      compileNodetype(node, token)
  of PARAM:     compileParam(node, token[1..-1], head)
  of CLOSING:   error("token position")
  else:         error("token error")

#       def compile_predicate(tokens, cur_node, predicate, seq_head)
#         if predicate.end_with?('(') # is there an arglist?
#           args = compile_args(tokens)
#           predicate = predicate[0..-2] # drop the trailing (
#           "(#{cur_node}#{'.type' if seq_head}.#{predicate}(#{args.join(',')}))"
#         else
#           "(#{cur_node}#{'.type' if seq_head}.#{predicate})"
#         end
#       end

#       def compile_funcall(tokens, cur_node, method, seq_head)
#         # call a method in the context which this pattern-matching
#         # code is used in. pass target value as an argument
#         method = method[1..-1] # drop the leading #
#         if method.end_with?('(') # is there an arglist?
#           args = compile_args(tokens)
#           method = method[0..-2] # drop the trailing (
#           "(#{method}(#{cur_node}#{'.type' if seq_head},#{args.join(',')}))"
#         else
#           "(#{method}(#{cur_node}#{'.type' if seq_head}))"
#         end
#       end

#       def compile_nodetype(cur_node, type)
#         "(#{cur_node} && #{cur_node}.#{type.tr('-', '_')}_type?)"
#       end

#       def compile_param(cur_node, number, seq_head)
#         "(#{cur_node}#{'.type' if seq_head} == #{get_param(number)})"
#       end

#       def compile_args(tokens)
#         index = tokens.find_index { |token| token == ')' }

#         tokens.slice!(0..index).each_with_object([]) do |token, args|
#           next if [')', ','].include?(token)

#           args << compile_arg(token)
#         end
#       end

#       def compile_arg(token)
#         case token
#         when WILDCARD  then
#           name   = token[1..-1]
#           number = @unify[name] || fail_due_to('invalid in arglist: ' + token)
#           "temp#{number}"
#         when LITERAL   then token
#         when PARAM     then get_param(token[1..-1])
#         when CLOSING   then fail_due_to("#{token} in invalid position")
#         when nil       then fail_due_to('pattern ended prematurely')
#         else fail_due_to("invalid token in arglist: #{token.inspect}")
#         end
#       end

#       def next_capture
#         "capture#{@captures += 1}"
#       end

#       def get_param(number)
#         number = number.empty? ? 1 : Integer(number)
#         @params = number if number > @params
#         number.zero? ? @root : "param#{number}"
#       end

#       def join_terms(init, terms, operator)
#         "(#{init};#{terms.join(operator)})"
#       end

#       def emit_capture_list
#         (1..@captures).map { |n| "capture#{n}" }.join(',')
#       end

#       def emit_retval
#         if @captures.zero?
#           'true'
#         elsif @captures == 1
#           'capture1'
#         else
#           "[#{emit_capture_list}]"
#         end
#       end

#       def emit_param_list
#         (1..@params).map { |n| "param#{n}" }.join(',')
#       end

#       def emit_trailing_params
#         params = emit_param_list
#         params.empty? ? '' : ",#{params}"
#       end

#       def emit_guard_clause
#         <<-RUBY
#           return unless node.is_a?(RuboCop::AST::Node)
#         RUBY
#       end

#       def emit_method_code
#         <<-RUBY
#           return unless #{@match_code}
#           block_given? ? yield(#{emit_capture_list}) : (return #{emit_retval})
#         RUBY
#       end

#       def fail_due_to(message)
#         raise Invalid, "Couldn't compile due to #{message}. Pattern: #{@string}"
#       end

#       def with_temp_node(cur_node)
#         with_temp_variable do |temp_var|
#           # double assign to temp#{n} to avoid "assigned but unused variable"
#           yield "#{temp_var} = #{cur_node}; #{temp_var} = #{temp_var}", temp_var
#         end
#       end

#       def with_temp_variable
#         yield "temp#{next_temp_value}"
#       end

#       def next_temp_value
#         @temps += 1
#       end
#     end
#     private_constant :Compiler

#     # Helpers for defining methods based on a pattern string
#     module Macros
#       # Define a method which applies a pattern to an AST node
#       #
#       # The new method will return nil if the node does not match
#       # If the node matches, and a block is provided, the new method will
#       # yield to the block (passing any captures as block arguments).
#       # If the node matches, and no block is provided, the new method will
#       # return the captures, or `true` if there were none.
#       def def_node_matcher(method_name, pattern_str)
#         compiler = Compiler.new(pattern_str, 'node')
#         src = "def #{method_name}(node = self" \
#               "#{compiler.emit_trailing_params});" \
#               "#{compiler.emit_guard_clause}" \
#               "#{compiler.emit_method_code};end"

#         location = caller_locations(1, 1).first
#         class_eval(src, location.path, location.lineno)
#       end

#       # Define a method which recurses over the descendants of an AST node,
#       # checking whether any of them match the provided pattern
#       #
#       # If the method name ends with '?', the new method will return `true`
#       # as soon as it finds a descendant which matches. Otherwise, it will
#       # yield all descendants which match.
#       def def_node_search(method_name, pattern_str)
#         compiler = Compiler.new(pattern_str, 'node')
#         called_from = caller(1..1).first.split(':')

#         if method_name.to_s.end_with?('?')
#           node_search_first(method_name, compiler, called_from)
#         else
#           node_search_all(method_name, compiler, called_from)
#         end
#       end

#       def node_search_first(method_name, compiler, called_from)
#         node_search(method_name, compiler, 'return true', '', called_from)
#       end

#       def node_search_all(method_name, compiler, called_from)
#         yieldval = compiler.emit_capture_list
#         yieldval = 'node' if yieldval.empty?
#         prelude = "return enum_for(:#{method_name}, node0" \
#                   "#{compiler.emit_trailing_params}) unless block_given?"

#         node_search(method_name, compiler, "yield(#{yieldval})", prelude,
#                     called_from)
#       end

#       def node_search(method_name, compiler, on_match, prelude, called_from)
#         src = node_search_body(method_name, compiler.emit_trailing_params,
#                                prelude, compiler.match_code, on_match)
#         filename, lineno = *called_from
#         class_eval(src, filename, lineno.to_i)
#       end

#       def node_search_body(method_name, trailing_params, prelude, match_code,
#                            on_match)
#         <<-RUBY
#           def #{method_name}(node0#{trailing_params})
#             #{prelude}
#             node0.each_node do |node|
#               if #{match_code}
#                 #{on_match}
#               end
#             end
#             nil
#           end
#         RUBY
#       end
#     end

#     def initialize(str)
#       compiler = Compiler.new(str)
#       src = "def match(node0#{compiler.emit_trailing_params});" \
#             "#{compiler.emit_method_code}end"
#       instance_eval(src)
#     end
#   end
# end

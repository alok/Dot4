import Dot4.Basic
import Dot4.Parser.AST
import Dot4.Parser.Lexer
import Dot4.Parser.Parser
import Dot4.Parser.Convert

/-!
# DOT Parser

Main entry point for parsing DOT files into Dot4 Graph structures.
Use Graph.fromDot to parse a DOT string into a Graph.
-/

namespace Dot4

/-- Parse a DOT string into a Graph -/
def Graph.fromDot (input : String) : Except String Graph :=
  match Parser.parseString input with
  | .ok ast => .ok (Parser.astToGraph ast)
  | .error msg => .error msg

/-- Parse a DOT string into a GraphAST (for advanced use) -/
def Graph.fromDotAST (input : String) : Except String Parser.GraphAST :=
  Parser.parseString input

end Dot4

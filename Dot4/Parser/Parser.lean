import Std.Internal.Parsec
import Std.Internal.Parsec.String
import Dot4.Parser.AST
import Dot4.Parser.Lexer

/-!
# DOT Parser

Parsec-based parser for DOT language that produces AST nodes.
-/

namespace Dot4.Parser

open Std.Internal.Parsec String

set_option linter.missingDocs false

/-- Parse optional trailing semicolon or comma -/
def optSep : Parser Unit := do
  skipWsAndComments
  (pchar ';' *> pure ()) <|> (pchar ',' *> pure ()) <|> pure ()

/-- Parse an identifier (quoted or unquoted) -/
def parseId : Parser String := do
  skipWsAndComments
  quotedString <|> unquotedIdent

/-- Check if string is a compass point -/
def isCompassPoint (s : String) : Bool :=
  s == "n" || s == "ne" || s == "e" || s == "se" ||
  s == "s" || s == "sw" || s == "w" || s == "nw" ||
  s == "c" || s == "_"

/-- Parse a compass point -/
def parseCompass : Parser String := do
  let id ← unquotedIdent
  if isCompassPoint id then
    pure id
  else
    fail s!"expected compass point, got: {id}"

/-- Parse port specification -/
def parsePort : Parser PortAST := do
  skipWsAndComments
  let _ ← pchar ':'
  skipWsAndComments
  -- Try compass first (single letter)
  let name ← unquotedIdent
  skipWsAndComments
  -- Check for second colon (compass after port name)
  let compass ← (pchar ':' *> skipWsAndComments *> parseCompass >>= fun cp => pure (Option.some cp)) <|> pure Option.none
  -- If no compass and name is a compass point, treat as compass
  if compass.isNone && isCompassPoint name then
    pure { compass := Option.some name }
  else
    pure { name := Option.some name, compass }

/-- Parse node ID with optional port -/
def parseNodeId : Parser NodeIdAST := do
  let id ← parseId
  skipWsAndComments
  -- Try to parse port
  let port ← (do
    let p ← parsePort
    pure (Option.some p)) <|> pure Option.none
  pure { id, port }

/-- Parse a single attribute: key = value -/
def parseAttr : Parser AttrAST := do
  skipWsAndComments
  let key ← parseId
  skipWsAndComments
  let _ ← pchar '='
  skipWsAndComments
  let value ← parseId <|> numericLiteral
  pure { key, value }

/-- Parse attribute list contents (simplified) -/
partial def parseAttrsLoop (acc : List AttrAST) : Parser (List AttrAST) := do
  skipWsAndComments
  (do
    let attr ← parseAttr
    skipWsAndComments
    let _ ← (pchar ',' <|> pchar ';') <|> pure ' '
    parseAttrsLoop (acc ++ [attr])) <|> pure acc

def parseAttrs : Parser (List AttrAST) := parseAttrsLoop []

/-- Parse optional attribute list or empty -/
partial def parseOptAttrsLoop (acc : List AttrAST) : Parser (List AttrAST) := do
  skipWsAndComments
  (do
    let _ ← pchar '['
    skipWsAndComments
    let attrs ← parseAttrs
    skipWsAndComments
    let _ ← pchar ']'
    parseOptAttrsLoop (acc ++ attrs)) <|> pure acc

def parseOptAttrs : Parser (List AttrAST) := parseOptAttrsLoop []

/-- Parse edge RHS: additional nodes after arrow -/
partial def parseEdgeRHSLoop (directed : Bool) (acc : List NodeIdAST) : Parser (List NodeIdAST) := do
  skipWsAndComments
  let arrow := if directed then "->" else "--"
  (do
    let _ ← pstring arrow
    skipWsAndComments
    let node ← parseNodeId
    parseEdgeRHSLoop directed (acc ++ [node])) <|> pure acc

def parseEdgeRHS (directed : Bool) : Parser (List NodeIdAST) := parseEdgeRHSLoop directed []

/-- Parse attribute target: graph, node, or edge -/
def parseAttrTarget : Parser AttrTarget := do
  skipWsAndComments
  (pstring "graph" *> pure .graph) <|>
  (pstring "node" *> pure .node) <|>
  (pstring "edge" *> pure .edge)

/-- Parse a simple statement (no subgraph) -/
def parseSimpleStmt (directed : Bool) : Parser SimpleStmtAST := do
  skipWsAndComments
  -- Try attribute statement first: graph/node/edge [attrs]
  (do
    let target ← parseAttrTarget
    skipWsAndComments
    let attrs ← parseOptAttrs
    pure (SimpleStmtAST.attrStmt target attrs)) <|>
  -- Try node/edge statement
  (do
    let firstNode ← parseNodeId
    skipWsAndComments
    -- Check for edge
    let arrow := if directed then "->" else "--"
    (do
      let _ ← pstring arrow
      skipWsAndComments
      let secondNode ← parseNodeId
      let rest ← parseEdgeRHS directed
      skipWsAndComments
      let attrs ← parseOptAttrs
      pure (SimpleStmtAST.edgeStmt ([firstNode, secondNode] ++ rest) attrs)) <|>
    -- Check for assignment (top-level key=value)
    (do
      skipWsAndComments
      let _ ← pchar '='
      skipWsAndComments
      let value ← parseId <|> numericLiteral
      pure (SimpleStmtAST.assignment firstNode.id value)) <|>
    -- Just a node statement
    (do
      skipWsAndComments
      let attrs ← parseOptAttrs
      pure (SimpleStmtAST.nodeStmt firstNode attrs)))

/-- Parse simple statement list (inside subgraph) -/
partial def parseSimpleStmtListLoop (directed : Bool) (acc : List SimpleStmtAST) : Parser (List SimpleStmtAST) := do
  skipWsAndComments
  (do
    let stmt ← parseSimpleStmt directed
    optSep
    parseSimpleStmtListLoop directed (acc ++ [stmt])) <|> pure acc

def parseSimpleStmtList (directed : Bool) : Parser (List SimpleStmtAST) := parseSimpleStmtListLoop directed []

/-- Parse subgraph with optional name -/
def parseSubgraph (directed : Bool) : Parser SubgraphAST := do
  skipWsAndComments
  -- Optional 'subgraph' keyword
  let hasKeyword ← (pstring "subgraph" *> pure true) <|> pure false
  skipWsAndComments
  -- Optional name
  let name ← if hasKeyword then
    (parseId >>= fun n => pure (Option.some n)) <|> pure Option.none
  else
    pure Option.none
  skipWsAndComments
  let _ ← pchar '{'
  let stmts ← parseSimpleStmtList directed
  skipWsAndComments
  let _ ← pchar '}'
  pure { name, stmts }

/-- Parse a statement (simple or subgraph) -/
def parseStmt (directed : Bool) : Parser StmtAST := do
  skipWsAndComments
  -- Try subgraph first (starts with 'subgraph' or '{')
  (do
    let sg ← parseSubgraph directed
    pure (StmtAST.subgraph sg)) <|>
  -- Simple statement
  (do
    let s ← parseSimpleStmt directed
    pure (StmtAST.simple s))

/-- Parse statement list -/
partial def parseStmtListLoop (directed : Bool) (acc : List StmtAST) : Parser (List StmtAST) := do
  skipWsAndComments
  (do
    let stmt ← parseStmt directed
    optSep
    parseStmtListLoop directed (acc ++ [stmt])) <|> pure acc

def parseStmtList (directed : Bool) : Parser (List StmtAST) := parseStmtListLoop directed []

/-- Parse a complete DOT graph -/
def parseGraph : Parser GraphAST := do
  skipWsAndComments
  -- Optional 'strict'
  let strict ← (pstring "strict" *> ws *> pure true) <|> pure false
  skipWsAndComments
  -- 'digraph' or 'graph'
  let directed ← (pstring "digraph" *> pure true) <|> (pstring "graph" *> pure false)
  skipWsAndComments
  -- Optional name
  let name ← (parseId >>= fun n => pure (Option.some n)) <|> pure Option.none
  skipWsAndComments
  let _ ← pchar '{'
  let stmts ← parseStmtList directed
  skipWsAndComments
  let _ ← pchar '}'
  skipWsAndComments
  eof
  pure { strict, directed, name, stmts }

/-- Parse a DOT string into GraphAST -/
def parseString (input : String) : Except String GraphAST :=
  Parser.run parseGraph input

end Dot4.Parser

/-!
# DOT Parser AST

Intermediate Abstract Syntax Tree for parsing DOT files.
This closely mirrors the DOT grammar before conversion to Dot4.Graph.
-/

namespace Dot4.Parser

set_option linter.missingDocs false

/-- A key-value attribute pair -/
structure AttrAST where
  key : String
  value : String
  deriving Repr, BEq, Inhabited

/-- Port specification: :name, :compass, or :name:compass -/
structure PortAST where
  name : Option String := none
  compass : Option String := none
  deriving Repr, BEq, Inhabited

/-- Node identifier with optional port -/
structure NodeIdAST where
  id : String
  port : Option PortAST := none
  deriving Repr, BEq, Inhabited

/-- Target for attribute statements -/
inductive AttrTarget where
  | graph
  | node
  | edge
  deriving Repr, BEq, Inhabited

/-- A simple statement (non-recursive part) -/
inductive SimpleStmtAST where
  | nodeStmt (id : NodeIdAST) (attrs : List AttrAST)
  | edgeStmt (endpoints : List NodeIdAST) (attrs : List AttrAST)
  | attrStmt (target : AttrTarget) (attrs : List AttrAST)
  | assignment (key : String) (value : String)
  deriving Repr, BEq, Inhabited

/-- A subgraph definition (with simple statements only for now) -/
structure SubgraphAST where
  name : Option String := none
  stmts : List SimpleStmtAST := []
  deriving Repr, BEq, Inhabited

/-- A DOT statement (can be simple or a subgraph) -/
inductive StmtAST where
  | simple (s : SimpleStmtAST)
  | subgraph (sg : SubgraphAST)
  deriving Repr, BEq, Inhabited

/-- A complete DOT graph -/
structure GraphAST where
  strict : Bool := false
  directed : Bool := true
  name : Option String := none
  stmts : List StmtAST := []
  deriving Repr, Inhabited

namespace StmtAST

def nodeStmt (id : NodeIdAST) (attrs : List AttrAST) : StmtAST :=
  .simple (.nodeStmt id attrs)

def edgeStmt (endpoints : List NodeIdAST) (attrs : List AttrAST) : StmtAST :=
  .simple (.edgeStmt endpoints attrs)

def attrStmt (target : AttrTarget) (attrs : List AttrAST) : StmtAST :=
  .simple (.attrStmt target attrs)

def assignment (key value : String) : StmtAST :=
  .simple (.assignment key value)

end StmtAST

end Dot4.Parser

/-!
# Dot4: Graphviz DOT DSL for Lean 4

A type-safe DSL for generating Graphviz DOT diagrams.
-/

namespace Dot4

/-- Node or graph attribute -/
structure Attr where
  key : String
  value : String
  deriving Repr, BEq, Hashable

/-- A node in the graph -/
structure Node where
  id : String
  label : Option String := none
  attrs : List Attr := []
  deriving Repr

/-- An edge between nodes -/
structure Edge where
  src : String
  dst : String
  label : Option String := none
  attrs : List Attr := []
  deriving Repr

/-- Graph direction -/
inductive Direction where
  | directed   -- digraph
  | undirected -- graph
  deriving Repr, BEq

/-- A complete graph -/
structure Graph where
  name : String := "G"
  direction : Direction := .directed
  nodes : List Node := []
  edges : List Edge := []
  attrs : List Attr := []
  nodeDefaults : List Attr := []
  edgeDefaults : List Attr := []
  deriving Repr

namespace Attr

def shape (s : String) : Attr := ⟨"shape", s⟩
def color (c : String) : Attr := ⟨"color", c⟩
def fillcolor (c : String) : Attr := ⟨"fillcolor", c⟩
def fontcolor (c : String) : Attr := ⟨"fontcolor", c⟩
def fontname (f : String) : Attr := ⟨"fontname", f⟩
def fontsize (s : Nat) : Attr := ⟨"fontsize", toString s⟩
def style (s : String) : Attr := ⟨"style", s⟩
def bgcolor (c : String) : Attr := ⟨"bgcolor", c⟩
def rankdir (d : String) : Attr := ⟨"rankdir", d⟩
def arrowsize (s : Float) : Attr := ⟨"arrowsize", toString s⟩

end Attr

namespace Node

def new (id : String) : Node := { id }

def withLabel (n : Node) (l : String) : Node := { n with label := some l }

def withAttrs (n : Node) (as : List Attr) : Node := { n with attrs := n.attrs ++ as }

def withAttr (n : Node) (a : Attr) : Node := n.withAttrs [a]

end Node

namespace Edge

def new (src dst : String) : Edge := { src, dst }

def withLabel (e : Edge) (l : String) : Edge := { e with label := some l }

def withAttrs (e : Edge) (as : List Attr) : Edge := { e with attrs := e.attrs ++ as }

end Edge

namespace Graph

def empty : Graph := {}

def digraph (name : String := "G") : Graph :=
  { name, direction := .directed }

def graph (name : String := "G") : Graph :=
  { name, direction := .undirected }

def addNode (g : Graph) (n : Node) : Graph :=
  { g with nodes := g.nodes ++ [n] }

def addEdge (g : Graph) (e : Edge) : Graph :=
  { g with edges := g.edges ++ [e] }

def withAttr (g : Graph) (a : Attr) : Graph :=
  { g with attrs := g.attrs ++ [a] }

def withNodeDefaults (g : Graph) (as : List Attr) : Graph :=
  { g with nodeDefaults := g.nodeDefaults ++ as }

def withEdgeDefaults (g : Graph) (as : List Attr) : Graph :=
  { g with edgeDefaults := g.edgeDefaults ++ as }

end Graph

end Dot4

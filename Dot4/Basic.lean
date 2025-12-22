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

/-- Compass point for edge ports -/
inductive Compass where
  | n | ne | e | se | s | sw | w | nw | c
  deriving Repr, BEq

namespace Compass
def toString : Compass → String
  | n => "n" | ne => "ne" | e => "e" | se => "se"
  | s => "s" | sw => "sw" | w => "w" | nw => "nw" | c => "c"
end Compass

/-- Port specification for edge endpoints -/
structure Port where
  name : Option String := none     -- record port name
  compass : Option Compass := none -- compass direction
  deriving Repr, BEq

/-- An edge between nodes -/
structure Edge where
  src : String
  dst : String
  srcPort : Port := {}
  dstPort : Port := {}
  label : Option String := none
  attrs : List Attr := []
  /-- Cluster to route edge head to (requires compound=true on graph) -/
  lhead : Option String := none
  /-- Cluster to route edge tail from (requires compound=true on graph) -/
  ltail : Option String := none
  deriving Repr

/-- Graph direction -/
inductive Direction where
  | directed   -- digraph
  | undirected -- graph
  deriving Repr, BEq

/-- A subgraph (cluster when name starts with "cluster") -/
structure Subgraph where
  name : String
  nodes : List Node := []
  edges : List Edge := []
  attrs : List Attr := []
  deriving Repr

/-- A complete graph -/
structure Graph where
  name : String := "G"
  direction : Direction := .directed
  /-- Strict mode: no multi-edges between same node pair -/
  strict : Bool := false
  nodes : List Node := []
  edges : List Edge := []
  subgraphs : List Subgraph := []
  attrs : List Attr := []
  nodeDefaults : List Attr := []
  edgeDefaults : List Attr := []
  deriving Repr

namespace Attr

-- Common attributes
def shape (s : String) : Attr := ⟨"shape", s⟩
def color (c : String) : Attr := ⟨"color", c⟩
def fillcolor (c : String) : Attr := ⟨"fillcolor", c⟩
def fontcolor (c : String) : Attr := ⟨"fontcolor", c⟩
def fontname (f : String) : Attr := ⟨"fontname", f⟩
def fontsize (s : Nat) : Attr := ⟨"fontsize", toString s⟩
def style (s : String) : Attr := ⟨"style", s⟩
def bgcolor (c : String) : Attr := ⟨"bgcolor", c⟩
def rankdir (d : String) : Attr := ⟨"rankdir", d⟩

-- Edge arrow attributes
def arrowsize (s : Float) : Attr := ⟨"arrowsize", toString s⟩
def arrowhead (s : String) : Attr := ⟨"arrowhead", s⟩
def arrowtail (s : String) : Attr := ⟨"arrowtail", s⟩
def dir (d : String) : Attr := ⟨"dir", d⟩

-- Layout and positioning
def rank (r : String) : Attr := ⟨"rank", r⟩
def width (w : Float) : Attr := ⟨"width", toString w⟩
def height (h : Float) : Attr := ⟨"height", toString h⟩
def fixedsize (b : Bool) : Attr := ⟨"fixedsize", if b then "true" else "false"⟩
def pos (p : String) : Attr := ⟨"pos", p⟩
def margin (m : String) : Attr := ⟨"margin", m⟩

-- Edge routing
def constraint (b : Bool) : Attr := ⟨"constraint", if b then "true" else "false"⟩
def weight (w : Float) : Attr := ⟨"weight", toString w⟩
def minlen (n : Nat) : Attr := ⟨"minlen", toString n⟩
def headport (p : String) : Attr := ⟨"headport", p⟩
def tailport (p : String) : Attr := ⟨"tailport", p⟩

-- Visual styling
def penwidth (w : Float) : Attr := ⟨"penwidth", toString w⟩
def peripheries (n : Nat) : Attr := ⟨"peripheries", toString n⟩
def gradientangle (a : Nat) : Attr := ⟨"gradientangle", toString a⟩

-- Text/label
def xlabel (l : String) : Attr := ⟨"xlabel", l⟩
def headlabel (l : String) : Attr := ⟨"headlabel", l⟩
def taillabel (l : String) : Attr := ⟨"taillabel", l⟩
def tooltip (t : String) : Attr := ⟨"tooltip", t⟩
def labelloc (l : String) : Attr := ⟨"labelloc", l⟩
def labeljust (j : String) : Attr := ⟨"labeljust", j⟩

-- Graph layout engine
def layout (l : String) : Attr := ⟨"layout", l⟩
def splines (s : String) : Attr := ⟨"splines", s⟩
def overlap (o : String) : Attr := ⟨"overlap", o⟩
def nodesep (n : Float) : Attr := ⟨"nodesep", toString n⟩
def ranksep (r : Float) : Attr := ⟨"ranksep", toString r⟩
def concentrate (b : Bool) : Attr := ⟨"concentrate", if b then "true" else "false"⟩
def compound (b : Bool) : Attr := ⟨"compound", if b then "true" else "false"⟩

-- URLs and interaction
def href (u : String) : Attr := ⟨"href", u⟩
def url (u : String) : Attr := ⟨"URL", u⟩
def target (t : String) : Attr := ⟨"target", t⟩

-- Record nodes
def rects (r : String) : Attr := ⟨"rects", r⟩

end Attr

namespace Node

def new (id : String) : Node := { id }

def withLabel (n : Node) (l : String) : Node := { n with label := some l }

def withAttrs (n : Node) (as : List Attr) : Node := { n with attrs := n.attrs ++ as }

def withAttr (n : Node) (a : Attr) : Node := n.withAttrs [a]

end Node

namespace Port

def fromCompass (dir : Compass) : Port := { compass := some dir }
def fromName (n : String) : Port := { name := some n }
def mk' (n : String) (dir : Compass) : Port := { name := some n, compass := some dir }

end Port

namespace Edge

def new (src dst : String) : Edge := { src, dst }

def withLabel (e : Edge) (l : String) : Edge := { e with label := some l }

def withAttrs (e : Edge) (as : List Attr) : Edge := { e with attrs := e.attrs ++ as }

def withSrcPort (e : Edge) (p : Port) : Edge := { e with srcPort := p }

def withDstPort (e : Edge) (p : Port) : Edge := { e with dstPort := p }

def withPorts (e : Edge) (src dst : Port) : Edge := { e with srcPort := src, dstPort := dst }

/-- Route edge head to a cluster boundary (requires compound=true on graph) -/
def toCluster (e : Edge) (clusterName : String) : Edge :=
  { e with lhead := some ("cluster_" ++ clusterName) }

/-- Route edge tail from a cluster boundary (requires compound=true on graph) -/
def fromCluster (e : Edge) (clusterName : String) : Edge :=
  { e with ltail := some ("cluster_" ++ clusterName) }

/-- Set both lhead and ltail for cluster-to-cluster edges -/
def betweenClusters (e : Edge) (from_ to : String) : Edge :=
  { e with ltail := some ("cluster_" ++ from_), lhead := some ("cluster_" ++ to) }

end Edge

namespace Subgraph

def cluster (name : String) : Subgraph := { name := "cluster_" ++ name }

def plain (name : String) : Subgraph := { name }

def addNode (sg : Subgraph) (n : Node) : Subgraph :=
  { sg with nodes := sg.nodes ++ [n] }

def addEdge (sg : Subgraph) (e : Edge) : Subgraph :=
  { sg with edges := sg.edges ++ [e] }

def withAttr (sg : Subgraph) (a : Attr) : Subgraph :=
  { sg with attrs := sg.attrs ++ [a] }

end Subgraph

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

def addSubgraph (g : Graph) (sg : Subgraph) : Graph :=
  { g with subgraphs := g.subgraphs ++ [sg] }

def withAttr (g : Graph) (a : Attr) : Graph :=
  { g with attrs := g.attrs ++ [a] }

def withNodeDefaults (g : Graph) (as : List Attr) : Graph :=
  { g with nodeDefaults := g.nodeDefaults ++ as }

def withEdgeDefaults (g : Graph) (as : List Attr) : Graph :=
  { g with edgeDefaults := g.edgeDefaults ++ as }

/-- Enable strict mode (no multi-edges between same node pair) -/
def withStrict (g : Graph) (s : Bool := true) : Graph :=
  { g with strict := s }

end Graph

end Dot4

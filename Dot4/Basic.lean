/-!
# Dot4: Graphviz DOT DSL for Lean 4

A type-safe DSL for generating [Graphviz](https://graphviz.org/) DOT diagrams.

See the [DOT Language Reference](https://graphviz.org/doc/info/lang.html) for the
underlying format specification.

## Core Types

- `Graph`: The main container for nodes, edges, and subgraphs
- `Node`: A vertex with an ID, optional label, and attributes
- `Edge`: A connection between two nodes
- `Subgraph`: A nested graph (clusters when name starts with `cluster`)
- `Attr`: A key-value pair for DOT attributes

## References

- [DOT Language](https://graphviz.org/doc/info/lang.html)
- [Node, Edge and Graph Attributes](https://graphviz.org/doc/info/attrs.html)
- [Node Shapes](https://graphviz.org/doc/info/shapes.html)
- [Colors](https://graphviz.org/doc/info/colors.html)
-/

namespace Dot4

/-- A DOT attribute as a key-value pair (e.g., `shape=box`) -/
structure Attr where
  /-- Attribute name (e.g., `shape`, `color`, `label`) -/
  key : String
  /-- Attribute value (e.g., `box`, `red`, `"My Label"`) -/
  value : String
  deriving Repr, BEq, Hashable

/-- Source location for go-to-definition support -/
structure SourceRange where
  /-- File URI -/
  uri : String
  /-- Start line (0-indexed) -/
  startLine : Nat
  /-- Start character (0-indexed) -/
  startChar : Nat
  /-- End line (0-indexed) -/
  endLine : Nat
  /-- End character (0-indexed) -/
  endChar : Nat
  deriving Repr, BEq, Inhabited

/-- A node in the graph -/
structure Node where
  /-- Unique node identifier -/
  id : String
  /-- Optional display label (uses `id` if not set) -/
  label : Option String := none
  /-- Node attributes like `shape`, `color`, `style` -/
  attrs : List Attr := []
  /-- Source location for go-to-definition -/
  srcRange : Option SourceRange := none
  deriving Repr

/-- Compass point for edge ports. Maps to DOT compass points: `n`, `ne`, `e`, etc. -/
inductive Compass where
  /-- North -/ | n
  /-- Northeast -/ | ne
  /-- East -/ | e
  /-- Southeast -/ | se
  /-- South -/ | s
  /-- Southwest -/ | sw
  /-- West -/ | w
  /-- Northwest -/ | nw
  /-- Center -/ | c
  deriving Repr, BEq

namespace Compass
/-- Convert compass point to DOT string -/
def toString : Compass → String
  | n => "n" | ne => "ne" | e => "e" | se => "se"
  | s => "s" | sw => "sw" | w => "w" | nw => "nw" | c => "c"
end Compass

/-- Port specification for edge endpoints -/
structure Port where
  /-- Record port name (for record-shaped nodes) -/
  name : Option String := none
  /-- Compass direction for edge attachment -/
  compass : Option Compass := none
  deriving Repr, BEq

/-- An edge between two nodes -/
structure Edge where
  /-- Source node ID -/
  src : String
  /-- Destination node ID -/
  dst : String
  /-- Port specification for source endpoint -/
  srcPort : Port := {}
  /-- Port specification for destination endpoint -/
  dstPort : Port := {}
  /-- Optional edge label -/
  label : Option String := none
  /-- Edge attributes like `color`, `style`, `arrowhead` -/
  attrs : List Attr := []
  /-- Cluster to route edge head to (requires `compound=true` on graph) -/
  lhead : Option String := none
  /-- Cluster to route edge tail from (requires `compound=true` on graph) -/
  ltail : Option String := none
  /-- Source location for go-to-definition -/
  srcRange : Option SourceRange := none
  deriving Repr

/-- Graph direction: directed (`digraph`) or undirected (`graph`) -/
inductive Direction where
  /-- Directed graph with arrows (`->`) -/
  | directed
  /-- Undirected graph with plain edges (`--`) -/
  | undirected
  deriving Repr, BEq

/-- Rank constraint type for subgraphs -/
inductive RankType where
  /-- All nodes in subgraph have same rank -/
  | same
  /-- All nodes have minimum rank -/
  | min
  /-- All nodes are sources (no incoming edges at this rank) -/
  | source
  /-- All nodes have maximum rank -/
  | max
  /-- All nodes are sinks (no outgoing edges at this rank) -/
  | sink
  deriving Repr, BEq

namespace RankType
/-- Convert rank type to DOT string -/
def toString : RankType → String
  | same => "same"
  | min => "min"
  | source => "source"
  | max => "max"
  | sink => "sink"

/-- Parse a string to a `RankType` -/
def fromString? (s : String) : Option RankType :=
  match s with
  | "same" => some same
  | "min" => some min
  | "source" => some source
  | "max" => some max
  | "sink" => some sink
  | _ => Option.none

/-- All valid rank type names -/
def allNames : List String := ["same", "min", "source", "max", "sink"]
end RankType

/-- A subgraph or cluster. Clusters have names starting with `cluster`. -/
structure Subgraph where
  /-- Subgraph name. Use `cluster_` prefix for visual clustering. -/
  name : String
  /-- Nodes contained in this subgraph -/
  nodes : List Node := []
  /-- Edges contained in this subgraph -/
  edges : List Edge := []
  /-- Subgraph attributes like `label`, `style`, `bgcolor` -/
  attrs : List Attr := []
  /-- Default attributes for nodes in this subgraph -/
  nodeDefaults : List Attr := []
  /-- Default attributes for edges in this subgraph -/
  edgeDefaults : List Attr := []
  /-- Rank constraint for nodes in this subgraph -/
  rank : Option RankType := none
  deriving Repr

/-- A complete DOT graph -/
structure Graph where
  /-- Graph name (appears in output as `digraph "name"`) -/
  name : String := "G"
  /-- Whether edges are directed (`->`) or undirected (`--`) -/
  direction : Direction := .directed
  /-- Strict mode: no multi-edges between same node pair -/
  strict : Bool := false
  /-- Top-level nodes -/
  nodes : List Node := []
  /-- Top-level edges -/
  edges : List Edge := []
  /-- Subgraphs and clusters -/
  subgraphs : List Subgraph := []
  /-- Graph-level attributes -/
  attrs : List Attr := []
  /-- Default attributes applied to all nodes -/
  nodeDefaults : List Attr := []
  /-- Default attributes applied to all edges -/
  edgeDefaults : List Attr := []
  deriving Repr

/-! ### Attribute Constructors

Convenience functions for creating common DOT attributes.
These match DOT attribute names exactly. -/
namespace Attr
set_option linter.missingDocs false

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

/-- Number of sides for polygon shape (3-100) -/
def sides (n : Nat) : Attr := ⟨"sides", toString n⟩
/-- Distortion factor for polygon shape (-100.0 to 100.0) -/
def distortion (d : Float) : Attr := ⟨"distortion", toString d⟩
/-- Skew factor for polygon shape (-100.0 to 100.0) -/
def skew (s : Float) : Attr := ⟨"skew", toString s⟩
/-- Whether polygon vertices are evenly spaced (regular=true) -/
def regular (b : Bool) : Attr := ⟨"regular", if b then "true" else "false"⟩
/-- Rotation angle for node in degrees -/
def orientation (angle : Float) : Attr := ⟨"orientation", toString angle⟩

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

-- fdp/sfdp layout engine parameters
/-- Ideal edge length for fdp/sfdp layouts -/
def k (length : Float) : Attr := ⟨"K", toString length⟩
/-- Convergence threshold for fdp/sfdp (smaller = more accurate, slower) -/
def epsilon (threshold : Float) : Attr := ⟨"epsilon", toString threshold⟩
/-- Random seed or initialization for fdp/sfdp ("random", "self", or seed number) -/
def start (init : String) : Attr := ⟨"start", init⟩
/-- Force damping factor for fdp/neato (0.0-1.0) -/
def damping (factor : Float) : Attr := ⟨"Damping", toString factor⟩

/-- Logical head cluster for compound edges (requires compound=true) -/
def lhead (cluster : String) : Attr := ⟨"lhead", cluster⟩
/-- Logical tail cluster for compound edges (requires compound=true) -/
def ltail (cluster : String) : Attr := ⟨"ltail", cluster⟩

-- URLs and interaction
def href (u : String) : Attr := ⟨"href", u⟩
def url (u : String) : Attr := ⟨"URL", u⟩
def target (t : String) : Attr := ⟨"target", t⟩

-- Record nodes
def rects (r : String) : Attr := ⟨"rects", r⟩

-- Colorscheme support (e.g., "blues9", "set312", "accent8")
def colorscheme (s : String) : Attr := ⟨"colorscheme", s⟩

-- Image support
def image (path : String) : Attr := ⟨"image", path⟩
def imagescale (s : String) : Attr := ⟨"imagescale", s⟩

-- Ordering
def ordering (o : String) : Attr := ⟨"ordering", o⟩

-- Size and ratio
def size (s : String) : Attr := ⟨"size", s⟩
def ratio (r : String) : Attr := ⟨"ratio", r⟩

-- Output format hints
def dpi (n : Nat) : Attr := ⟨"dpi", toString n⟩

end Attr

/-! ### Builder Functions

Fluent builder patterns for constructing nodes, edges, and graphs.
Method names follow standard conventions (`new`, `withX`). -/
section Builders

namespace Node
/-- Create a new node with the given ID -/
def new (id : String) : Node := { id }
/-- Set the display label for a node -/
def withLabel (n : Node) (l : String) : Node := { n with label := some l }
/-- Append a list of attributes to a node -/
def withAttrs (n : Node) (as : List Attr) : Node := { n with attrs := n.attrs ++ as }
/-- Add a single attribute to a node -/
def withAttr (n : Node) (a : Attr) : Node := n.withAttrs [a]
end Node

namespace Port
/-- Create a port from a compass direction -/
def fromCompass (dir : Compass) : Port := { compass := some dir }
/-- Create a port from a record field name -/
def fromName (n : String) : Port := { name := some n }
/-- Create a port with both name and compass direction -/
def mk' (n : String) (dir : Compass) : Port := { name := some n, compass := some dir }
end Port

namespace Edge
/-- Create a new edge from source to destination node -/
def new (src dst : String) : Edge := { src, dst }
/-- Set the edge label -/
def withLabel (e : Edge) (l : String) : Edge := { e with label := some l }
/-- Append a list of attributes to an edge -/
def withAttrs (e : Edge) (as : List Attr) : Edge := { e with attrs := e.attrs ++ as }
/-- Set the source port for the edge -/
def withSrcPort (e : Edge) (p : Port) : Edge := { e with srcPort := p }
/-- Set the destination port for the edge -/
def withDstPort (e : Edge) (p : Port) : Edge := { e with dstPort := p }
/-- Set both source and destination ports for the edge -/
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

/-- Create a visual cluster subgraph (adds `cluster_` prefix) -/
def cluster (name : String) : Subgraph := { name := "cluster_" ++ name }

/-- Create a plain subgraph without visual clustering -/
def plain (name : String) : Subgraph := { name }

/-- Create a same-rank subgraph for horizontal alignment -/
def sameRank (name : String := "") : Subgraph :=
  { name := if name.isEmpty then "" else name, rank := some .same }

/-- Create a same-rank subgraph from a list of node IDs -/
def sameRankNodes (nodes : List String) (name : String := "") : Subgraph :=
  let sg := sameRank name
  nodes.foldl (fun acc id => { acc with nodes := acc.nodes ++ [{ id }] }) sg

/-- Add a node to the subgraph -/
def addNode (sg : Subgraph) (n : Node) : Subgraph :=
  { sg with nodes := sg.nodes ++ [n] }

/-- Add an edge to the subgraph -/
def addEdge (sg : Subgraph) (e : Edge) : Subgraph :=
  { sg with edges := sg.edges ++ [e] }

/-- Add an attribute to the subgraph -/
def withAttr (sg : Subgraph) (a : Attr) : Subgraph :=
  { sg with attrs := sg.attrs ++ [a] }

/-- Set default attributes for all nodes in the subgraph -/
def withNodeDefaults (sg : Subgraph) (as : List Attr) : Subgraph :=
  { sg with nodeDefaults := sg.nodeDefaults ++ as }

/-- Set default attributes for all edges in the subgraph -/
def withEdgeDefaults (sg : Subgraph) (as : List Attr) : Subgraph :=
  { sg with edgeDefaults := sg.edgeDefaults ++ as }

/-- Set the rank constraint for nodes in the subgraph -/
def withRank (sg : Subgraph) (r : RankType) : Subgraph :=
  { sg with rank := some r }

end Subgraph

namespace Graph

/-- Create an empty directed graph -/
def empty : Graph := {}

/-- Create a directed graph with arrows -/
def digraph (name : String := "G") : Graph :=
  { name, direction := .directed }

/-- Create an undirected graph -/
def graph (name : String := "G") : Graph :=
  { name, direction := .undirected }

/-- Add a node to the graph -/
def addNode (g : Graph) (n : Node) : Graph :=
  { g with nodes := g.nodes ++ [n] }

/-- Add an edge to the graph -/
def addEdge (g : Graph) (e : Edge) : Graph :=
  { g with edges := g.edges ++ [e] }

/-- Add a subgraph to the graph -/
def addSubgraph (g : Graph) (sg : Subgraph) : Graph :=
  { g with subgraphs := g.subgraphs ++ [sg] }

/-- Add an attribute to the graph -/
def withAttr (g : Graph) (a : Attr) : Graph :=
  { g with attrs := g.attrs ++ [a] }

/-- Set default attributes for all nodes in the graph -/
def withNodeDefaults (g : Graph) (as : List Attr) : Graph :=
  { g with nodeDefaults := g.nodeDefaults ++ as }

/-- Set default attributes for all edges in the graph -/
def withEdgeDefaults (g : Graph) (as : List Attr) : Graph :=
  { g with edgeDefaults := g.edgeDefaults ++ as }

/-- Enable strict mode (no multi-edges between same node pair) -/
def withStrict (g : Graph) (s : Bool := true) : Graph :=
  { g with strict := s }

end Graph

end Builders

end Dot4

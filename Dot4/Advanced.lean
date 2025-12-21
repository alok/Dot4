import Dot4.Syntax
import Dot4.Colors
import Dot4.Shapes

/-!
# Advanced DOT DSL Features

Power features that make Lean's DOT DSL better than raw DOT:
- Edge chains: `chain "A" → "B" → "C" → "D"`
- Rank constraints: `sameRank ["A", "B", "C"]`
- Type-safe attributes via shapes/colors
- Graph transformations
-/

namespace Dot4

/-! ## Graph Transformations -/

namespace Graph

/-- Filter nodes by predicate -/
def filterNodes (g : Graph) (p : Node → Bool) : Graph :=
  let nodes := g.nodes.filter p
  let nodeIdSet := nodes.map (·.id)
  let edges := g.edges.filter fun e =>
    nodeIdSet.contains e.src && nodeIdSet.contains e.dst
  { g with nodes, edges }

/-- Filter edges by predicate -/
def filterEdges (g : Graph) (p : Edge → Bool) : Graph :=
  { g with edges := g.edges.filter p }

/-- Map over all nodes -/
def mapNodes (g : Graph) (f : Node → Node) : Graph :=
  { g with nodes := g.nodes.map f }

/-- Map over all edges -/
def mapEdges (g : Graph) (f : Edge → Edge) : Graph :=
  { g with edges := g.edges.map f }

/-- Merge two graphs -/
def merge (g1 g2 : Graph) : Graph :=
  { g1 with
    nodes := g1.nodes ++ g2.nodes
    edges := g1.edges ++ g2.edges
    subgraphs := g1.subgraphs ++ g2.subgraphs
    attrs := g1.attrs ++ g2.attrs }

/-- Add a chain of edges: A → B → C → D -/
def addChain (g : Graph) (nodeIds : List String) (attrs : List Attr := []) : Graph :=
  match nodeIds with
  | [] | [_] => g
  | a :: b :: rest =>
    let g := g.addEdge { src := a, dst := b, attrs }
    addChain g (b :: rest) attrs

/-- Add nodes with same rank constraint -/
def withSameRank (g : Graph) (nodeIds : List String) : Graph :=
  let sg : Subgraph := {
    name := s!"rank_{nodeIds.length}_{nodeIds.head?.getD ""}"
    attrs := [Attr.rank "same"]
    nodes := nodeIds.map fun id => { id }
  }
  g.addSubgraph sg

/-- Reverse all edge directions -/
def reverseEdges (g : Graph) : Graph :=
  { g with edges := g.edges.map fun e => { e with src := e.dst, dst := e.src } }

/-- Get all node IDs -/
def getNodeIds (g : Graph) : List String :=
  g.nodes.map (·.id)

/-- Check if graph contains a node -/
def hasNode (g : Graph) (id : String) : Bool :=
  g.nodes.any (·.id == id)

/-- Check if graph contains an edge -/
def hasEdge (g : Graph) (src dst : String) : Bool :=
  g.edges.any fun e => e.src == src && e.dst == dst

/-- Get predecessors of a node -/
def predecessors (g : Graph) (id : String) : List String :=
  g.edges.filter (·.dst == id) |>.map (·.src)

/-- Get successors of a node -/
def successors (g : Graph) (id : String) : List String :=
  g.edges.filter (·.src == id) |>.map (·.dst)

/-- Apply attributes to all nodes -/
def withGlobalNodeAttrs (g : Graph) (attrs : List Attr) : Graph :=
  { g with nodes := g.nodes.map fun n => { n with attrs := attrs ++ n.attrs } }

/-- Apply attributes to all edges -/
def withGlobalEdgeAttrs (g : Graph) (attrs : List Attr) : Graph :=
  { g with edges := g.edges.map fun e => { e with attrs := attrs ++ e.attrs } }

end Graph

/-! ## Convenient Builders -/

/-- Quick node with just an ID -/
def mkNode (id : String) : Node := { id }

/-- Quick node with ID and label -/
def mkNodeL (id label : String) : Node := { id, label := some label }

/-- Quick edge -/
def mkEdge (src dst : String) : Edge := { src, dst }

/-- Quick labeled edge -/
def mkEdgeL (src dst label : String) : Edge := { src, dst, label := some label }

/-- Create a linear chain graph -/
def linearGraph (name : String) (nodeIds : List String) : Graph :=
  let g := Graph.digraph name
  let g := nodeIds.foldl (fun acc id => acc.addNode { id }) g
  g.addChain nodeIds

/-- Create a star graph (one center connected to all others) -/
def starGraph (name : String) (center : String) (spokes : List String) : Graph :=
  let g := Graph.digraph name
  let g := g.addNode { id := center }
  let g := spokes.foldl (fun acc id => acc.addNode { id }) g
  spokes.foldl (fun acc id => acc.addEdge { src := center, dst := id }) g

/-- Create a complete graph (all nodes connected to all others) -/
def completeGraph (name : String) (nodeIds : List String) : Graph :=
  let g := Graph.digraph name
  let g := nodeIds.foldl (fun acc id => acc.addNode { id }) g
  -- Generate all pairs where a < b
  let pairs := nodeIds.foldl (fun acc a =>
    acc ++ (nodeIds.filterMap fun b => if a < b then some (a, b) else none)) []
  pairs.foldl (fun acc (a, b) => acc.addEdge { src := a, dst := b }) g

/-- Create a bipartite graph -/
def bipartiteGraph (name : String) (left right : List String) : Graph :=
  let g := Graph.digraph name
  let g := left.foldl (fun acc id => acc.addNode { id }) g
  let g := right.foldl (fun acc id => acc.addNode { id }) g
  let g := g.withSameRank left
  let g := g.withSameRank right
  left.foldl (fun g1 l =>
    right.foldl (fun g2 r => g2.addEdge { src := l, dst := r }) g1) g

/-! ## HTML Labels -/

/-- HTML table cell -/
structure HtmlCell where
  content : String
  port : Option String := none
  colspan : Option Nat := none
  rowspan : Option Nat := none
  bgcolor : Option String := none
  align : Option String := none

/-- HTML table row -/
structure HtmlRow where
  cells : List HtmlCell

/-- HTML table for record-like labels -/
structure HtmlTable where
  rows : List HtmlRow
  border : Nat := 0
  cellborder : Nat := 1
  cellspacing : Nat := 0
  cellpadding : Nat := 4
  bgcolor : Option String := none

namespace HtmlCell

def toHtml (c : HtmlCell) : String :=
  let portAttr := c.port.map (s!" PORT=\"{·}\"") |>.getD ""
  let colspanAttr := c.colspan.map (s!" COLSPAN=\"{·}\"") |>.getD ""
  let rowspanAttr := c.rowspan.map (s!" ROWSPAN=\"{·}\"") |>.getD ""
  let bgcolorAttr := c.bgcolor.map (s!" BGCOLOR=\"{·}\"") |>.getD ""
  let alignAttr := c.align.map (s!" ALIGN=\"{·}\"") |>.getD ""
  s!"<TD{portAttr}{colspanAttr}{rowspanAttr}{bgcolorAttr}{alignAttr}>{c.content}</TD>"

end HtmlCell

namespace HtmlRow

def toHtml (r : HtmlRow) : String :=
  let cells := r.cells.map HtmlCell.toHtml |> String.join
  s!"<TR>{cells}</TR>"

end HtmlRow

namespace HtmlTable

def toHtml (t : HtmlTable) : String :=
  let bgAttr := t.bgcolor.map (s!" BGCOLOR=\"{·}\"") |>.getD ""
  let rows := t.rows.map HtmlRow.toHtml |> "\n".intercalate
  s!"<<TABLE BORDER=\"{t.border}\" CELLBORDER=\"{t.cellborder}\" CELLSPACING=\"{t.cellspacing}\" CELLPADDING=\"{t.cellpadding}\"{bgAttr}>\n{rows}\n</TABLE>>"

/-- Convert to label attribute -/
def toLabel (t : HtmlTable) : Attr := Attr.mk "label" t.toHtml

end HtmlTable

/-- Quick cell constructor -/
def cell (content : String) (port : Option String := none) : HtmlCell :=
  { content, port }

/-- Quick row constructor -/
def htmlRow (cells : List HtmlCell) : HtmlRow := { cells }

/-- Quick table constructor -/
def htmlTable (rows : List HtmlRow) : HtmlTable := { rows }

end Dot4

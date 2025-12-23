import Dot4.Syntax
import Dot4.Colors
import Dot4.Shapes

/-!
# Advanced DOT DSL Features

Power features that make Lean's DOT DSL better than raw DOT:
edge chains, rank constraints, type-safe attributes, and graph transformations.
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
  /-- Cell text content -/
  content : String
  /-- Optional port name for edge connections -/
  port : Option String := none
  /-- Number of columns to span -/
  colspan : Option Nat := none
  /-- Number of rows to span -/
  rowspan : Option Nat := none
  /-- Background color -/
  bgcolor : Option String := none
  /-- Text alignment (LEFT, CENTER, RIGHT) -/
  align : Option String := none

/-- HTML table row -/
structure HtmlRow where
  /-- Cells in this row -/
  cells : List HtmlCell

/-- HTML table for record-like labels -/
structure HtmlTable where
  /-- Table rows -/
  rows : List HtmlRow
  /-- Outer table border width -/
  border : Nat := 0
  /-- Cell border width -/
  cellborder : Nat := 1
  /-- Spacing between cells -/
  cellspacing : Nat := 0
  /-- Padding inside cells -/
  cellpadding : Nat := 4
  /-- Table background color -/
  bgcolor : Option String := none

namespace HtmlCell

/-- Render cell as HTML TD element -/
def toHtml (c : HtmlCell) : String :=
  let portAttr := c.port.map (s!" PORT=\"{·}\"") |>.getD ""
  let colspanAttr := c.colspan.map (s!" COLSPAN=\"{·}\"") |>.getD ""
  let rowspanAttr := c.rowspan.map (s!" ROWSPAN=\"{·}\"") |>.getD ""
  let bgcolorAttr := c.bgcolor.map (s!" BGCOLOR=\"{·}\"") |>.getD ""
  let alignAttr := c.align.map (s!" ALIGN=\"{·}\"") |>.getD ""
  s!"<TD{portAttr}{colspanAttr}{rowspanAttr}{bgcolorAttr}{alignAttr}>{c.content}</TD>"

end HtmlCell

namespace HtmlRow

/-- Render row as HTML TR element -/
def toHtml (r : HtmlRow) : String :=
  let cells := r.cells.map HtmlCell.toHtml |> String.join
  s!"<TR>{cells}</TR>"

end HtmlRow

namespace HtmlTable

/-- Render table as HTML TABLE element with angle bracket wrapper -/
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

/-! ## Graph Templates -/

/-- Create a flowchart with decision nodes and process nodes -/
def flowchart (name : String) : Graph :=
  Graph.digraph name
  |>.withAttr (Attr.rankdir "TB")
  |>.withNodeDefaults [Attr.shape "box", Attr.style "rounded"]

/-- Create a state machine diagram -/
def stateMachine (name : String) : Graph :=
  Graph.digraph name
  |>.withAttr (Attr.rankdir "LR")
  |>.withNodeDefaults [Attr.shape "circle"]

/-- Create an entity-relationship diagram -/
def erDiagram (name : String) : Graph :=
  Graph.digraph name
  |>.withAttr (Attr.rankdir "TB")
  |>.withAttr (Attr.splines "ortho")
  |>.withNodeDefaults [Attr.shape "record"]

/-- Create a class diagram (UML-style) -/
def classDiagram (name : String) : Graph :=
  Graph.digraph name
  |>.withAttr (Attr.rankdir "BT")  -- Bottom to top for inheritance
  |>.withAttr (Attr.splines "ortho")
  |>.withNodeDefaults [Attr.shape "record"]

/-- Create a dependency graph -/
def dependencyGraph (name : String) : Graph :=
  Graph.digraph name
  |>.withAttr (Attr.rankdir "LR")
  |>.withNodeDefaults [Attr.shape "box", Attr.style "rounded,filled", Attr.fillcolor "#e8e8e8"]
  |>.withEdgeDefaults [Attr.arrowsize 0.7]

/-- Create a tree layout graph -/
def treeGraph (name : String) : Graph :=
  Graph.digraph name
  |>.withAttr (Attr.rankdir "TB")
  |>.withAttr (Attr.splines "line")
  |>.withNodeDefaults [Attr.shape "ellipse"]

/-- Create a network topology diagram -/
def networkDiagram (name : String) : Graph :=
  Graph.graph name  -- Undirected
  |>.withAttr (Attr.layout "neato")
  |>.withAttr (Attr.overlap "false")
  |>.withNodeDefaults [Attr.shape "box3d"]

/-- Add a decision diamond node to a flowchart -/
def Graph.addDecision (g : Graph) (id : String) (question : String) : Graph :=
  g.addNode { id, label := some question, attrs := [Attr.shape "diamond"] }

/-- Add a process box node to a flowchart -/
def Graph.addProcess (g : Graph) (id : String) (action : String) : Graph :=
  g.addNode { id, label := some action, attrs := [Attr.shape "box"] }

/-- Add a start/end node to a flowchart -/
def Graph.addTerminal (g : Graph) (id : String) (text : String) : Graph :=
  g.addNode { id, label := some text, attrs := [Attr.shape "ellipse"] }

/-- Add a state with entry/exit actions -/
def Graph.addState (g : Graph) (id : String) (name : String)
    (entry : Option String := none) (exit : Option String := none) : Graph :=
  let label := match entry, exit with
    | some e, some x => s!"{name}|entry: {e}|exit: {x}"
    | some e, none => s!"{name}|entry: {e}"
    | none, some x => s!"{name}|exit: {x}"
    | none, none => name
  let shape := if entry.isSome || exit.isSome then "record" else "ellipse"
  g.addNode { id, label := some label, attrs := [Attr.shape shape] }

/-- Add a labeled transition edge -/
def Graph.addTransition (g : Graph) (src dst : String) (event : String)
    (guard : Option String := none) (action : Option String := none) : Graph :=
  let label := match guard, action with
    | some gd, some act => s!"{event} [{gd}] / {act}"
    | some gd, none => s!"{event} [{gd}]"
    | none, some act => s!"{event} / {act}"
    | none, none => event
  g.addEdge { src, dst, label := some label }

/-- Add a class node (UML record format) -/
def Graph.addClass (g : Graph) (id : String) (name : String)
    (attrs : List String := []) (methods : List String := []) : Graph :=
  let attrSection := if attrs.isEmpty then "" else "|" ++ "|".intercalate attrs
  let methodSection := if methods.isEmpty then "" else "|" ++ "|".intercalate methods
  let label := s!"{name}{attrSection}{methodSection}"
  g.addNode { id, label := some label, attrs := [Attr.shape "record"] }

/-- Add an inheritance edge (empty arrowhead) -/
def Graph.addInheritance (g : Graph) (child parent : String) : Graph :=
  g.addEdge { src := child, dst := parent, attrs := [Attr.arrowhead "empty"] }

/-- Add a composition edge (filled diamond) -/
def Graph.addComposition (g : Graph) (whole part : String) : Graph :=
  g.addEdge { src := whole, dst := part, attrs := [Attr.arrowtail "diamond", Attr.dir "back"] }

/-- Add an aggregation edge (empty diamond) -/
def Graph.addAggregation (g : Graph) (whole part : String) : Graph :=
  g.addEdge { src := whole, dst := part, attrs := [Attr.arrowtail "odiamond", Attr.dir "back"] }

/-- Add a dependency edge (dashed arrow) -/
def Graph.addDependency (g : Graph) (dependent dependency : String) : Graph :=
  g.addEdge { src := dependent, dst := dependency, attrs := [Attr.style "dashed"] }

/-! ## Graph Diff -/

/-- A node change in a graph diff -/
inductive NodeChange where
  /-- Node was added in the new graph -/
  | added (n : Node)
  /-- Node was removed from the old graph -/
  | removed (n : Node)
  /-- Node attributes changed -/
  | modified (oldNode newNode : Node)
  deriving Repr

/-- An edge change in a graph diff -/
inductive EdgeChange where
  /-- Edge was added in the new graph -/
  | added (e : Edge)
  /-- Edge was removed from the old graph -/
  | removed (e : Edge)
  /-- Edge attributes changed -/
  | modified (oldEdge newEdge : Edge)
  deriving Repr

/-- Represents the difference between two graphs -/
structure GraphDiff where
  /-- Node changes (added, removed, modified) -/
  nodes : List NodeChange := []
  /-- Edge changes (added, removed, modified) -/
  edges : List EdgeChange := []
  /-- Subgraph names added in new graph -/
  subgraphsAdded : List String := []
  /-- Subgraph names removed from old graph -/
  subgraphsRemoved : List String := []
  deriving Repr

namespace GraphDiff

/-- Check if graphs are identical -/
def isEmpty (d : GraphDiff) : Bool :=
  d.nodes.isEmpty && d.edges.isEmpty &&
  d.subgraphsAdded.isEmpty && d.subgraphsRemoved.isEmpty

/-- Get only added nodes -/
def addedNodes (d : GraphDiff) : List Node :=
  d.nodes.filterMap fun c => match c with
    | .added n => some n
    | _ => none

/-- Get only removed nodes -/
def removedNodes (d : GraphDiff) : List Node :=
  d.nodes.filterMap fun c => match c with
    | .removed n => some n
    | _ => none

/-- Get only modified nodes (old, new pairs) -/
def modifiedNodes (d : GraphDiff) : List (Node × Node) :=
  d.nodes.filterMap fun c => match c with
    | .modified oldNode newNode => some (oldNode, newNode)
    | _ => none

/-- Get only added edges -/
def addedEdges (d : GraphDiff) : List Edge :=
  d.edges.filterMap fun c => match c with
    | .added e => some e
    | _ => none

/-- Get only removed edges -/
def removedEdges (d : GraphDiff) : List Edge :=
  d.edges.filterMap fun c => match c with
    | .removed e => some e
    | _ => none

/-- Summary of changes -/
def summary (d : GraphDiff) : String :=
  let added := d.addedNodes.length
  let removed := d.removedNodes.length
  let modified := d.modifiedNodes.length
  let edgesAdded := d.addedEdges.length
  let edgesRemoved := d.removedEdges.length
  s!"+{added}/-{removed}/~{modified} nodes, +{edgesAdded}/-{edgesRemoved} edges"

end GraphDiff

namespace Graph

/-- Compare attribute lists (order-independent) -/
private def attrsEqual (a1 a2 : List Attr) : Bool :=
  a1.length == a2.length &&
  a1.all fun attr => a2.any fun attr' => attr.key == attr'.key && attr.value == attr'.value

/-- Compare two nodes for equality (including attributes) -/
private def nodeEqual (n1 n2 : Node) : Bool :=
  n1.id == n2.id && n1.label == n2.label && attrsEqual n1.attrs n2.attrs

/-- Compare two edges for equality (including attributes) -/
private def edgeEqual (e1 e2 : Edge) : Bool :=
  e1.src == e2.src && e1.dst == e2.dst &&
  e1.srcPort == e2.srcPort && e1.dstPort == e2.dstPort &&
  e1.label == e2.label && attrsEqual e1.attrs e2.attrs

/-- Compute the diff between two graphs.
    Returns changes needed to transform the first graph into the second. -/
def diff (old new : Graph) : GraphDiff :=
  -- Collect all nodes from both graphs (including subgraphs)
  let oldNodes := old.nodes ++ old.subgraphs.flatMap (·.nodes)
  let newNodes := new.nodes ++ new.subgraphs.flatMap (·.nodes)

  -- Collect all edges from both graphs
  let oldEdges := old.edges ++ old.subgraphs.flatMap (·.edges)
  let newEdges := new.edges ++ new.subgraphs.flatMap (·.edges)

  -- Compute node changes
  let nodeChanges := Id.run do
    let mut changes : List NodeChange := []

    -- Find removed and modified nodes
    for oNode in oldNodes do
      match newNodes.find? (·.id == oNode.id) with
      | none => changes := .removed oNode :: changes
      | some nNode =>
        if !nodeEqual oNode nNode then
          changes := .modified oNode nNode :: changes

    -- Find added nodes
    for nNode in newNodes do
      if !oldNodes.any (·.id == nNode.id) then
        changes := .added nNode :: changes

    changes.reverse

  -- Compute edge changes
  let edgeChanges := Id.run do
    let mut changes : List EdgeChange := []

    -- Find removed and modified edges
    for oEdge in oldEdges do
      match newEdges.find? (fun e => e.src == oEdge.src && e.dst == oEdge.dst) with
      | none => changes := .removed oEdge :: changes
      | some nEdge =>
        if !edgeEqual oEdge nEdge then
          changes := .modified oEdge nEdge :: changes

    -- Find added edges
    for nEdge in newEdges do
      if !oldEdges.any (fun e => e.src == nEdge.src && e.dst == nEdge.dst) then
        changes := .added nEdge :: changes

    changes.reverse

  -- Compute subgraph changes
  let oldSubgraphNames := old.subgraphs.map (·.name)
  let newSubgraphNames := new.subgraphs.map (·.name)
  let subgraphsAdded := newSubgraphNames.filter (!oldSubgraphNames.contains ·)
  let subgraphsRemoved := oldSubgraphNames.filter (!newSubgraphNames.contains ·)

  { nodes := nodeChanges
    edges := edgeChanges
    subgraphsAdded
    subgraphsRemoved }

/-- Check if two graphs are structurally equal (ignoring order) -/
def structuralEq (g1 g2 : Graph) : Bool :=
  (diff g1 g2).isEmpty

/-- Apply a diff to a graph to produce a new graph -/
def applyDiff (g : Graph) (d : GraphDiff) : Graph :=
  let g := d.nodes.foldl (fun gr change =>
    match change with
    | .added n => gr.addNode n
    | .removed n => { gr with nodes := gr.nodes.filter (·.id != n.id) }
    | .modified _ newNode => { gr with nodes := gr.nodes.map fun n =>
        if n.id == newNode.id then newNode else n }
  ) g

  let g := d.edges.foldl (fun gr change =>
    match change with
    | .added e => gr.addEdge e
    | .removed e => { gr with edges := gr.edges.filter fun e' =>
        e'.src != e.src || e'.dst != e.dst }
    | .modified _ newEdge => { gr with edges := gr.edges.map fun e =>
        if e.src == newEdge.src && e.dst == newEdge.dst then newEdge else e }
  ) g

  g

end Graph

end Dot4

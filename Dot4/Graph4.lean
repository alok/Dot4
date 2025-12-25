import Dot4.Basic
import Graph4

/-!
# Graph4 Integration

Convert Graph4 graphs to Dot4 for visualization.
-/

namespace Dot4

/-- Convert a Graph4.Graph to a Dot4.Graph for visualization.

The node type must have ToString for node IDs.
-/
def fromGraph4 {α : Type} [ToString α] (g4 : Graph4.Graph α) : Graph :=
  let base : Graph := {
    name := g4.name
    direction := if g4.directed then .directed else .undirected
    strict := false
    nodes := []
    edges := []
    subgraphs := []
    attrs := []
  }
  -- Add nodes
  let withNodes := g4.nodes.foldl (fun g n =>
    g.addNode {
      id := toString n.id
      label := n.label
      attrs := match n.data with
        | some d => [Attr.mk "data" d]
        | none => []
    }) base
  -- Add edges
  g4.edges.foldl (fun g e =>
    let attrs := match (e.label, e.weight) with
      | (some l, some w) => [Attr.mk "label" l, Attr.mk "weight" (toString w)]
      | (some l, none) => [Attr.mk "label" l]
      | (none, some w) => [Attr.mk "label" (toString w)]
      | (none, none) => []
    g.addEdge {
      src := toString e.src
      dst := toString e.dst
      attrs := attrs
    }) withNodes

/-- Convert a Graph4.Graph with custom node styling.

The `nodeStyle` function can return custom attributes for each node.
-/
def fromGraph4WithStyle {α : Type} [ToString α]
    (g4 : Graph4.Graph α)
    (nodeStyle : Graph4.Node α → List Attr := fun _ => [])
    (edgeStyle : Graph4.Edge α → List Attr := fun _ => []) : Graph :=
  let base : Graph := {
    name := g4.name
    direction := if g4.directed then .directed else .undirected
    strict := false
    nodes := []
    edges := []
    subgraphs := []
    attrs := []
  }
  -- Add nodes with custom styling
  let withNodes := g4.nodes.foldl (fun g n =>
    let baseAttrs := match n.data with
      | some d => [Attr.mk "data" d]
      | none => []
    g.addNode {
      id := toString n.id
      label := n.label
      attrs := baseAttrs ++ nodeStyle n
    }) base
  -- Add edges with custom styling
  g4.edges.foldl (fun g e =>
    let baseAttrs := match (e.label, e.weight) with
      | (some l, some w) => [Attr.mk "label" l, Attr.mk "weight" (toString w)]
      | (some l, none) => [Attr.mk "label" l]
      | (none, some w) => [Attr.mk "label" (toString w)]
      | (none, none) => []
    g.addEdge {
      src := toString e.src
      dst := toString e.dst
      attrs := baseAttrs ++ edgeStyle e
    }) withNodes

/-- Create a Dot4 graph from a Graph4 graph highlighting source and sink nodes. -/
def fromGraph4Highlighted {α : Type} [BEq α] [ToString α] (g4 : Graph4.Graph α) : Graph :=
  let sources := g4.sources
  let sinks := g4.sinks
  fromGraph4WithStyle g4
    (nodeStyle := fun n =>
      if sources.any (· == n.id) then
        [Attr.mk "style" "filled", Attr.mk "fillcolor" "lightgreen"]
      else if sinks.any (· == n.id) then
        [Attr.mk "style" "filled", Attr.mk "fillcolor" "lightcoral"]
      else [])

/-- Show edges as weighted if they have weights. -/
def fromGraph4Weighted {α : Type} [ToString α] (g4 : Graph4.Graph α) : Graph :=
  fromGraph4WithStyle g4
    (edgeStyle := fun e =>
      match e.weight with
      | some w =>
        let width := max 1.0 (min 5.0 w)  -- Clamp width between 1 and 5
        [Attr.mk "penwidth" (toString width)]
      | none => [])

end Dot4

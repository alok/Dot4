import Dot4.Basic
import Dot4.Render
import Lean
import ProofWidgets.Component.Basic
import ProofWidgets.Presentation.Expr

/-!
# Dot4 Widget

Renders Dot4 graphs as SVG in the VS Code infoview using Graphviz (viz.js).

## Commands
- {lit}`#dot myGraph` - Render graph as SVG
- {lit}`#dot_raw "digraph { a -> b }"` - Render raw DOT string
-/

namespace Dot4

open Lean Widget Server Elab Command Meta Term

/-! ## Graphviz Widget (viz.js) -/

/-- Props for the Graphviz widget. -/
structure DotVisualizationProps where
  /-- The DOT source string to render. -/
  dot : String
  /-- Layout engine (dot, neato, fdp, sfdp, circo, twopi, osage, patchwork). -/
  engine : Option String := none
  /-- Whether this is a diff view. -/
  isDiff : Option Bool := none
  /-- Node IDs that were added (for diff). -/
  addedNodes : Option (Array String) := none
  /-- Node IDs that were removed (for diff). -/
  removedNodes : Option (Array String) := none
  /-- Edge IDs that were added (for diff, format "src->dst"). -/
  addedEdges : Option (Array String) := none
  /-- Edge IDs that were removed (for diff). -/
  removedEdges : Option (Array String) := none
  deriving Inhabited, Server.RpcEncodable

/-- Widget component using viz.js (Graphviz compiled to WASM). -/
@[widget_module]
def DotVisualization : ProofWidgets.Component DotVisualizationProps where
  javascript := include_str ".." / "build" / "js" / "dotVisualization.js"

/-! ## Commands -/

/-- Evaluate Graph.toDot at compile time. -/
unsafe def evalGraphToDot (stx : Syntax) : TermElabM String := do
  let expr ← elabTerm stx (some (Lean.mkConst ``Graph))
  let toDotExpr ← mkAppM ``Graph.toDot #[expr]
  let strExpr ← whnf toDotExpr
  evalExpr String (Lean.mkConst ``String) strExpr

/-- Render a Dot4 graph as SVG in the infoview.

Usage: {lit}`#dot myGraph`
-/
syntax (name := showDotCmd) "#dot " term : command

/-- Command elaborator for {lit}`#dot`. -/
@[command_elab showDotCmd]
unsafe def elabShowDotCmd : CommandElab := fun
  | stx@`(#dot $g:term) => do
    let dotStr ← liftTermElabM <| evalGraphToDot g
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash DotVisualization.javascript)
      (return (← rpcEncode ({ dot := dotStr } : DotVisualizationProps)))
      stx
  | stx => throwError "Unexpected syntax {stx}."

/-- Render a raw DOT string as SVG.

Usage: {lit}`#dot_raw "digraph { a -> b }"`
-/
syntax (name := showDotRawCmd) "#dot_raw " str : command

/-- Command elaborator for {lit}`#dot_raw`. -/
@[command_elab showDotRawCmd]
def elabShowDotRawCmd : CommandElab := fun
  | stx@`(#dot_raw $s:str) => do
    let dotStr := s.getString
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash DotVisualization.javascript)
      (return (← rpcEncode ({ dot := dotStr } : DotVisualizationProps)))
      stx
  | stx => throwError "Unexpected syntax {stx}."

/-! ## Graph Diff -/

/-- Compute node IDs from a graph. -/
def Graph.nodeIds (g : Graph) : List String :=
  g.nodes.map (·.id) ++ (g.subgraphs.map (·.nodes.map (·.id))).flatten

/-- Compute edge IDs from a graph (format: "src->dst"). -/
def Graph.edgeIds (g : Graph) : List String :=
  let mkEdgeId (e : Edge) := s!"{e.src}->{e.dst}"
  g.edges.map mkEdgeId ++ (g.subgraphs.map (·.edges.map mkEdgeId)).flatten

/-- Merge two graphs for diff visualization. -/
def Graph.mergeForDiff (g1 g2 : Graph) : Graph :=
  let allNodes := (g1.nodes ++ g2.nodes).foldl (fun acc n =>
    if acc.any (·.id == n.id) then acc else acc ++ [n]) []
  let allEdges := (g1.edges ++ g2.edges).foldl (fun acc e =>
    if acc.any (fun e' => e'.src == e.src && e'.dst == e.dst) then acc else acc ++ [e]) []
  { g1 with nodes := allNodes, edges := allEdges }

/-- Render a diff of two Dot4 graphs as SVG with highlighting.

Usage: {lit}`#dot_diff oldGraph newGraph`
-/
syntax (name := showDotDiffCmd) "#dot_diff " term:max term:max : command

/-- Command elaborator for {lit}`#dot_diff`. -/
@[command_elab showDotDiffCmd]
unsafe def elabShowDotDiffCmd : CommandElab := fun stx => do
  let g1 := stx[1]
  let g2 := stx[2]
  let (graph1, graph2) ← liftTermElabM do
    let e1 ← elabTerm g1 (some (Lean.mkConst ``Graph))
    let e2 ← elabTerm g2 (some (Lean.mkConst ``Graph))
    let gr1 ← Lean.Meta.evalExpr' Graph ``Graph e1
    let gr2 ← Lean.Meta.evalExpr' Graph ``Graph e2
    pure (gr1, gr2)
  let merged := Graph.mergeForDiff graph1 graph2
  let dotStr := merged.toDot
  let nodes1 := graph1.nodeIds.toArray
  let nodes2 := graph2.nodeIds.toArray
  let edges1 := graph1.edgeIds.toArray
  let edges2 := graph2.edgeIds.toArray
  let addedNodes := nodes2.filter (fun n => !nodes1.contains n)
  let removedNodes := nodes1.filter (fun n => !nodes2.contains n)
  let addedEdges := edges2.filter (fun e => !edges1.contains e)
  let removedEdges := edges1.filter (fun e => !edges2.contains e)
  liftCoreM <| Widget.savePanelWidgetInfo
    (hash DotVisualization.javascript)
    (return (← rpcEncode ({
      dot := dotStr
      isDiff := some true
      addedNodes := some addedNodes
      removedNodes := some removedNodes
      addedEdges := some addedEdges
      removedEdges := some removedEdges
    } : DotVisualizationProps)))
    stx

/-! ## Expression Presenter -/

/-- Evaluate Graph.toDot using runtime evaluation. -/
unsafe def evalGraphUnsafe (e : Expr) : MetaM String := do
  let toDotExpr ← mkAppM ``Graph.toDot #[e]
  Lean.Meta.evalExpr' String ``String toDotExpr

/-- Safe wrapper for evalGraphUnsafe. -/
@[implemented_by evalGraphUnsafe]
opaque evalGraph (e : Expr) : MetaM String

/-- Expression presenter for Graph values in the infoview. -/
@[expr_presenter]
def graphPresenter : ProofWidgets.ExprPresenter where
  userName := "Dot4 Graph"
  layoutKind := .inline
  present e := do
    let ty ← inferType e
    let_expr Dot4.Graph := ty | return .text s!"{← ppExpr e}"
    try
      let dotStr ← evalGraph e
      let preview := if dotStr.length > 50 then dotStr.take 50 ++ "…" else dotStr
      return .text s!"📊 {preview}"
    catch _ =>
      return .text s!"{← ppExpr e}"

end Dot4

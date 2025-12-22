import Dot4.Basic
import Dot4.Parser.AST

/-!
# AST to Graph Conversion

Converts parsed AST to Dot4 Graph structures.
Handles implicit node creation and edge chain expansion.
-/

namespace Dot4.Parser

set_option linter.missingDocs false

/-- Convert compass string to Compass enum -/
def parseCompassDir (s : String) : Option Compass :=
  match s with
  | "n" => some .n
  | "ne" => some .ne
  | "e" => some .e
  | "se" => some .se
  | "s" => some .s
  | "sw" => some .sw
  | "w" => some .w
  | "nw" => some .nw
  | "c" => some .c
  | "_" => some .c  -- underscore means center
  | _ => none

/-- Convert PortAST to Port -/
def convertPort (p : PortAST) : Port :=
  { name := p.name
    compass := p.compass.bind parseCompassDir }

/-- Convert AttrAST to Attr -/
def convertAttr (a : AttrAST) : Attr :=
  { key := a.key, value := a.value }

/-- Convert list of AttrAST to list of Attr -/
def convertAttrs (attrs : List AttrAST) : List Attr :=
  attrs.map convertAttr

/-- Extract label from attributes if present -/
def extractLabel (attrs : List AttrAST) : Option String × List AttrAST :=
  let labelAttr := attrs.find? (·.key == "label")
  let otherAttrs := attrs.filter (·.key != "label")
  (labelAttr.map (·.value), otherAttrs)

/-- Extract lhead/ltail from attributes -/
def extractClusterAttrs (attrs : List AttrAST) : Option String × Option String × List AttrAST :=
  let lhead := (attrs.find? (·.key == "lhead")).map (·.value)
  let ltail := (attrs.find? (·.key == "ltail")).map (·.value)
  let other := attrs.filter fun a => a.key != "lhead" && a.key != "ltail"
  (lhead, ltail, other)

/-- Conversion state -/
structure ConvState where
  nodes : List Node := []
  edges : List Edge := []
  subgraphs : List Subgraph := []
  graphAttrs : List Attr := []
  nodeDefaults : List Attr := []
  edgeDefaults : List Attr := []
  /-- Track seen node IDs to avoid duplicates -/
  seenNodes : List String := []

/-- Add a node if not already seen -/
def addNodeIfNew (state : ConvState) (id : String) (attrs : List Attr) (label : Option String) : ConvState :=
  if state.seenNodes.contains id then
    state
  else
    let node : Node := { id, label, attrs }
    { state with nodes := state.nodes ++ [node], seenNodes := state.seenNodes ++ [id] }

/-- Ensure node exists (for implicit nodes from edges) -/
def ensureNode (state : ConvState) (nodeId : NodeIdAST) : ConvState :=
  addNodeIfNew state nodeId.id [] none

/-- Process an edge statement -/
def processEdge (state : ConvState) (endpoints : List NodeIdAST) (attrs : List AttrAST) : ConvState :=
  -- Ensure all nodes exist
  let state := endpoints.foldl ensureNode state

  -- Extract special attributes
  let (lhead, ltail, otherAttrs) := extractClusterAttrs attrs
  let (label, finalAttrs) := extractLabel otherAttrs

  -- Create edges for each consecutive pair
  let pairs := endpoints.zip (endpoints.drop 1)
  let newEdges := pairs.map fun (src, dst) =>
    { src := src.id
      dst := dst.id
      srcPort := src.port.map convertPort |>.getD {}
      dstPort := dst.port.map convertPort |>.getD {}
      label := label
      attrs := convertAttrs finalAttrs
      lhead := lhead
      ltail := ltail : Edge }

  { state with edges := state.edges ++ newEdges }

/-- Process a simple statement -/
def processSimpleStmt (state : ConvState) (stmt : SimpleStmtAST) : ConvState :=
  match stmt with
  | .nodeStmt nodeId attrs =>
    let (label, otherAttrs) := extractLabel attrs
    addNodeIfNew state nodeId.id (convertAttrs otherAttrs) label

  | .edgeStmt endpoints attrs =>
    processEdge state endpoints attrs

  | .attrStmt target attrs =>
    let converted := convertAttrs attrs
    match target with
    | .graph => { state with graphAttrs := state.graphAttrs ++ converted }
    | .node => { state with nodeDefaults := converted }
    | .edge => { state with edgeDefaults := converted }

  | .assignment key value =>
    { state with graphAttrs := state.graphAttrs ++ [{ key, value }] }

/-- Process a statement -/
def processStmt (state : ConvState) (stmt : StmtAST) : ConvState :=
  match stmt with
  | .simple s => processSimpleStmt state s
  | .subgraph sg =>
    let subgraphState := sg.stmts.foldl processSimpleStmt {}
    let subgraph : Subgraph := {
      name := sg.name.getD ""
      nodes := subgraphState.nodes
      edges := subgraphState.edges
      attrs := subgraphState.graphAttrs
      nodeDefaults := subgraphState.nodeDefaults
      edgeDefaults := subgraphState.edgeDefaults
    }
    { state with subgraphs := state.subgraphs ++ [subgraph] }

/-- Convert GraphAST to Graph -/
def astToGraph (ast : GraphAST) : Graph :=
  let direction := if ast.directed then Direction.directed else Direction.undirected
  let finalState := ast.stmts.foldl processStmt {}

  { name := ast.name.getD "G"
    direction := direction
    strict := ast.strict
    nodes := finalState.nodes
    edges := finalState.edges
    subgraphs := finalState.subgraphs
    attrs := finalState.graphAttrs
    nodeDefaults := finalState.nodeDefaults
    edgeDefaults := finalState.edgeDefaults }

end Dot4.Parser

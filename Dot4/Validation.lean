import Dot4.Basic
import Dot4.Advanced

/-!
# Graph Validation Helpers

Utilities for validating and analyzing graphs.

## Usage

```lean
open Dot4 in
def g := dot { ... }

#eval g.isDAG          -- Check for cycles (directed graphs)
#eval g.validate       -- Get list of validation errors
#eval g.sources        -- Nodes with no incoming edges
#eval g.sinks          -- Nodes with no outgoing edges
```
-/

namespace Dot4

namespace Graph

/-- Get all edge pairs in the graph (including subgraphs) -/
def edgePairs (g : Graph) : List (String × String) :=
  let topLevel := g.edges.map (fun e => (e.src, e.dst))
  let subgraphEdges := (g.subgraphs.map (·.edges.map (fun e => (e.src, e.dst)))).flatten
  topLevel ++ subgraphEdges

/-- Get nodes with no incoming edges (sources) -/
def sources (g : Graph) : List String :=
  let allNodes := g.getNodeIds
  let hasIncoming := g.edgePairs.map (·.2)
  allNodes.filter (fun n => !hasIncoming.contains n)

/-- Get nodes with no outgoing edges (sinks) -/
def sinks (g : Graph) : List String :=
  let allNodes := g.getNodeIds
  let hasOutgoing := g.edgePairs.map (·.1)
  allNodes.filter (fun n => !hasOutgoing.contains n)

/-- Check if the graph is a DAG (no cycles) using DFS -/
partial def isDAG (g : Graph) : Bool :=
  let allNodes := g.getNodeIds
  -- States: 0 = unvisited, 1 = visiting (in current path), 2 = visited
  let rec hasCycle (nodeId : String) (visiting : List String) (visited : List String) : Bool × List String :=
    if visited.contains nodeId then
      (false, visited)
    else if visiting.contains nodeId then
      (true, visited)  -- Found a back edge = cycle
    else
      let newVisiting := nodeId :: visiting
      let succs := g.successors nodeId
      let (foundCycle, newVisited) := succs.foldl
        (fun (acc : Bool × List String) succ =>
          if acc.1 then acc  -- Already found cycle
          else hasCycle succ newVisiting acc.2)
        (false, visited)
      if foundCycle then (true, newVisited)
      else (false, nodeId :: newVisited)

  let (hasCycleResult, _) := allNodes.foldl
    (fun (acc : Bool × List String) nodeId =>
      if acc.1 then acc  -- Already found cycle
      else hasCycle nodeId [] acc.2)
    (false, [])
  !hasCycleResult

/-- Validation error type -/
inductive ValidationError where
  | danglingEdge (src : String) (dst : String) (missing : String)
  | duplicateNode (id : String)
  | emptyNodeId
  | cyclicGraph (msg : String)
  deriving Repr, BEq

namespace ValidationError

def toString : ValidationError → String
  | danglingEdge src dst missing =>
    s!"Edge '{src}' → '{dst}' references undefined node '{missing}'"
  | duplicateNode id =>
    s!"Duplicate node ID: '{id}'"
  | emptyNodeId =>
    "Node has empty ID"
  | cyclicGraph msg =>
    s!"Graph contains cycle: {msg}"

instance : ToString ValidationError := ⟨toString⟩

end ValidationError

/-- Validate the graph and return a list of errors -/
def validate (g : Graph) : List ValidationError :=
  let nodeIds := g.getNodeIds

  -- Check for empty node IDs
  let emptyIdErrors := g.nodes.filterMap fun n =>
    if n.id.isEmpty then some .emptyNodeId else none

  -- Check for duplicate node IDs
  let findDuplicates : List String → List String → List String :=
    fun xs =>
      let rec go : List String → List String → List String
        | [], _ => []
        | x :: rest, seen =>
          if seen.contains x then x :: go rest seen
          else go rest (x :: seen)
      go xs
  let dupErrors := (findDuplicates nodeIds []).map ValidationError.duplicateNode

  -- Check for dangling edges (edges referencing non-existent nodes)
  let danglingErrors := g.edgePairs.filterMap fun (src, dst) =>
    if !nodeIds.contains src then some (.danglingEdge src dst src)
    else if !nodeIds.contains dst then some (.danglingEdge src dst dst)
    else none

  emptyIdErrors ++ dupErrors ++ danglingErrors

/-- Check if the graph is valid (no errors) -/
def isValid (g : Graph) : Bool :=
  g.validate.isEmpty

/-- Count total nodes -/
def nodeCount (g : Graph) : Nat :=
  g.getNodeIds.length

/-- Count total edges -/
def edgeCount (g : Graph) : Nat :=
  g.edgePairs.length

/-- Get degree (in + out) for a node -/
def degree (g : Graph) (id : String) : Nat :=
  (g.successors id).length + (g.predecessors id).length

/-- Get in-degree for a node -/
def inDegree (g : Graph) (id : String) : Nat :=
  (g.predecessors id).length

/-- Get out-degree for a node -/
def outDegree (g : Graph) (id : String) : Nat :=
  (g.successors id).length

end Graph

end Dot4

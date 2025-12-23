import Dot4.Basic
import Dot4.Advanced

/-!
# Graph Validation Helpers

Utilities for validating and analyzing graphs including DAG checking,
node degree analysis, and structural validation.
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

/-- Validation errors that can be detected in a graph -/
inductive ValidationError where
  /-- An edge references a node that doesn't exist -/
  | danglingEdge (src : String) (dst : String) (missing : String)
  /-- Multiple nodes share the same ID -/
  | duplicateNode (id : String)
  /-- A node has an empty string as its ID -/
  | emptyNodeId
  /-- The graph contains a cycle (for DAG validation) -/
  | cyclicGraph (msg : String)
  deriving Repr, BEq

namespace ValidationError

/-- Convert a validation error to a human-readable message -/
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

/-- Topological sort using Kahn's algorithm. Returns None if graph has cycles. -/
partial def topologicalSort (g : Graph) : Option (List String) :=
  let allNodes := g.getNodeIds
  -- Start with nodes that have no incoming edges
  let queue := allNodes.filter (fun n => g.inDegree n == 0)
  -- Track in-degrees as list of pairs
  let inDegrees := allNodes.map (fun n => (n, g.inDegree n))
  -- Helper to lookup in-degree
  let getDeg (degrees : List (String × Nat)) (n : String) : Nat :=
    match degrees.find? (fun p => p.1 == n) with
    | some (_, d) => d
    | none => 0
  -- Helper to update in-degree
  let setDeg (degrees : List (String × Nat)) (n : String) (d : Nat) : List (String × Nat) :=
    degrees.map (fun p => if p.1 == n then (n, d) else p)
  -- Process queue
  let rec process (q : List String) (result : List String)
                  (degrees : List (String × Nat)) : Option (List String) :=
    match q with
    | [] =>
      -- If we processed all nodes, return result; otherwise there's a cycle
      if result.length == allNodes.length then some result.reverse
      else none
    | curr :: rest =>
      let succs := g.successors curr
      -- Decrease in-degree of successors
      let (newDegrees, newQueue) := succs.foldl
        (fun (acc : List (String × Nat) × List String) succ =>
          let (degs, q') := acc
          let deg := getDeg degs succ
          if deg != 0 then
            let newDeg := deg - 1
            let newDegs := setDeg degs succ newDeg
            if newDeg == 0 then (newDegs, succ :: q')
            else (newDegs, q')
          else acc)
        (degrees, rest)
      process newQueue (curr :: result) newDegrees
  process queue [] inDegrees

/-- Find all nodes reachable from a given node using BFS -/
partial def reachable (g : Graph) (startId : String) : List String :=
  let rec bfs (queue : List String) (visited : List String) : List String :=
    match queue with
    | [] => visited
    | curr :: rest =>
      if visited.contains curr then bfs rest visited
      else
        let succs := g.successors curr
        let newNodes := succs.filter (fun s => !visited.contains s && !queue.contains s)
        bfs (rest ++ newNodes) (curr :: visited)
  if g.getNodeIds.contains startId then bfs [startId] []
  else []

/-- Find shortest path between two nodes using BFS. Returns None if no path exists. -/
partial def shortestPath (g : Graph) (startId endId : String) : Option (List String) :=
  if startId == endId then some [startId]
  else
    -- Helper to find parent in list
    let findParent (parents : List (String × String)) (n : String) : Option String :=
      match parents.find? (fun p => p.1 == n) with
      | some (_, p) => some p
      | none => none
    -- BFS with parent tracking as list of pairs
    let rec bfs (queue : List String) (visited : List String)
                (parents : List (String × String)) : Option (List String) :=
      match queue with
      | [] => none  -- No path found
      | curr :: rest =>
        if curr == endId then
          -- Reconstruct path
          let rec buildPath (c : String) (path : List String) : List String :=
            match findParent parents c with
            | none => c :: path
            | some p => buildPath p (c :: path)
          some (buildPath endId [])
        else if visited.contains curr then bfs rest visited parents
        else
          let succs := g.successors curr
          let (newQueue, newParents) := succs.foldl
            (fun (acc : List String × List (String × String)) succ =>
              let (q, pars) := acc
              let inParents := pars.any (fun p => p.1 == succ)
              if visited.contains succ || q.contains succ || inParents
              then acc
              else (q ++ [succ], (succ, curr) :: pars))
            (rest, parents)
          bfs newQueue (curr :: visited) newParents
    if g.getNodeIds.contains startId && g.getNodeIds.contains endId
    then bfs [startId] [] []
    else none

/-- Find strongly connected components using Kosaraju's algorithm -/
partial def stronglyConnectedComponents (g : Graph) : List (List String) :=
  let allNodes := g.getNodeIds
  -- First pass: DFS to get finish order
  let rec dfs1 (curr : String) (visited : List String) (stack : List String)
              : List String × List String :=
    if visited.contains curr then (visited, stack)
    else
      let newVisited := curr :: visited
      let (finalVisited, afterSuccs) := (g.successors curr).foldl
        (fun (acc : List String × List String) succ =>
          let (v, s) := acc
          dfs1 succ v s)
        (newVisited, stack)
      (finalVisited, curr :: afterSuccs)

  let (_, finishOrder) := allNodes.foldl
    (fun (acc : List String × List String) n =>
      let (v, s) := acc
      dfs1 n v s)
    ([], [])

  -- Build reverse graph edges
  let reverseSuccessors (nodeId : String) : List String :=
    g.edgePairs.filterMap fun (src, dst) =>
      if dst == nodeId then some src else none

  -- Second pass: DFS on reverse graph in finish order
  let rec dfs2 (curr : String) (visited : List String) (component : List String)
              : List String × List String :=
    if visited.contains curr then (visited, component)
    else
      let newVisited := curr :: visited
      let newComponent := curr :: component
      (reverseSuccessors curr).foldl
        (fun (acc : List String × List String) pred =>
          let (v, c) := acc
          dfs2 pred v c)
        (newVisited, newComponent)

  let (_, sccs) := finishOrder.foldl
    (fun (acc : List String × List (List String)) n =>
      let (visited, components) := acc
      if visited.contains n then acc
      else
        let (newVisited, component) := dfs2 n visited []
        (newVisited, component :: components))
    ([], [])
  sccs

/-- DFS traversal with pre-order and post-order callbacks -/
partial def dfs (g : Graph) (startId : String)
    (preVisit : String → Unit := fun _ => ())
    (postVisit : String → Unit := fun _ => ()) : List String :=
  let rec go (curr : String) (visited : List String) : List String :=
    if visited.contains curr then visited
    else
      let _ := preVisit curr
      let newVisited := curr :: visited
      let afterSuccs := (g.successors curr).foldl
        (fun v succ => go succ v)
        newVisited
      let _ := postVisit curr
      afterSuccs
  if g.getNodeIds.contains startId then go startId []
  else []

/-- BFS traversal with level tracking -/
partial def bfsWithLevels (g : Graph) (startId : String) : List (String × Nat) :=
  let rec go (queue : List (String × Nat)) (visited : List String)
            (result : List (String × Nat)) : List (String × Nat) :=
    match queue with
    | [] => result.reverse
    | (curr, level) :: rest =>
      if visited.contains curr then go rest visited result
      else
        let succs := g.successors curr
        let newItems := succs.filterMap fun s =>
          if visited.contains s || queue.any (·.1 == s)
          then none
          else some (s, level + 1)
        go (rest ++ newItems) (curr :: visited) ((curr, level) :: result)
  if g.getNodeIds.contains startId then go [(startId, 0)] [] []
  else []

/-- Get connected components (treating graph as undirected) -/
partial def connectedComponents (g : Graph) : List (List String) :=
  let allNodes := g.getNodeIds
  -- Get neighbors in both directions (undirected)
  let neighbors (nodeId : String) : List String :=
    (g.successors nodeId) ++ (g.predecessors nodeId)
  -- BFS to find component
  let rec findComponent (start : String) (visited : List String) : List String × List String :=
    let rec bfs (queue : List String) (v : List String) (comp : List String)
               : List String × List String :=
      match queue with
      | [] => (v, comp)
      | curr :: rest =>
        if v.contains curr then bfs rest v comp
        else
          let newV := curr :: v
          let ns := (neighbors curr).filter (fun n => !v.contains n && !queue.contains n)
          bfs (rest ++ ns) newV (curr :: comp)
    bfs [start] visited []
  -- Find all components
  let (_, components) := allNodes.foldl
    (fun (acc : List String × List (List String)) n =>
      let (visited, comps) := acc
      if visited.contains n then acc
      else
        let (newVisited, comp) := findComponent n visited
        (newVisited, comp :: comps))
    ([], [])
  components

/-- Check if graph is connected (as undirected) -/
def isConnected (g : Graph) : Bool :=
  g.connectedComponents.length <= 1

/-- Compute transitive closure (all reachable pairs) -/
partial def transitiveClosure (g : Graph) : List (String × String) :=
  let allNodes := g.getNodeIds
  allNodes.flatMap fun start =>
    (g.reachable start).filterMap fun dest =>
      if start != dest then some (start, dest) else none

/-- Find all simple paths between two nodes (no repeated nodes) -/
partial def allPaths (g : Graph) (startId endId : String) (maxLen : Nat := 100) : List (List String) :=
  let rec go (curr : String) (path : List String) (depth : Nat) : List (List String) :=
    if depth > maxLen then []
    else if curr == endId then [path.reverse]
    else
      (g.successors curr).flatMap fun succ =>
        if path.contains succ then []  -- No cycles in simple paths
        else go succ (succ :: path) (depth + 1)
  if g.getNodeIds.contains startId && g.getNodeIds.contains endId
  then go startId [startId] 0
  else []

/-- Compute graph diameter (longest shortest path) -/
partial def diameter (g : Graph) : Option Nat :=
  let allNodes := g.getNodeIds
  if allNodes.isEmpty then none
  else
    let maxDist := allNodes.foldl (fun acc start =>
      allNodes.foldl (fun acc2 «end» =>
        match g.shortestPath start «end» with
        | some path => max acc2 (path.length - 1)
        | none => acc2) acc) 0
    some maxDist

/-- Check if there's a path from src to dst -/
def hasPath (g : Graph) (src dst : String) : Bool :=
  (g.reachable src).contains dst

/-- Get all leaf nodes (nodes with no outgoing edges) -/
def leaves (g : Graph) : List String :=
  g.getNodeIds.filter (fun n => g.outDegree n == 0)

/-- Get all root nodes (nodes with no incoming edges) -/
def roots (g : Graph) : List String :=
  g.getNodeIds.filter (fun n => g.inDegree n == 0)

end Graph

end Dot4

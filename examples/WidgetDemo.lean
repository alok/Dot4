import Dot4

/-!
# Dot4 Widget Demo

Open this file in VS Code with the Lean extension to see graphs rendered
as SVG in the infoview panel.

Use {syntax command}`#dot myGraph` to render any Graph.
-/

open Dot4

/-- Basic graph example. -/
def basicGraph := dot {
  digraph "Hello"
  rankdir "LR"

  node "A" label="Hello"
  node "B" label="World"

  edge "A" → "B"
}

#dot basicGraph

/-- Architecture diagram with clusters. -/
def architectureGraph := dot {
  digraph "Architecture"
  rankdir "TB"

  node_defaults style="filled" fillcolor="#E3F2FD"

  cluster "frontend" {
    label "Frontend"
    bgcolor "#FFECB3"
    node "ui" label="UI"
    node "state" label="State"
  }

  cluster "backend" {
    label "Backend"
    bgcolor "#C8E6C9"
    node "api" label="API"
    node "db" label="Database"
  }

  edge "ui" → "api"
  edge "api" → "db"
  chain ["state", "ui", "api"]
}

#dot architectureGraph

-- Raw DOT string
#dot_raw "digraph { rankdir=LR; a -> b -> c -> d }"

/-- Fanout example. -/
def fanoutGraph := dot {
  digraph "Fanout"

  node "hub" label="Hub" shape="circle"
  node "a" label="A"
  node "b" label="B"
  node "c" label="C"

  fanout "hub" → ["a", "b", "c"]
}

#dot fanoutGraph

/-! ## Graph Diff Demo

Compare two graphs - added nodes/edges shown in green, removed in red dashed.
-/

/-- Old version of a graph. -/
def oldGraph := dot {
  digraph "Old"
  rankdir "LR"
  node "A" label="Start"
  node "B" label="Middle"
  node "C" label="End"
  edge "A" → "B"
  edge "B" → "C"
}

/-- New version with modifications. -/
def newGraph := dot {
  digraph "New"
  rankdir "LR"
  node "A" label="Start"
  node "B" label="Middle"
  node "D" label="New Node"
  edge "A" → "B"
  edge "B" → "D"
}

-- Shows diff: C and B→C removed (red dashed), D and B→D added (green)
#dot_diff oldGraph newGraph

/-! ## Topological Sort Animation

Click "Animate" to see nodes highlighted in topological order.
-/

/-- A DAG for topological sort demo. -/
def dagGraph := dot {
  digraph "DAG"
  rankdir "TB"

  node "A" label="A"
  node "B" label="B"
  node "C" label="C"
  node "D" label="D"
  node "E" label="E"

  edge "A" → "B"
  edge "A" → "C"
  edge "B" → "D"
  edge "C" → "D"
  edge "D" → "E"
}

-- Click "Animate" to see topological traversal: A → B → C → D → E
#dot_topo dagGraph

/-! ## Unquoted Identifiers

Node names, graph names, and cluster names can be unquoted identifiers:
-/

/-- Graph using unquoted identifiers for names. -/
def identGraph := dot {
  digraph MyGraph
  rankdir "LR"

  node start label="Start"
  node middle label="Middle"
  node finish label="End"

  edge start → middle
  edge middle → finish
}

#dot identGraph

/-! ## Interpolation Demo

Use {lit}`$(expr)` syntax to interpolate Lean expressions into graph definitions.
-/

/-- Dynamic node labels and graph names using interpolation. -/
def version := "v2.0"
def serverName := "Production"
def nodeColor := "#4CAF50"

def interpolatedGraph := dot {
  digraph $(s!"System_{version}")
  rankdir "LR"

  node "server" label=$(serverName) fillcolor=$nodeColor style="filled"
  node client label=Client

  edge client → server
}

#dot interpolatedGraph

/-- Programmatic node generation with interpolation. -/
def makeNode (name : String) (idx : Nat) : String := s!"{name}_{idx}"

def dynamicGraph := dot {
  digraph "Dynamic"
  rankdir "LR"

  node $(makeNode "task" 1) label="Task 1"
  node $(makeNode "task" 2) label="Task 2"
  node $(makeNode "task" 3) label="Task 3"

  edge $(makeNode "task" 1) → $(makeNode "task" 2)
  edge $(makeNode "task" 2) → $(makeNode "task" 3)
}

#dot dynamicGraph

/-! ## Programmatic Diff API

Use `Graph.diff` to compute structural differences between graphs.
-/

-- Demonstrate programmatic diff
#eval do
  let d := Graph.diff oldGraph newGraph
  IO.println s!"Diff summary: {d.summary}"
  IO.println s!"Added nodes: {d.addedNodes.map (·.id)}"
  IO.println s!"Removed nodes: {d.removedNodes.map (·.id)}"
  IO.println s!"Added edges: {d.addedEdges.map fun e => s!"{e.src}->{e.dst}"}"
  IO.println s!"Removed edges: {d.removedEdges.map fun e => s!"{e.src}->{e.dst}"}"

-- Check structural equality
#eval IO.println s!"oldGraph == newGraph: {Graph.structuralEq oldGraph newGraph}"
#eval IO.println s!"oldGraph == oldGraph: {Graph.structuralEq oldGraph oldGraph}"

/-! ## Graph Algorithms

Comprehensive graph analysis algorithms.
-/

-- Graph properties
#eval IO.println s!"DAG is a DAG: {dagGraph.isDAG}"
#eval IO.println s!"DAG topo sort: {dagGraph.topologicalSort}"
#eval IO.println s!"Reachable from A: {dagGraph.reachable "A"}"
#eval IO.println s!"Shortest A→E: {dagGraph.shortestPath "A" "E"}"
#eval IO.println s!"All paths A→E: {dagGraph.allPaths "A" "E"}"
#eval IO.println s!"BFS levels from A: {dagGraph.bfsWithLevels "A"}"
#eval IO.println s!"Connected components: {dagGraph.connectedComponents}"
#eval IO.println s!"Is connected: {dagGraph.isConnected}"
#eval IO.println s!"Roots: {dagGraph.roots}"
#eval IO.println s!"Leaves: {dagGraph.leaves}"

import Dot4

/-!
# Dot4 Widget Demo

Open this file in VS Code with the Lean extension to see graphs rendered
as SVG in the infoview panel.

Use `#dot myGraph` to render any Graph.
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

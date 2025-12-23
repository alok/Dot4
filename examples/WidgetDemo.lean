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

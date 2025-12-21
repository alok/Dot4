# Dot4: Type-Safe Graphviz DSL for Lean 4

A **type-safe** DSL for generating [Graphviz DOT](https://graphviz.org/doc/info/lang.html) diagrams in Lean 4. Unlike stringly-typed DOT, Dot4 catches errors at compile time.

```lean
import Dot4
open Dot4

def myGraph := dot {
  digraph "Architecture"
  rankdir "LR"

  cluster "backend" {
    label "Backend Services"
    bgcolor "#e3f2fd"
    node "api" label="API Gateway"
    node "db" label="Database"
  }

  node "client" label="Client" shape="box"
  edge "client":e → "api":w
  edge "api" → "db"
}

#eval IO.println myGraph.toDot
```

## Why Dot4?

| Feature | Raw DOT | Dot4 |
|---------|---------|------|
| Typo in `shape="circl"` | Runtime: invisible node | **Compile error** |
| Invalid color `#GGG` | Silent failure | **Type-checked colors** |
| Missing quote | Cryptic parser error | **Lean syntax errors** |
| Refactoring | Find & replace | **IDE rename support** |

## Features

### DSL Syntax
- **Nodes & Edges**: `node "id" label="..." shape="..."` / `edge "a" → "b"`
- **Clusters**: `cluster "name" { ... }` for grouped subgraphs
- **Port Syntax**: `edge "a":port → "b":n` for record nodes and compass points
- **Defaults**: `node_defaults shape="box"` / `edge_defaults arrowsize="0.8"`

### Type Safety
- **60+ validated shapes**: `Shape.box`, `Shape.circle`, `Shape.diamond`, ...
- **Color palettes**: Nord, Solarized, Catppuccin + X11 colors + RGB/Hex
- **Arrow types**: `ArrowShape.normal`, `ArrowShape.diamond`, ...
- **Layout options**: `RankDir.LR`, `SplineType.ortho`, ...

### Graph Operations
- `Graph.merge`: Combine graphs
- `Graph.filterNodes`: Remove nodes by predicate
- `Graph.mapNodes`: Transform all nodes
- `Graph.addChain`: Create edge chains programmatically
- `starGraph`, `linearGraph`, `completeGraph`: Common patterns

### Advanced
- **HTML Labels**: Rich table-based node labels
- **Compass Points**: `:n`, `:ne`, `:e`, `:se`, `:s`, `:sw`, `:w`, `:nw`
- **Record Ports**: `"node":portname` for structured nodes

## Installation

Add to your `lakefile.toml`:

```toml
[[require]]
name = "Dot4"
git = "https://github.com/alok/Dot4"
rev = "main"
```

## Quick Examples

### State Machine

```lean
def stateMachine := dot {
  digraph "FSM"
  rankdir "LR"

  node_defaults shape="circle"

  node "start" shape="point"
  node "idle" label="Idle"
  node "run" label="Running"
  node "done" label="Done" shape="doublecircle"

  edge "start" → "idle"
  edge "idle" → "run" label="start()"
  edge "run" → "run" label="tick()"
  edge "run" → "done" label="finish()"
}
```

### Architecture Diagram with Clusters

```lean
def architecture := dot {
  digraph "System"
  rankdir "TB"
  splines "ortho"

  node_defaults shape="box" style="rounded,filled"

  cluster "frontend" {
    label "Frontend"
    bgcolor "#E3F2FD"
    node "web" label="Web App"
    node "mobile" label="Mobile"
  }

  cluster "backend" {
    label "Backend"
    bgcolor "#E8F5E9"
    node "api" label="API"
    node "worker" label="Worker"
  }

  edge "web" → "api"
  edge "mobile" → "api"
  edge "api" → "worker"
}
```

### Type-Safe Styling

```lean
-- Compile-time checked colors and shapes!
def styledGraph :=
  Graph.digraph "Styled"
  |>.addNode {
    id := "n1"
    label := some "Nord Theme"
    attrs := [
      Attr.shapeT Shape.box,           -- Type-safe shape
      Attr.fillcolorC Color.nordFrost1, -- Type-safe color
      Attr.nodeStyleT NodeStyle.filled
    ]
  }
```

### Record Nodes with Ports

```lean
def dataFlow := dot {
  digraph "DataFlow"
  rankdir "LR"

  node "src" label="{<h>Header|<b>Body|<f>Footer}" shape="record"
  node "dst" label="{<i>Input|<o>Output}" shape="record"

  edge "src":h → "dst":i
  edge "src":b → "dst":i
  edge "dst":o → "next"
}
```

### Programmatic Graph Building

```lean
-- Create a star graph programmatically
def deps := starGraph "Dependencies" "main" ["lib1", "lib2", "lib3"]
  |>.mapNodes (fun n =>
    if n.id == "main"
    then { n with attrs := [Attr.shapeT Shape.box] }
    else n)

-- Merge multiple graphs
def combined := Graph.merge graph1 graph2
  |>.addEdge { src := "g1_node", dst := "g2_node" }
```

## API Reference

### Core Types

```lean
structure Graph       -- Complete graph with nodes, edges, subgraphs
structure Node        -- Node with id, label, attributes
structure Edge        -- Edge with src, dst, ports, attributes
structure Subgraph    -- Subgraph/cluster with nodes, edges
structure Attr        -- Key-value attribute
```

### Type-Safe Enums

```lean
inductive Shape       -- box, circle, diamond, hexagon, star, ...
inductive Color       -- named, hex, rgb, rgba, hsv
inductive ArrowShape  -- normal, diamond, dot, vee, ...
inductive RankDir     -- TB, BT, LR, RL
inductive NodeStyle   -- filled, dashed, bold, rounded, ...
inductive EdgeStyle   -- solid, dashed, dotted, bold, ...
inductive SplineType  -- ortho, spline, polyline, ...
```

### Color Palettes

```lean
-- X11 Colors
Color.red, Color.blue, Color.green, ...

-- Nord
Color.nordFrost0, Color.nordAuroraRed, ...

-- Solarized
Color.solarizedBlue, Color.solarizedBase03, ...

-- Catppuccin Mocha
Color.catppuccinMauve, Color.catppuccinPeach, ...
```

### Graph Builders

```lean
linearGraph "name" ["A", "B", "C"]           -- A → B → C
starGraph "name" "center" ["a", "b", "c"]    -- center → {a, b, c}
completeGraph "name" ["A", "B", "C"]         -- all pairs connected
bipartiteGraph "name" ["L1", "L2"] ["R1"]    -- left ↔ right
```

## Output

Call `.toDot` on any `Graph` to get valid DOT source:

```lean
#eval IO.println myGraph.toDot
```

Pipe to Graphviz:

```bash
lake exe dot4 | dot -Tpng -o output.png
```

## License

MIT

## Contributing

Issues and PRs welcome! The codebase is structured as:

- `Dot4/Basic.lean` - Core types (Graph, Node, Edge, Attr)
- `Dot4/Render.lean` - DOT code generation
- `Dot4/Syntax.lean` - DSL macros
- `Dot4/Colors.lean` - Type-safe colors
- `Dot4/Shapes.lean` - Type-safe shapes and enums
- `Dot4/Advanced.lean` - Graph operations and utilities

/-
Dot4 Documentation - A Type-Safe Graphviz DSL for Lean 4
-/

import VersoManual
import Dot4

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean

open Dot4

set_option pp.rawOnError true

#doc (Manual) "Dot4: Type-Safe Graphviz DSL" =>

%%%
authors := ["Alok Singh"]
%%%

Dot4 is a type-safe domain-specific language for generating [Graphviz DOT](https://graphviz.org/doc/info/lang.html) diagrams in Lean 4.
Unlike stringly-typed DOT, Dot4 catches errors at compile time.

# Why Dot4?

Traditional DOT files are just strings. Typos like `shape="circl"` silently fail.
With Dot4, the Lean compiler catches these errors:

```lean
-- This won't compile - Shape.circl doesn't exist!
-- Attr.shapeT Shape.circl

-- Must use valid shapes
def validShape := Attr.shapeT Shape.circle
```

# Quick Start

## Basic Graph

The simplest way to create a graph is with the `dot { }` DSL:

```lean
def myGraph := dot {
  digraph "MyGraph"
  rankdir "LR"

  node "A" label="Start"
  node "B" label="End"

  edge "A" → "B"
}
```

This generates valid DOT code:

```
digraph "MyGraph" {
    rankdir="LR";

    "A" [label="Start"];
    "B" [label="End"];
    "A" -> "B";
}
```

## Clusters

Group related nodes with the `cluster` syntax:

```lean
def clusteredGraph := dot {
  digraph "Architecture"
  rankdir "TB"

  cluster "frontend" {
    label "Frontend"
    bgcolor "#E3F2FD"
    node "ui" label="UI"
    node "state" label="State"
  }

  cluster "backend" {
    label "Backend"
    bgcolor "#E8F5E9"
    node "api" label="API"
    node "db" label="Database"
  }

  edge "ui" → "api"
  edge "api" → "db"
}
```

# Type-Safe Attributes
%%%
tag := "type-safe"
%%%

## Shapes

Dot4 provides all 60+ Graphviz shapes as a type-safe enum:

```lean
-- All valid shapes
def shapes := [
  Shape.box,
  Shape.circle,
  Shape.diamond,
  Shape.hexagon,
  Shape.star,
  Shape.cylinder,
  Shape.folder,
  Shape.component
]
```

Use them with `Attr.shapeT`:

```lean
def styledNode := Node.mk "n1" (some "My Node")
  [Attr.shapeT Shape.diamond]
```

## Colors

Choose from multiple color palettes:

- *X11 Colors*: `Color.red`, `Color.blue`, ...
- *Nord*: `Color.nordFrost0`, `Color.nordAuroraRed`, ...
- *Solarized*: `Color.solarizedBlue`, ...
- *Catppuccin*: `Color.catppuccinMauve`, ...

Or use RGB/Hex:

```lean
def customColors := [
  Color.hex "#FF5733",
  Color.rgb 255 87 51,
  Color.rgba 255 87 51 128
]
```

## Arrow Shapes

Type-safe arrow heads and tails:

```lean
def arrowAttrs := [
  Attr.arrowheadT ArrowShape.diamond,
  Attr.arrowtailT ArrowShape.dot,
  Attr.dirT EdgeDir.both
]
```

# Port Syntax
%%%
tag := "ports"
%%%

## Compass Points

Connect edges to specific sides of nodes:

```lean
def compassEdges := dot {
  digraph "Compass"

  node "center" label="Center"
  node "north" label="North"

  edge "center":n → "north":s   -- north to south
  edge "center":e → "east":w    -- east to west
}
```

Available compass points: `n`, `ne`, `e`, `se`, `s`, `sw`, `w`, `nw`, `c`

## Record Ports

For record-shaped nodes, connect to named ports:

```lean
def recordGraph := dot {
  digraph "Records"
  rankdir "LR"

  node "struct" label="{<f0>F0|<f1>F1}" shape="record"
  node "target" label="Target"

  edge "struct":f0 → "target"
  edge "struct":f1 → "target"
}
```

# Edge Syntax Sugar
%%%
tag := "edge-sugar"
%%%

## Edge Chains

Create sequential connections with `chain`:

```lean
def pipeline := dot {
  digraph "Pipeline"
  rankdir "LR"

  node "A" label="Input"
  node "B" label="Process"
  node "C" label="Transform"
  node "D" label="Output"

  chain ["A", "B", "C", "D"]  -- A → B → C → D
}
```

## Bidirectional Edges

Use `↔` or `<->` for two-way connections:

```lean
def clientServer := dot {
  digraph "Communication"

  node "client" label="Client"
  node "server" label="Server"

  edge "client" ↔ "server" label="request/response"
}
```

## Fanout Edges

Connect one node to many targets with `fanout`:

```lean
def hubAndSpoke := dot {
  digraph "Hub"

  node "hub" label="Router" shape="circle"
  node "a" label="Node A"
  node "b" label="Node B"
  node "c" label="Node C"

  fanout "hub" → ["a", "b", "c"]
}
```

# Strict Graphs
%%%
tag := "strict"
%%%

Use `strict digraph` or `strict graph` to prevent multi-edges between the same node pair:

```lean
def noMultiEdges := dot {
  strict digraph "StrictMode"

  node "A" label="Source"
  node "B" label="Target"

  -- Only the last edge is kept in strict mode
  edge "A" → "B" label="first"
  edge "A" → "B" label="second"  -- This one wins
}
```

# Cluster Edge Routing
%%%
tag := "cluster-edges"
%%%

Route edges to cluster boundaries using lhead/ltail (requires `compound=true`):

```lean
def clusterToCluster :=
  let src := Subgraph.cluster "source"
    |>.withAttr (Attr.mk "label" "Source")
    |>.addNode { id := "s1", attrs := [] }

  let dst := Subgraph.cluster "target"
    |>.withAttr (Attr.mk "label" "Target")
    |>.addNode { id := "t1", attrs := [] }

  -- Edge routes to cluster boundaries, not individual nodes
  let clusterEdge := (Edge.new "s1" "t1")
    |>.fromCluster "source"
    |>.toCluster "target"

  Graph.digraph "ClusterRouting"
    |>.withAttr (Attr.compound true)
    |>.addSubgraph src
    |>.addSubgraph dst
    |>.addEdge clusterEdge
```

# Compile-Time Validation
%%%
tag := "validation"
%%%

Dot4 validates attribute values at compile time:

```lean
-- These compile (valid values):
def valid := dot {
  digraph "Valid"
  node "n" shape="box" fillcolor="#FF5733"
}

-- These would fail to compile:
-- shape="circl"     -- Error: Invalid shape
-- fillcolor="redd"  -- Error: Invalid color
-- rankdir="UP"      -- Error: Invalid rankdir
```

Validation covers: `shape`, `color`, `fillcolor`, `fontcolor`, `bgcolor`, `rankdir`, `rank`, `arrowhead`, `arrowtail`, `dir`, `splines`, `layout`, `overlap`, `labelloc`, `labeljust`, `outputorder`, and `style`.

Unknown attributes pass through for Graphviz compatibility.

# Graph Builders
%%%
tag := "builders"
%%%

Create common graph patterns programmatically:

```lean
-- Linear chain: A → B → C → D
def myChain := linearGraph "Chain" ["A", "B", "C", "D"]

-- Star graph: hub connected to all spokes
def star := starGraph "Star" "hub" ["s1", "s2", "s3"]

-- Complete graph: all nodes connected
def complete := completeGraph "Complete" ["A", "B", "C"]
```

# Graph Operations
%%%
tag := "operations"
%%%

Transform graphs functionally:

```lean
def transformed :=
  starGraph "Base" "center" ["a", "b", "c"]
  |>.mapNodes (fun n =>
    if n.id == "center"
    then { n with attrs := [Attr.shapeT Shape.circle] }
    else n)
  |>.filterEdges (fun e => e.dst != "c")
```

Available operations:
- `Graph.merge` - Combine two graphs
- `Graph.mapNodes` - Transform all nodes
- `Graph.mapEdges` - Transform all edges
- `Graph.filterNodes` - Keep nodes matching predicate
- `Graph.filterEdges` - Keep edges matching predicate
- `Graph.addChain` - Add a sequence of edges

# Defaults
%%%
tag := "defaults"
%%%

Set default attributes for all nodes or edges:

```lean
def withDefaults := dot {
  digraph "Styled"

  node_defaults shape="box" style="filled"
  edge_defaults arrowsize="0.7" color="#1976D2"

  node "a" label="Node A"
  node "b" label="Node B"

  edge "a" → "b"
}
```

# Rendering

Convert any graph to DOT format with `.toDot`:

```lean
#eval IO.println myGraph.toDot
```

Pipe to Graphviz:

```
lake exe dot4 | dot -Tpng -o output.png
```

# API Reference
%%%
tag := "api"
%%%

## Core Types

| Type | Description |
|------|-------------|
| `Graph` | Complete graph with nodes, edges, subgraphs |
| `Node` | Node with id, label, attributes |
| `Edge` | Edge with src, dst, ports, attributes |
| `Subgraph` | Cluster/subgraph container |
| `Attr` | Key-value attribute pair |

## Type-Safe Enums

| Type | Values |
|------|--------|
| `Shape` | `box`, `circle`, `diamond`, `hexagon`, ... (60+) |
| `Color` | `named`, `hex`, `rgb`, `rgba`, `hsv` |
| `ArrowShape` | `normal`, `diamond`, `dot`, `vee`, `crow`, ... |
| `RankDir` | `TB`, `BT`, `LR`, `RL` |
| `NodeStyle` | `filled`, `dashed`, `bold`, `rounded`, ... |
| `EdgeStyle` | `solid`, `dashed`, `dotted`, `bold`, ... |
| `SplineType` | `ortho`, `spline`, `polyline`, `curved`, ... |
| `LayoutEngine` | `dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, ... |
| `OverlapMode` | `true`, `false`, `scale`, `ortho`, `compress`, ... |
| `LabelLoc` | `t` (top), `c` (center), `b` (bottom) |
| `LabelJust` | `l` (left), `c` (center), `r` (right) |

## Edge Helpers

| Method | Description |
|--------|-------------|
| `Edge.toCluster` | Route edge head to cluster boundary |
| `Edge.fromCluster` | Route edge tail from cluster boundary |
| `Edge.betweenClusters` | Set both lhead and ltail |

## DSL Syntax

| Syntax | Description |
|--------|-------------|
| `chain ["A", "B", "C"]` | Create edge chain A → B → C |
| `edge "A" ↔ "B"` | Bidirectional edge (two arrows) |
| `fanout "hub" → ["a", "b"]` | One-to-many edges |
| `strict digraph "Name"` | No multi-edges allowed |


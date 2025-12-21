import Dot4

open Dot4

/-! # Dot4 Examples

Comprehensive examples demonstrating all features of the Dot4 DSL.
-/

/-! ## Basic Usage -/

/-- Example 1: Simple directed graph -/
def simpleGraph : Graph := dot {
  digraph "SimpleGraph"
  rankdir "LR"

  node "A" label="Start"
  node "B" label="Middle"
  node "C" label="End"

  edge "A" → "B"
  edge "B" → "C"
}

/-- Example 2: State machine with styling -/
def stateMachine : Graph := dot {
  digraph "StateMachine"
  rankdir "LR"

  node_defaults shape="circle" fontname="Helvetica"
  edge_defaults fontsize="10"

  node "start" shape="point"
  node "idle" label="Idle"
  node "running" label="Running"
  node "done" label="Done" shape="doublecircle"

  edge "start" → "idle"
  edge "idle" → "running" label="run()"
  edge "running" → "running" label="tick()"
  edge "running" → "done" label="finish()"
  edge "done" → "idle" label="reset()"
}

/-! ## Clusters and Subgraphs -/

/-- Example 3: Clustered architecture diagram -/
def architectureDiagram : Graph := dot {
  digraph "Architecture"
  rankdir "TB"
  compound "true"
  splines "ortho"

  node_defaults shape="box" style="rounded,filled" fillcolor="#e8e8e8"
  edge_defaults arrowsize="0.8"

  cluster "frontend" {
    label "Frontend"
    bgcolor "#fff3e0"
    node "ui" label="UI Components"
    node "state" label="State Manager"
    node "api_client" label="API Client"
  }

  cluster "backend" {
    label "Backend"
    bgcolor "#e3f2fd"
    node "router" label="Router"
    node "handlers" label="Handlers"
    node "db" label="Database"
  }

  -- Cross-cluster edges
  edge "ui" → "state"
  edge "state" → "api_client"
  edge "api_client":s → "router":n
  edge "router" → "handlers"
  edge "handlers" → "db"
}

/-! ## Port Syntax -/

/-- Example 4: Record nodes with ports -/
def recordNodes : Graph := dot {
  digraph "DataFlow"
  rankdir "LR"
  nodesep "1.0"

  node "input" label="{<h>Header|<d>Data|<f>Footer}" shape="record"
  node "process" label="{<i>Input|Process|<o>Output}" shape="record"
  node "output" label="{<a>Channel A|<b>Channel B}" shape="record"

  edge "input":h → "process":i
  edge "input":d → "process":i
  edge "process":o → "output":a
  edge "process":o → "output":b
}

/-- Example 5: Compass points for precise edge routing -/
def compassEdges : Graph := dot {
  digraph "Compass"
  node_defaults shape="box" width="1.5" height="1.0"

  node "center" label="Center"
  node "north" label="North"
  node "east" label="East"
  node "south" label="South"
  node "west" label="West"

  edge "center":n → "north":s
  edge "center":e → "east":w
  edge "center":s → "south":n
  edge "center":w → "west":e
}

/-! ## Type-Safe Attributes -/

/-- Example 6: Using type-safe colors (compile-time checked!) -/
def colorfulGraph : Graph :=
  Graph.digraph "Colorful"
  |>.withAttr (Attr.bgcolorC Color.nordPolarNight0)
  |>.withAttr (Attr.rankdirT RankDir.LR)
  |>.addNode {
    id := "node1"
    label := some "Nord Theme"
    attrs := [
      Attr.shapeT Shape.box,
      Attr.fillcolorC Color.nordFrost1,
      Attr.fontcolorC Color.nordSnowStorm2,
      Attr.nodeStyleT NodeStyle.filled
    ]
  }
  |>.addNode {
    id := "node2"
    label := some "Solarized"
    attrs := [
      Attr.shapeT Shape.ellipse,
      Attr.fillcolorC Color.solarizedBlue,
      Attr.fontcolorC Color.solarizedBase3,
      Attr.nodeStyleT NodeStyle.filled
    ]
  }
  |>.addNode {
    id := "node3"
    label := some "Catppuccin"
    attrs := [
      Attr.shapeT Shape.diamond,
      Attr.fillcolorC Color.catppuccinMauve,
      Attr.fontcolorC Color.catppuccinCrust,
      Attr.nodeStyleT NodeStyle.filled
    ]
  }
  |>.addEdge { src := "node1", dst := "node2", attrs := [Attr.colorC Color.nordAuroraGreen] }
  |>.addEdge { src := "node2", dst := "node3", attrs := [Attr.colorC Color.solarizedMagenta] }

/-- Example 7: Type-safe shapes (try changing to an invalid shape - it won't compile!) -/
def shapesShowcase : Graph :=
  let shapes := [
    ("box", Shape.box),
    ("circle", Shape.circle),
    ("diamond", Shape.diamond),
    ("hexagon", Shape.hexagon),
    ("star", Shape.star),
    ("cylinder", Shape.cylinder),
    ("folder", Shape.folder),
    ("component", Shape.component)
  ]
  let g := Graph.digraph "Shapes" |>.withAttr (Attr.rankdirT RankDir.LR)
  shapes.foldl (fun acc (name, shape) =>
    acc.addNode { id := name, label := some name, attrs := [Attr.shapeT shape] }
  ) g

/-! ## Graph Transformations -/

/-- Example 8: Programmatic graph building -/
def programmaticGraph : Graph :=
  -- Start with a star graph
  starGraph "Dependencies" "main" ["lib1", "lib2", "lib3", "lib4"]
  |>.withAttr (Attr.rankdirT RankDir.TB)
  |>.mapNodes (fun n =>
    if n.id == "main"
    then { n with attrs := [Attr.shapeT Shape.box, Attr.nodeStyleT NodeStyle.bold] }
    else { n with attrs := [Attr.shapeT Shape.ellipse] })

/-- Example 9: Graph operations -/
def mergedGraph : Graph :=
  let g1 := linearGraph "Part1" ["A", "B", "C"]
  let g2 := linearGraph "Part2" ["X", "Y", "Z"]
  let combined := Graph.merge g1 g2
  combined
  |>.addEdge { src := "C", dst := "X", label := some "connect" }
  |>.withAttr (Attr.rankdirT RankDir.LR)

/-! ## HTML Labels -/

/-- Example 10: Rich HTML table labels -/
def htmlLabelGraph : Graph :=
  let tableLabel := htmlTable [
    htmlRow [
      cell "Header" (port := some "h"),
      { content := "Status", bgcolor := some "#90EE90" }
    ],
    htmlRow [
      cell "Row 1",
      cell "OK"
    ],
    htmlRow [
      cell "Row 2",
      cell "Pending"
    ]
  ]
  Graph.digraph "HTMLLabels"
  |>.addNode { id := "table1", attrs := [tableLabel.toLabel, Attr.shapeT Shape.none] }
  |>.addNode { id := "output", label := some "Output" }
  |>.addEdge { src := "table1", dst := "output" }

/-! ## Complex Example -/

/-- Example 11: Complete software architecture -/
def fullArchitecture : Graph := dot {
  digraph "SoftwareArchitecture"
  rankdir "TB"
  splines "ortho"
  nodesep "0.8"
  ranksep "1.0"
  fontname "Helvetica"

  node_defaults shape="box" style="rounded,filled" fontname="Helvetica" fontsize="11"
  edge_defaults fontsize="9" arrowsize="0.7"

  -- Presentation Layer
  cluster "presentation" {
    label "Presentation Layer"
    bgcolor "#E3F2FD"
    node "web_ui" label="Web UI" fillcolor="#BBDEFB"
    node "mobile" label="Mobile App" fillcolor="#BBDEFB"
    node "cli" label="CLI" fillcolor="#BBDEFB"
  }

  -- Application Layer
  cluster "application" {
    label "Application Layer"
    bgcolor "#E8F5E9"
    node "auth" label="Auth Service" fillcolor="#C8E6C9"
    node "api" label="API Gateway" fillcolor="#C8E6C9"
    node "worker" label="Background Worker" fillcolor="#C8E6C9"
  }

  -- Domain Layer
  cluster "domain" {
    label "Domain Layer"
    bgcolor "#FFF3E0"
    node "users" label="Users" fillcolor="#FFE0B2"
    node "orders" label="Orders" fillcolor="#FFE0B2"
    node "products" label="Products" fillcolor="#FFE0B2"
  }

  -- Infrastructure Layer
  cluster "infra" {
    label "Infrastructure"
    bgcolor "#F3E5F5"
    node "postgres" label="PostgreSQL" fillcolor="#E1BEE7"
    node "redis" label="Redis" fillcolor="#E1BEE7"
    node "s3" label="S3 Storage" fillcolor="#E1BEE7"
  }

  -- Edges: Presentation -> Application
  edge "web_ui" → "api"
  edge "mobile" → "api"
  edge "cli" → "api"

  -- Edges: Application -> Domain
  edge "api" → "auth"
  edge "api" → "users"
  edge "api" → "orders"
  edge "api" → "products"
  edge "worker" → "orders"

  -- Edges: Domain -> Infrastructure
  edge "users" → "postgres"
  edge "orders" → "postgres"
  edge "products" → "postgres"
  edge "auth" → "redis"
  edge "products" → "s3"
}

/-! ## Main Entry Point -/

def main : IO Unit := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║                    Dot4 - DOT DSL for Lean 4                 ║"
  IO.println "║          Type-safe • Compile-time checked • Extensible       ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  IO.println "━━━ Example 1: Simple Graph ━━━"
  IO.println simpleGraph.toDot
  IO.println ""

  IO.println "━━━ Example 2: State Machine ━━━"
  IO.println stateMachine.toDot
  IO.println ""

  IO.println "━━━ Example 3: Architecture with Clusters ━━━"
  IO.println architectureDiagram.toDot
  IO.println ""

  IO.println "━━━ Example 4: Record Nodes with Ports ━━━"
  IO.println recordNodes.toDot
  IO.println ""

  IO.println "━━━ Example 5: Compass Edge Routing ━━━"
  IO.println compassEdges.toDot
  IO.println ""

  IO.println "━━━ Example 6: Type-Safe Colors ━━━"
  IO.println colorfulGraph.toDot
  IO.println ""

  IO.println "━━━ Example 11: Full Architecture ━━━"
  IO.println fullArchitecture.toDot

import Dot4

open Dot4

/-!
# Dot4 Test Suite

Visual tests for all Dot4 features. Run with:
```
lake exe dot4-test
```

Each test outputs a numbered DOT file for visual inspection.
-/

/-- Test 1: Basic graph with nodes and edges -/
def test01_basic : Graph := dot {
  digraph "Test01_Basic"
  rankdir "LR"

  node "a" label="Node A"
  node "b" label="Node B"
  node "c" label="Node C"

  edge "a" → "b"
  edge "b" → "c"
  edge "c" → "a"
}

/-- Test 2: All basic shapes -/
def test02_shapes : Graph := dot {
  digraph "Test02_Shapes"
  rankdir "TB"

  node "box" shape="box" label="box"
  node "circle" shape="circle" label="circle"
  node "diamond" shape="diamond" label="diamond"
  node "ellipse" shape="ellipse" label="ellipse"
  node "hexagon" shape="hexagon" label="hexagon"
  node "octagon" shape="octagon" label="octagon"
  node "star" shape="star" label="star"
  node "triangle" shape="triangle" label="triangle"
  node "cylinder" shape="cylinder" label="cylinder"
  node "note" shape="note" label="note"

  edge "box" → "circle"
  edge "circle" → "diamond"
  edge "diamond" → "ellipse"
  edge "ellipse" → "hexagon"
  edge "hexagon" → "octagon"
  edge "octagon" → "star"
  edge "star" → "triangle"
  edge "triangle" → "cylinder"
  edge "cylinder" → "note"
}

/-- Test 3: Node and edge styling -/
def test03_styling : Graph := dot {
  digraph "Test03_Styling"
  rankdir "LR"

  node "filled" style="filled" fillcolor="#FFB6C1" label="Filled"
  node "bold" style="bold" label="Bold"
  node "dashed" style="dashed" label="Dashed"
  node "dotted" style="dotted" label="Dotted"
  node "rounded" style="rounded" shape="box" label="Rounded"

  edge "filled" → "bold" style="bold"
  edge "bold" → "dashed" style="dashed"
  edge "dashed" → "dotted" style="dotted"
  edge "dotted" → "rounded"
}

/-- Test 4: Clusters -/
def test04_clusters : Graph := dot {
  digraph "Test04_Clusters"
  rankdir "TB"

  cluster "group1" {
    label "Group 1"
    bgcolor "#E8F5E9"
    node "a1" label="A1"
    node "a2" label="A2"
    node "a3" label="A3"
  }

  cluster "group2" {
    label "Group 2"
    bgcolor "#E3F2FD"
    node "b1" label="B1"
    node "b2" label="B2"
  }

  cluster "group3" {
    label "Group 3"
    bgcolor "#FFF3E0"
    node "c1" label="C1"
  }

  edge "a1" → "a2"
  edge "a2" → "a3"
  edge "a3" → "b1"
  edge "b1" → "b2"
  edge "b2" → "c1"
}

/-- Test 5: Compass port edges -/
def test05_compass : Graph := dot {
  digraph "Test05_Compass"

  node_defaults shape="box" width="1.5" height="1.0"

  node "center" label="Center"
  node "north" label="North"
  node "south" label="South"
  node "east" label="East"
  node "west" label="West"
  node "ne" label="NE"
  node "nw" label="NW"
  node "se" label="SE"
  node "sw" label="SW"

  edge "center":n → "north":s
  edge "center":s → "south":n
  edge "center":e → "east":w
  edge "center":w → "west":e
  edge "center":ne → "ne":sw
  edge "center":nw → "nw":se
  edge "center":se → "se":nw
  edge "center":sw → "sw":ne
}

/-- Test 6: Record nodes with ports -/
def test06_records : Graph := dot {
  digraph "Test06_Records"
  rankdir "LR"

  node "struct1" label="{<f0>field0|<f1>field1|<f2>field2}" shape="record"
  node "struct2" label="{<a>A|<b>B|<c>C}" shape="record"
  node "struct3" label="{{<x>X|<y>Y}|<z>Z}" shape="record"

  edge "struct1":f0 → "struct2":a
  edge "struct1":f1 → "struct2":b
  edge "struct1":f2 → "struct2":c
  edge "struct2":c → "struct3":x
}

/-- Test 7: Colors from palettes -/
def test07_colors : Graph :=
  let n1 := Node.mk "nord1" (some "Nord Frost")
    [Attr.shapeT Shape.box, Attr.fillcolorC Color.nordFrost1,
     Attr.fontcolorC Color.nordSnowStorm2, Attr.nodeStyleT NodeStyle.filled]
  let n2 := Node.mk "nord2" (some "Nord Aurora")
    [Attr.shapeT Shape.box, Attr.fillcolorC Color.nordAuroraGreen,
     Attr.fontcolorC Color.nordPolarNight0, Attr.nodeStyleT NodeStyle.filled]
  let n3 := Node.mk "solar" (some "Solarized")
    [Attr.shapeT Shape.box, Attr.fillcolorC Color.solarizedBlue,
     Attr.fontcolorC Color.solarizedBase3, Attr.nodeStyleT NodeStyle.filled]
  let n4 := Node.mk "cat" (some "Catppuccin")
    [Attr.shapeT Shape.box, Attr.fillcolorC Color.catppuccinMauve,
     Attr.fontcolorC Color.catppuccinCrust, Attr.nodeStyleT NodeStyle.filled]
  Graph.digraph "Test07_Colors"
  |>.withAttr (Attr.rankdirT RankDir.LR)
  |>.withAttr (Attr.bgcolorC Color.nordPolarNight0)
  |>.addNode n1 |>.addNode n2 |>.addNode n3 |>.addNode n4
  |>.addEdge (Edge.mk "nord1" "nord2" {} {} none [Attr.colorC Color.nordFrost2])
  |>.addEdge (Edge.mk "nord2" "solar" {} {} none [Attr.colorC Color.solarizedCyan])
  |>.addEdge (Edge.mk "solar" "cat" {} {} none [Attr.colorC Color.catppuccinPink])

/-- Test 8: Arrow shapes -/
def test08_arrows : Graph :=
  Graph.digraph "Test08_Arrows"
  |>.withAttr (Attr.rankdirT RankDir.LR)
  |>.addNode { id := "a" } |>.addNode { id := "b" }
  |>.addNode { id := "c" } |>.addNode { id := "d" }
  |>.addNode { id := "e" } |>.addNode { id := "f" }
  |>.addEdge { src := "a", dst := "b", attrs := [Attr.arrowheadT ArrowShape.normal, Attr.mk "label" "normal"] }
  |>.addEdge { src := "a", dst := "c", attrs := [Attr.arrowheadT ArrowShape.diamond, Attr.mk "label" "diamond"] }
  |>.addEdge { src := "a", dst := "d", attrs := [Attr.arrowheadT ArrowShape.dot, Attr.mk "label" "dot"] }
  |>.addEdge { src := "a", dst := "e", attrs := [Attr.arrowheadT ArrowShape.vee, Attr.mk "label" "vee"] }
  |>.addEdge { src := "a", dst := "f", attrs := [Attr.arrowheadT ArrowShape.crow, Attr.mk "label" "crow"] }

/-- Test 9: Spline types -/
def test09_splines_ortho : Graph := dot {
  digraph "Test09_Ortho"
  splines "ortho"
  rankdir "TB"

  node_defaults shape="box"

  node "a" label="Start"
  node "b" label="Step 1"
  node "c" label="Step 2"
  node "d" label="End"

  edge "a" → "b"
  edge "a" → "c"
  edge "b" → "d"
  edge "c" → "d"
}

/-- Test 10: Graph operations - star graph -/
def test10_star : Graph :=
  starGraph "Test10_Star" "hub" ["spoke1", "spoke2", "spoke3", "spoke4", "spoke5", "spoke6"]
  |>.withAttr (Attr.rankdirT RankDir.TB)
  |>.mapNodes (fun n =>
    if n.id == "hub"
    then { n with attrs := [Attr.shapeT Shape.doublecircle, Attr.nodeStyleT NodeStyle.filled, Attr.fillcolor "#FFD700"] }
    else { n with attrs := [Attr.shapeT Shape.circle] })

/-- Test 11: Linear graph chain -/
def test11_linear : Graph :=
  linearGraph "Test11_Linear" ["A", "B", "C", "D", "E", "F"]
  |>.withAttr (Attr.rankdirT RankDir.LR)
  |>.mapNodes (fun n => { n with attrs := [Attr.shapeT Shape.box] })

/-- Test 12: Graph with defaults -/
def test12_defaults : Graph := dot {
  digraph "Test12_Defaults"
  rankdir "LR"

  node_defaults shape="box" style="filled" fillcolor="#B3E5FC" fontname="Helvetica"
  edge_defaults arrowsize="0.7" color="#1976D2"

  node "a" label="Node A"
  node "b" label="Node B"
  node "c" label="Node C"
  node "special" label="Override" fillcolor="#FFCDD2"

  edge "a" → "b"
  edge "b" → "c"
  edge "c" → "special"
}

/-- Test 13: Unicode and special chars -/
def test13_unicode : Graph := dot {
  digraph "Test13_Unicode"
  rankdir "LR"

  node "alpha" label="α β γ"
  node "math" label="∀x∈ℝ: x²≥0"
  node "arrows" label="← → ↔ ⇒"
  node "quote" label="Say \"Hello\""

  edge "alpha" → "math"
  edge "math" → "arrows"
  edge "arrows" → "quote"
}

/-- Test 14: Complex architecture -/
def test14_architecture : Graph := dot {
  digraph "Test14_Architecture"
  rankdir "TB"
  splines "ortho"
  nodesep "0.8"
  ranksep "1.0"

  node_defaults shape="box" style="rounded,filled" fontname="Helvetica"
  edge_defaults arrowsize="0.7"

  cluster "ui" {
    label "UI Layer"
    bgcolor "#FFECB3"
    node "web" label="Web" fillcolor="#FFE082"
    node "mobile" label="Mobile" fillcolor="#FFE082"
  }

  cluster "api" {
    label "API Layer"
    bgcolor "#C8E6C9"
    node "gateway" label="Gateway" fillcolor="#A5D6A7"
    node "auth" label="Auth" fillcolor="#A5D6A7"
  }

  cluster "data" {
    label "Data Layer"
    bgcolor "#BBDEFB"
    node "cache" label="Cache" fillcolor="#90CAF9"
    node "db" label="Database" fillcolor="#90CAF9"
  }

  edge "web" → "gateway"
  edge "mobile" → "gateway"
  edge "gateway" → "auth"
  edge "gateway" → "cache"
  edge "auth" → "cache"
  edge "cache" → "db"
}

/-- Test 15: Edge labels -/
def test15_edge_labels : Graph := dot {
  digraph "Test15_EdgeLabels"
  rankdir "LR"

  node_defaults shape="circle"

  node "s0" label="0"
  node "s1" label="1"
  node "s2" label="2"

  edge "s0" → "s0" label="a"
  edge "s0" → "s1" label="b"
  edge "s1" → "s1" label="a,b"
  edge "s1" → "s2" label="c"
  edge "s2" → "s0" label="reset"
}

-- Collect all tests
def allTests : List (String × Graph) := [
  ("01_basic", test01_basic),
  ("02_shapes", test02_shapes),
  ("03_styling", test03_styling),
  ("04_clusters", test04_clusters),
  ("05_compass", test05_compass),
  ("06_records", test06_records),
  ("07_colors", test07_colors),
  ("08_arrows", test08_arrows),
  ("09_splines", test09_splines_ortho),
  ("10_star", test10_star),
  ("11_linear", test11_linear),
  ("12_defaults", test12_defaults),
  ("13_unicode", test13_unicode),
  ("14_architecture", test14_architecture),
  ("15_edge_labels", test15_edge_labels)
]

def main : IO Unit := do
  IO.println "Dot4 Test Suite - Generating DOT files"
  IO.println "======================================="

  for (testName, g) in allTests do
    let dotContent := g.toDot
    let filename := s!"test/output/{testName}.dot"
    IO.FS.writeFile filename dotContent
    IO.println s!"✓ {filename}"

  IO.println ""
  IO.println "To generate PNGs, run:"
  IO.println "  ./test/render_all.sh"

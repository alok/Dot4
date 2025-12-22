import Dot4

/-!
# Test New DSL Features

Tests for:
1. Edge chain syntax
2. Bidirectional edges
3. Multi-target (fanout) edges
4. Compile-time attribute validation
-/

open Dot4

-- Test 1: Edge chain syntax
def testChain : Graph := dot {
  digraph "ChainTest"
  rankdir "LR"

  node "A" label="Start"
  node "B" label="Step 1"
  node "C" label="Step 2"
  node "D" label="End"

  chain ["A", "B", "C", "D"]
}

-- Test 2: Bidirectional edges
def testBidirectional : Graph := dot {
  digraph "BidirectionalTest"

  node "client" label="Client"
  node "server" label="Server"

  edge "client" ↔ "server" label="request/response"
}

-- Test 3: Fanout edges (multi-target)
def testFanout : Graph := dot {
  digraph "FanoutTest"

  node "hub" label="Hub" shape="circle"
  node "a" label="Node A"
  node "b" label="Node B"
  node "c" label="Node C"

  fanout "hub" → ["a", "b", "c"]
}

-- Test 4: Combined features
def testCombined : Graph := dot {
  digraph "CombinedTest"
  rankdir "TB"

  node_defaults style="filled" fillcolor="#E3F2FD"

  cluster "input" {
    label "Inputs"
    bgcolor "#FFECB3"
    node "i1" label="Input 1"
    node "i2" label="Input 2"
  }

  node "process" label="Process" shape="box"
  node "out1" label="Out 1"
  node "out2" label="Out 2"
  node "out3" label="Out 3"

  edge "i1" → "process"
  edge "i2" → "process"

  fanout "process" → ["out1", "out2", "out3"]
}

-- Test 5: Cluster edge routing (lhead/ltail) - using programmatic API
def testClusterEdges : Graph :=
  let sg1 := Subgraph.cluster "source"
    |>.withAttr (Attr.mk "label" "Source")
    |>.addNode { id := "s1", attrs := [Attr.mk "label" "S1"] }
    |>.addNode { id := "s2", attrs := [Attr.mk "label" "S2"] }
  let sg2 := Subgraph.cluster "target"
    |>.withAttr (Attr.mk "label" "Target")
    |>.addNode { id := "t1", attrs := [Attr.mk "label" "T1"] }
    |>.addNode { id := "t2", attrs := [Attr.mk "label" "T2"] }
  -- Edge from s1 to t1, routed to cluster boundaries
  let clusterEdge := (Edge.new "s1" "t1").fromCluster "source" |>.toCluster "target"
  Graph.digraph "ClusterEdges"
    |>.withAttr (Attr.compound true)  -- Required for lhead/ltail
    |>.addSubgraph sg1
    |>.addSubgraph sg2
    |>.addEdge clusterEdge
    |>.addEdge (Edge.new "s2" "t2")  -- Regular edge between individual nodes

-- Test 6: Strict graph (no multi-edges)
def testStrictGraph : Graph := dot {
  strict digraph "StrictTest"
  rankdir "LR"

  node "A" label="Node A"
  node "B" label="Node B"

  -- In strict mode, only one edge between A and B will be kept
  edge "A" → "B" label="first"
  edge "A" → "B" label="second"
  edge "A" → "B" label="third"
}

-- Test 7: Validation test - these should compile successfully
def testValidAttributes : Graph := dot {
  digraph "ValidationTest"
  rankdir "LR"

  -- Valid shapes
  node "n1" shape="box"
  node "n2" shape="circle"
  node "n3" shape="diamond"

  -- Valid colors
  node "c1" fillcolor="#FF5733"
  node "c2" fillcolor="red"
  node "c3" fillcolor="lightblue"

  -- Valid styles
  node_defaults style="filled,rounded"
}

-- Helper to save DOT file and render PNG
def saveAndRender (name : String) (g : Graph) : IO Unit := do
  let dotContent := g.toDot
  let dotPath := s!"test/output/{name}.dot"
  let pngPath := s!"test/output/{name}.png"

  -- Save DOT file
  IO.FS.writeFile dotPath dotContent
  IO.println s!"Saved: {dotPath}"

  -- Render PNG with Graphviz
  let result ← IO.Process.output {
    cmd := "dot"
    args := #["-Tpng", "-o", pngPath, dotPath]
  }
  if result.exitCode == 0 then
    IO.println s!"Rendered: {pngPath}"
  else
    IO.eprintln s!"Graphviz error: {result.stderr}"

-- Print all tests and generate visual output
def main : IO Unit := do
  -- Create output directory
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", "test/output"] }

  IO.println "=== Chain Test ==="
  IO.println testChain.toDot
  IO.println ""

  IO.println "=== Bidirectional Test ==="
  IO.println testBidirectional.toDot
  IO.println ""

  IO.println "=== Fanout Test ==="
  IO.println testFanout.toDot
  IO.println ""

  IO.println "=== Combined Test ==="
  IO.println testCombined.toDot
  IO.println ""

  IO.println "=== Cluster Edges Test ==="
  IO.println testClusterEdges.toDot
  IO.println ""

  IO.println "=== Strict Graph Test ==="
  IO.println testStrictGraph.toDot
  IO.println ""

  IO.println "=== Validation Test ==="
  IO.println testValidAttributes.toDot
  IO.println ""

  -- Save and render all graphs
  IO.println "\n=== Rendering graphs to PNGs ==="
  saveAndRender "chain" testChain
  saveAndRender "bidirectional" testBidirectional
  saveAndRender "fanout" testFanout
  saveAndRender "combined" testCombined
  saveAndRender "cluster_edges" testClusterEdges
  saveAndRender "strict" testStrictGraph
  saveAndRender "validation" testValidAttributes

  IO.println "\n✓ All graphs rendered to test/output/"

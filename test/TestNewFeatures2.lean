import Dot4

/-!
Test DSL features: sameRank, cluster defaults, colorscheme, unquoted identifiers
-/

open Dot4

set_option linter.missingDocs false

-- Test 1: sameRank syntax
def testSameRank := dot {
  digraph "SameRank"
  rankdir "TB"

  node "A" label="Top"
  node "B" label="Left"
  node "C" label="Right"
  node "D" label="Bottom"

  -- Force B and C to be on the same horizontal level
  sameRank ["B", "C"]

  edge "A" → "B"
  edge "A" → "C"
  edge "B" → "D"
  edge "C" → "D"
}

#eval IO.println testSameRank.toDot

-- Test 2: Cluster with node_defaults
def testClusterDefaults := dot {
  digraph "ClusterDefaults"

  cluster "frontend" {
    label "Frontend"
    bgcolor "#FFECB3"
    node_defaults style="filled" fillcolor="#FFE082"
    edge_defaults color="blue"
    node "ui" label="UI"
    node "state" label="State"
    edge "ui" → "state"
  }

  cluster "backend" {
    label "Backend"
    bgcolor "#C8E6C9"
    node_defaults style="filled" fillcolor="#A5D6A7"
    node "api" label="API"
    node "db" label="Database"
  }

  edge "state" → "api"
}

#eval IO.println testClusterDefaults.toDot

-- Test 3: Colorscheme attribute
def testColorscheme := dot {
  digraph "Colors"
  colorscheme "blues9"

  node "n1" color="1"
  node "n2" color="5"
  node "n3" color="9"

  chain ["n1", "n2", "n3"]
}

#eval IO.println testColorscheme.toDot

-- Test 4: Unquoted identifiers
def testUnquoted := dot {
  digraph "Unquoted"

  -- Unquoted node IDs
  node start label="Begin"
  node middle label="Process"
  node «end» label="End"

  -- Unquoted in edges
  edge start → middle
  edge middle → «end»
}

#eval IO.println testUnquoted.toDot

-- Test 5: Unquoted in clusters
def testUnquotedCluster := dot {
  digraph "UnquotedCluster"

  cluster "inputs" {
    label "Inputs"
    node inputA label="A"
    node inputB label="B"
  }

  node processor label="Process"
  edge inputA → processor
  edge inputB → processor
}

#eval IO.println testUnquotedCluster.toDot

-- Test 6: Combined features
def testCombined := dot {
  digraph "Combined"
  rankdir "LR"
  colorscheme "set312"

  cluster "sources" {
    label "Data Sources"
    node_defaults style="filled" fillcolor="1"
    node src1 label="Source 1"
    node src2 label="Source 2"
    node src3 label="Source 3"
  }

  sameRank ["src1", "src2", "src3"]

  node transform label="Transform" shape="box"
  node output label="Output"

  fanout transform → ["src1", "src2", "src3"]
  edge transform → output
}

#eval IO.println testCombined.toDot

-- Test compound edges with lhead/ltail DSL syntax
def testCompoundEdges : Graph := dot {
  digraph "CompoundTest"
  compound "true"

  cluster "backend" {
    label "Backend Services"
    node "api" label="API"
    node "db" label="Database"
  }

  cluster "frontend" {
    label "Frontend"
    node "web" label="Web App"
  }

  -- Edge from web to api, visually connecting to cluster boundaries
  edge "web" → "api" lhead="cluster_backend" ltail="cluster_frontend"
}

#eval IO.println testCompoundEdges.toDot

-- Test polygon shape parameters
def testPolygonShapes : Graph :=
  Graph.digraph "PolygonShapes"
  |>.withAttr (Attr.rankdirT RankDir.LR)
  -- Pentagon (5 sides)
  |>.addNode {
    id := "pentagon"
    label := some "Pentagon"
    attrs := [Attr.shapeT Shape.polygon, Attr.sides 5, Attr.regular true]
  }
  -- Octagon (8 sides)
  |>.addNode {
    id := "octagon"
    label := some "Octagon"
    attrs := [Attr.shapeT Shape.polygon, Attr.sides 8]
  }
  -- Distorted hexagon
  |>.addNode {
    id := "distorted"
    label := some "Distorted"
    attrs := [Attr.shapeT Shape.polygon, Attr.sides 6, Attr.distortion 0.5]
  }
  -- Skewed square
  |>.addNode {
    id := "skewed"
    label := some "Skewed"
    attrs := [Attr.shapeT Shape.polygon, Attr.sides 4, Attr.skew 0.3]
  }
  -- Rotated triangle
  |>.addNode {
    id := "rotated"
    label := some "Rotated"
    attrs := [Attr.shapeT Shape.polygon, Attr.sides 3, Attr.orientation 30.0]
  }
  |>.addEdge { src := "pentagon", dst := "octagon" }
  |>.addEdge { src := "octagon", dst := "distorted" }
  |>.addEdge { src := "distorted", dst := "skewed" }
  |>.addEdge { src := "skewed", dst := "rotated" }

#eval IO.println testPolygonShapes.toDot

-- Test fdp/sfdp layout parameters
def testFdpLayout : Graph :=
  Graph.graph "FdpLayout"  -- undirected graph for fdp
  |>.withAttr (Attr.layout "fdp")
  |>.withAttr (Attr.k 2.0)  -- ideal edge length
  |>.withAttr (Attr.epsilon 0.01)  -- convergence threshold
  |>.withAttr (Attr.start "random")  -- random initialization
  -- Create a small network
  |>.addNode { id := "center", label := some "Hub" }
  |>.addNode { id := "n1", label := some "Node 1" }
  |>.addNode { id := "n2", label := some "Node 2" }
  |>.addNode { id := "n3", label := some "Node 3" }
  |>.addNode { id := "n4", label := some "Node 4" }
  |>.addEdge { src := "center", dst := "n1" }
  |>.addEdge { src := "center", dst := "n2" }
  |>.addEdge { src := "center", dst := "n3" }
  |>.addEdge { src := "center", dst := "n4" }
  |>.addEdge { src := "n1", dst := "n2" }
  |>.addEdge { src := "n3", dst := "n4" }

#eval IO.println testFdpLayout.toDot

-- Test graph algorithms
def testGraphAlgorithms : IO Unit := do
  -- Build a simple DAG: A -> B -> C -> D, A -> C
  let dag := Graph.digraph "DAG"
    |>.addNode { id := "A", label := some "A" }
    |>.addNode { id := "B", label := some "B" }
    |>.addNode { id := "C", label := some "C" }
    |>.addNode { id := "D", label := some "D" }
    |>.addEdge { src := "A", dst := "B" }
    |>.addEdge { src := "B", dst := "C" }
    |>.addEdge { src := "C", dst := "D" }
    |>.addEdge { src := "A", dst := "C" }

  -- Test topological sort
  IO.println s!"DAG: {dag.isDAG}"
  match dag.topologicalSort with
  | some order => IO.println s!"Topological order: {order}"
  | none => IO.println "Has cycle!"

  -- Test reachability
  IO.println s!"Reachable from A: {dag.reachable \"A\"}"

  -- Test shortest path
  match dag.shortestPath "A" "D" with
  | some path => IO.println s!"Shortest path A->D: {path}"
  | none => IO.println "No path!"

  -- Test SCC on a graph with cycles
  let cyclic := Graph.digraph "Cyclic"
    |>.addNode { id := "1" }
    |>.addNode { id := "2" }
    |>.addNode { id := "3" }
    |>.addNode { id := "4" }
    |>.addEdge { src := "1", dst := "2" }
    |>.addEdge { src := "2", dst := "3" }
    |>.addEdge { src := "3", dst := "1" }  -- cycle 1-2-3
    |>.addEdge { src := "3", dst := "4" }

  IO.println s!"Cyclic graph DAG: {cyclic.isDAG}"
  IO.println s!"SCCs: {cyclic.stronglyConnectedComponents}"

#eval testGraphAlgorithms

-- All tests pass if this file compiles
#check "All new features compile successfully!"

import Dot4

/-!
# Test New Features: sameRank, cluster node_defaults, colorscheme, unquoted identifiers
-/

open Dot4

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

-- All tests pass if this file compiles
#check "All new features compile successfully!"

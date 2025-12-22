import Dot4

/-!
# Test Advanced Features: Record Nodes, Validation, Typed Enums
-/

open Dot4

-- Test 1: Record nodes with DSL
def testRecordDSL := dot {
  digraph "RecordTest"
  rankdir "LR"

  -- Simple record with ports
  record "struct1" ["f0" : "left", "f1" : "middle", "f2" : "right"]

  -- Record with nested rows
  record "struct2" [
    {"a" : "top", "b" : "bottom"},
    {"c" : "left", "d" : "right"}
  ]

  -- Rounded record
  mrecord "rounded" ["port1" : "Field 1", "port2" : "Field 2"]

  edge "struct1":f1 → "struct2"
  edge "struct2" → "rounded"
}

#eval IO.println testRecordDSL.toDot

-- Test 2: Record nodes programmatic API
def testRecordAPI : Graph :=
  let rec1 := mkRecord "table1" [
    .field' "col1" "Name",
    .field' "col2" "Age",
    .field' "col3" "City"
  ]
  let rec2 := mkMrecord "table2" [
    .row [.field' "a" "Row 1"],
    .row [.field' "b" "Row 2"]
  ]
  Graph.digraph "RecordAPI"
  |>.addRecordNode rec1
  |>.addRecordNode rec2
  |>.addEdge { src := "table1", dst := "table2" }

#eval IO.println testRecordAPI.toDot
#eval IO.println s!"Ports in table1: {(mkRecord "table1" [.field' "col1" "Name", .field' "col2" "Age"]).ports}"

-- Test 3: Validation helpers
def validGraph := dot {
  digraph "Valid"
  node "A"
  node "B"
  node "C"
  edge "A" → "B"
  edge "B" → "C"
}

def cyclicGraph := dot {
  digraph "Cyclic"
  node "A"
  node "B"
  node "C"
  edge "A" → "B"
  edge "B" → "C"
  edge "C" → "A"  -- Creates cycle
}

#eval IO.println s!"Valid graph isDAG: {validGraph.isDAG}"
#eval IO.println s!"Cyclic graph isDAG: {cyclicGraph.isDAG}"
#eval IO.println s!"Valid graph errors: {validGraph.validate}"
#eval IO.println s!"Valid graph isValid: {validGraph.isValid}"

-- Test 4: Graph analysis
def analysisGraph := dot {
  digraph "Analysis"
  node "source1"
  node "source2"
  node "middle"
  node "sink1"
  node "sink2"

  edge "source1" → "middle"
  edge "source2" → "middle"
  edge "middle" → "sink1"
  edge "middle" → "sink2"
}

#eval IO.println s!"Node count: {analysisGraph.nodeCount}"
#eval IO.println s!"Edge count: {analysisGraph.edgeCount}"
#eval IO.println s!"Sources: {analysisGraph.sources}"
#eval IO.println s!"Sinks: {analysisGraph.sinks}"
#eval IO.println s!"Degree of 'middle': {analysisGraph.degree "middle"}"
#eval IO.println s!"In-degree of 'middle': {analysisGraph.inDegree "middle"}"
#eval IO.println s!"Out-degree of 'middle': {analysisGraph.outDegree "middle"}"

-- Test 5: Typed enums (already in Shapes.lean)
def testTypedEnums := dot {
  digraph "TypedEnums"

  node "n1" shape="box"      -- validated at compile time
  node "n2" shape="circle"
  node "n3" shape="diamond"

  edge "n1" → "n2"
  edge "n2" → "n3"
}

-- Using typed constructors directly
def testTypedConstructors : Graph :=
  Graph.digraph "TypedConstructors"
  |>.withAttr (Attr.layoutT LayoutEngine.dot)
  |>.withAttr (Attr.rankdirT RankDir.LR)
  |>.addNode { id := "a", attrs := [Attr.shapeT Shape.box, Attr.nodeStyleT NodeStyle.filled] }
  |>.addNode { id := "b", attrs := [Attr.shapeT Shape.circle] }
  |>.addEdge { src := "a", dst := "b", attrs := [Attr.arrowheadT ArrowShape.diamond] }

#eval IO.println testTypedConstructors.toDot

-- All tests pass if this file compiles
#check "All advanced features compile successfully!"

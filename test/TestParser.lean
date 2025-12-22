import Dot4

/-!
# Parser Tests

Tests for the DOT parser including basic parsing, round-trip, and error cases.
-/

open Dot4

set_option linter.missingDocs false

/-- Test 1: Minimal digraph -/
def test_minimal_digraph : IO Unit := do
  let input := "digraph G { }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.name == "G"
    assert! g.direction == .directed
    assert! g.nodes.isEmpty
    assert! g.edges.isEmpty
    IO.println "test_minimal_digraph: PASSED"
  | .error e =>
    IO.println s!"test_minimal_digraph: FAILED - {e}"

/-- Test 2: Minimal undirected graph -/
def test_minimal_graph : IO Unit := do
  let input := "graph G { }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.direction == .undirected
    IO.println "test_minimal_graph: PASSED"
  | .error e =>
    IO.println s!"test_minimal_graph: FAILED - {e}"

/-- Test 3: Strict graph -/
def test_strict_graph : IO Unit := do
  let input := "strict digraph G { }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.strict == true
    IO.println "test_strict_graph: PASSED"
  | .error e =>
    IO.println s!"test_strict_graph: FAILED - {e}"

/-- Test 4: Simple nodes -/
def test_simple_nodes : IO Unit := do
  let input := "digraph G { A; B; C; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.nodes.length == 3
    IO.println "test_simple_nodes: PASSED"
  | .error e =>
    IO.println s!"test_simple_nodes: FAILED - {e}"

/-- Test 5: Simple edges -/
def test_simple_edges : IO Unit := do
  let input := "digraph G { A -> B; B -> C; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.edges.length == 2
    -- Nodes should be created implicitly
    assert! g.nodes.length == 3
    IO.println "test_simple_edges: PASSED"
  | .error e =>
    IO.println s!"test_simple_edges: FAILED - {e}"

/-- Test 6: Edge chains -/
def test_edge_chains : IO Unit := do
  let input := "digraph G { A -> B -> C -> D; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.edges.length == 3  -- A->B, B->C, C->D
    assert! g.nodes.length == 4
    IO.println "test_edge_chains: PASSED"
  | .error e =>
    IO.println s!"test_edge_chains: FAILED - {e}"

/-- Test 7: Node with attributes -/
def test_node_attrs : IO Unit := do
  let input := "digraph G { A [shape=box, color=red]; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.nodes.length == 1
    match g.nodes with
    | [n] =>
      assert! n.attrs.length == 2
      IO.println "test_node_attrs: PASSED"
    | _ => IO.println "test_node_attrs: FAILED - wrong node count"
  | .error e =>
    IO.println s!"test_node_attrs: FAILED - {e}"

/-- Test 8: Edge with attributes -/
def test_edge_attrs : IO Unit := do
  let input := "digraph G { A -> B [label=test, color=blue]; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.edges.length == 1
    match g.edges with
    | [e] =>
      assert! e.label == some "test"
      IO.println "test_edge_attrs: PASSED"
    | _ => IO.println "test_edge_attrs: FAILED - wrong edge count"
  | .error e =>
    IO.println s!"test_edge_attrs: FAILED - {e}"

/-- Test 9: Quoted strings -/
def test_quoted_strings : IO Unit := do
  let input := "digraph \"My Graph\" { \"Node A\" -> \"Node B\"; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.name == "My Graph"
    assert! g.nodes.length == 2
    IO.println "test_quoted_strings: PASSED"
  | .error e =>
    IO.println s!"test_quoted_strings: FAILED - {e}"

/-- Test 10: Graph attributes -/
def test_graph_attrs : IO Unit := do
  let input := "digraph G { rankdir=LR; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.attrs.length == 1
    IO.println "test_graph_attrs: PASSED"
  | .error e =>
    IO.println s!"test_graph_attrs: FAILED - {e}"

/-- Test 11: Node defaults -/
def test_node_defaults : IO Unit := do
  let input := "digraph G { node [shape=box]; A; B; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.nodeDefaults.length == 1
    IO.println "test_node_defaults: PASSED"
  | .error e =>
    IO.println s!"test_node_defaults: FAILED - {e}"

/-- Test 12: Subgraph -/
def test_subgraph : IO Unit := do
  let input := "digraph G { subgraph cluster_0 { A; B; } }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.subgraphs.length == 1
    match g.subgraphs with
    | [sg] =>
      assert! sg.name == "cluster_0"
      assert! sg.nodes.length == 2
      IO.println "test_subgraph: PASSED"
    | _ => IO.println "test_subgraph: FAILED - wrong subgraph count"
  | .error e =>
    IO.println s!"test_subgraph: FAILED - {e}"

/-- Test 13: Comments -/
def test_comments : IO Unit := do
  let input := "/* comment */ digraph G { // line comment\n A -> B; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.edges.length == 1
    IO.println "test_comments: PASSED"
  | .error e =>
    IO.println s!"test_comments: FAILED - {e}"

/-- Test 14: Undirected edges -/
def test_undirected_edges : IO Unit := do
  let input := "graph G { A -- B; B -- C; }"
  match Graph.fromDot input with
  | .ok g =>
    assert! g.direction == .undirected
    assert! g.edges.length == 2
    IO.println "test_undirected_edges: PASSED"
  | .error e =>
    IO.println s!"test_undirected_edges: FAILED - {e}"

/-- Test 15: Round-trip basic -/
def test_roundtrip_basic : IO Unit := do
  let original := dot {
    digraph "Test"
    node "A" label="Node A"
    node "B" label="Node B"
    edge "A" -> "B"
  }
  let dotStr := original.toDot
  match Graph.fromDot dotStr with
  | .ok parsed =>
    -- Check node count matches
    assert! parsed.nodes.length == original.nodes.length
    assert! parsed.edges.length == original.edges.length
    IO.println "test_roundtrip_basic: PASSED"
  | .error e =>
    IO.println s!"test_roundtrip_basic: FAILED - {e}"

/-- Run all tests -/
def main : IO Unit := do
  IO.println "Running Parser Tests..."
  IO.println "========================"
  test_minimal_digraph
  test_minimal_graph
  test_strict_graph
  test_simple_nodes
  test_simple_edges
  test_edge_chains
  test_node_attrs
  test_edge_attrs
  test_quoted_strings
  test_graph_attrs
  test_node_defaults
  test_subgraph
  test_comments
  test_undirected_edges
  test_roundtrip_basic
  IO.println "========================"
  IO.println "All tests completed!"

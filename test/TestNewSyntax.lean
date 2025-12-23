import Dot4

/-!
# New Syntax Tests

Tests for unquoted attribute values and edge chain arrows.
-/

open Dot4

set_option linter.missingDocs false

/-- Test unquoted attribute values -/
def testUnquotedAttrs : IO Unit := do
  let g := dot {
    digraph "UnquotedTest"
    node "A" shape=box color=red
    node "B" shape=circle color=blue
    edge "A" -> "B" style=dashed
  }
  let dotStr := g.toDot
  IO.println "=== Unquoted Attributes Test ==="
  IO.println dotStr
  -- Check node count (should have 2 nodes with attrs)
  assert! g.nodes.length == 2
  IO.println "PASSED"

/-- Test edge chains with arrows -/
def testEdgeChains : IO Unit := do
  let g := dot {
    digraph "ChainTest"
    node "A" label="Start"
    node "B" label="Middle"
    node "C" label="End"
    edge "A" -> "B" -> "C"
  }
  let dotStr := g.toDot
  IO.println "\n=== Edge Chain Test ==="
  IO.println dotStr
  -- Should have 2 edges: A->B and B->C
  assert! g.edges.length == 2
  IO.println "PASSED"

/-- Test edge chains with unicode arrows -/
def testEdgeChainsUnicode : IO Unit := do
  let g := dot {
    digraph "UnicodeChainTest"
    edge "X" → "Y" → "Z" → "W"
  }
  let dotStr := g.toDot
  IO.println "\n=== Unicode Edge Chain Test ==="
  IO.println dotStr
  -- Should have 3 edges: X->Y, Y->Z, Z->W
  assert! g.edges.length == 3
  IO.println "PASSED"

/-- Test numeric attribute values -/
def testNumericAttrs : IO Unit := do
  let g := dot {
    digraph "NumericTest"
    node "A" fontsize=12 width=2
  }
  let dotStr := g.toDot
  IO.println "\n=== Numeric Attributes Test ==="
  IO.println dotStr
  -- Check that we have a node with 2 attributes
  assert! g.nodes.length == 1
  match g.nodes with
  | [n] => assert! n.attrs.length == 2
  | _ => pure ()
  IO.println "PASSED"

def main : IO Unit := do
  IO.println "Testing New DSL Syntax Features"
  IO.println "================================"
  testUnquotedAttrs
  testEdgeChains
  testEdgeChainsUnicode
  testNumericAttrs
  IO.println "\n================================"
  IO.println "All new syntax tests passed!"

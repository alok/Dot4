import Dot4

open Dot4

/-- Example: Macro expansion tree (like what we generate for Lean) -/
def macroExpansionGraph : Graph := dot {
  digraph "MacroExpansion"
  rankdir "TB"
  bgcolor "#1e1e1e"

  node "n0" label="center"
  node "n1" label="b2"
  node "n2" label="(4 : Pos)"
  node "n3" label="Pos.mk 4"

  edge "n0" → "n1"
  edge "n1" → "n2"
  edge "n2" → "n3"
}

/-- Example: Simple state machine -/
def stateMachine : Graph := dot {
  digraph "StateMachine"
  rankdir "LR"

  node "start" shape="circle" label=""
  node "s1" label="Idle"
  node "s2" label="Running"
  node "s3" label="Done" shape="doublecircle"

  edge "start" → "s1"
  edge "s1" → "s2" label="run"
  edge "s2" → "s2" label="tick"
  edge "s2" → "s3" label="finish"
}

def main : IO Unit := do
  IO.println "=== Macro Expansion Graph ==="
  IO.println macroExpansionGraph.toDot
  IO.println ""
  IO.println "=== State Machine ==="
  IO.println stateMachine.toDot

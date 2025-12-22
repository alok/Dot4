import Dot4.Basic
import Dot4.Render
import Lean
import ProofWidgets.Component.Basic

/-!
# Dot4 Interactive Widget

Provides interactive graph visualization in the VS Code infoview using viz.js.

## Usage

```lean
open Dot4 in
def myGraph := dot {
  digraph "Example"
  node "A" label="Hello"
  node "B" label="World"
  edge "A" → "B"
}

#dot myGraph  -- Shows interactive graph in infoview
```
-/

namespace Dot4

open Lean Widget Server Elab Command Meta Term
open ProofWidgets

/-- Props for the Dot4 visualization widget -/
structure DotWidgetProps where
  dot : String
  deriving Server.RpcEncodable

/-- The widget module containing the viz.js-based graph renderer -/
@[widget_module]
def DotVisualization : Component DotWidgetProps where
  javascript := include_str ".." / "build" / "js" / "dotVisualization.js"

/-- Helper to evaluate a Graph.toDot expression -/
unsafe def evalGraphToDot (stx : Syntax) : TermElabM String := do
  let expr ← elabTerm stx (some (Lean.mkConst ``Graph))
  let toDotExpr ← mkAppM ``Graph.toDot #[expr]
  -- Create a thunk and evaluate it
  let strExpr ← whnf toDotExpr
  -- Use native evaluation
  evalExpr String (Lean.mkConst ``String) strExpr

/-- Display a Dot4 graph in the infoview panel.

Usage: `#dot myGraph`

This renders the graph using Graphviz (via viz.js) directly in VS Code.
-/
syntax (name := showDotCmd) "#dot " term : command

@[command_elab showDotCmd]
unsafe def elabShowDotCmd : CommandElab := fun
  | stx@`(#dot $g:term) => do
    -- Elaborate the graph and extract DOT string
    let dotStr ← liftTermElabM <| evalGraphToDot g
    -- Save the widget with the DOT string as props
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash DotVisualization.javascript)
      (return json% { dot: $(dotStr) })
      stx
  | stx => throwError "Unexpected syntax {stx}."

/-- Display a raw DOT string in the infoview panel.

Usage: `#dot_raw "digraph { a -> b }"`
-/
syntax (name := showDotRawCmd) "#dot_raw " str : command

@[command_elab showDotRawCmd]
def elabShowDotRawCmd : CommandElab := fun
  | stx@`(#dot_raw $s:str) => do
    let dotStr := s.getString
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash DotVisualization.javascript)
      (return json% { dot: $(dotStr) })
      stx
  | stx => throwError "Unexpected syntax {stx}."

end Dot4

import Dot4.Render

/-!
# DOT DSL Syntax

Custom syntax for defining DOT graphs in Lean.

## Usage

```lean
open Dot4 in
def myGraph := dot {
  digraph "MyGraph"
  rankdir "LR"

  node "A" label="Start" shape="circle"
  node "B" label="End" shape="doublecircle"

  edge "A" → "B" label="go"
}
```
-/

namespace Dot4

-- Attribute syntax: key=value
declare_syntax_cat dotKV
syntax ident "=" str : dotKV

-- Graph element syntax
declare_syntax_cat dotElem

-- node "id" key=value ...
syntax "node" str dotKV* : dotElem

-- edge "src" → "dst" key=value ...
syntax "edge" str "→" str dotKV* : dotElem
syntax "edge" str "->" str dotKV* : dotElem

-- Graph-level attributes
syntax "digraph" str : dotElem
syntax "graph" str : dotElem
syntax ident str : dotElem  -- generic attr like rankdir "TB"

-- Main graph block
syntax "dot" "{" dotElem* "}" : term

/-- Convert key-value pairs to attribute list -/
def parseKVs (kvs : Lean.TSyntaxArray `dotKV) : Lean.MacroM (Lean.TSyntax `term) := do
  let attrs ← kvs.mapM fun kv => do
    match kv with
    | `(dotKV| $key:ident = $val:str) =>
      `(Dot4.Attr.mk $(Lean.quote (toString key.getId)) $val)
    | _ => Lean.Macro.throwUnsupported
  `([ $[$attrs],* ])

macro_rules
  | `(dot { $elems* }) => do
    let mut graphExpr ← `(Graph.empty)

    for elem in elems do
      graphExpr ← match elem with
        | `(dotElem| digraph $name:str) =>
          `({ $graphExpr with name := $name, direction := .directed })
        | `(dotElem| graph $name:str) =>
          `({ $graphExpr with name := $name, direction := .undirected })
        | `(dotElem| rankdir $dir:str) =>
          `(Graph.withAttr $graphExpr (Attr.rankdir $dir))
        | `(dotElem| bgcolor $c:str) =>
          `(Graph.withAttr $graphExpr (Attr.bgcolor $c))
        | `(dotElem| fontname $f:str) =>
          `(Graph.withAttr $graphExpr (Attr.fontname $f))
        | `(dotElem| node $id:str $kvs:dotKV*) => do
          let attrList ← parseKVs kvs
          `(Graph.addNode $graphExpr { id := $id, attrs := $attrList })
        | `(dotElem| edge $src:str → $dst:str $kvs:dotKV*) => do
          let attrList ← parseKVs kvs
          `(Graph.addEdge $graphExpr { src := $src, dst := $dst, attrs := $attrList })
        | `(dotElem| edge $src:str -> $dst:str $kvs:dotKV*) => do
          let attrList ← parseKVs kvs
          `(Graph.addEdge $graphExpr { src := $src, dst := $dst, attrs := $attrList })
        | `(dotElem| $key:ident $val:str) =>
          `(Graph.withAttr $graphExpr (Attr.mk $(Lean.quote (toString key.getId)) $val))
        | _ => Lean.Macro.throwUnsupported

    pure graphExpr

end Dot4

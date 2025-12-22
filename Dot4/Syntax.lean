import Dot4.Render
import Dot4.Elab

/-!
# DOT DSL Syntax

Custom syntax for defining DOT graphs in Lean.

## Usage

```lean
open Dot4 in
def myGraph := dot {
  digraph "MyGraph"
  rankdir "LR"

  -- Node/edge defaults
  node_defaults shape="box" fontname="Arial"
  edge_defaults arrowsize="0.8"

  -- Subgraphs/clusters
  cluster "inputs" {
    label "Inputs"
    node "A" label="Start"
    node "B" label="Input"
  }

  -- Nodes with attributes
  node "C" label="Process" shape="circle"

  -- Edges with port syntax
  edge "A" → "C" label="go"
  edge "B":e → "C":w  -- port syntax
}
```
-/

namespace Dot4

-- Attribute syntax: key=value
declare_syntax_cat dotKV
syntax ident "=" str : dotKV

-- Port/compass syntax
declare_syntax_cat dotPort
syntax ":" ident : dotPort

-- Node ID with optional port
declare_syntax_cat dotNodeRef
syntax str : dotNodeRef
syntax str dotPort : dotNodeRef
syntax str dotPort dotPort : dotNodeRef

-- Graph element syntax
declare_syntax_cat dotElem

-- node "id" key=value ...
syntax "node" str dotKV* : dotElem

-- edge with various port combinations
syntax "edge" dotNodeRef "→" dotNodeRef dotKV* : dotElem
syntax "edge" dotNodeRef "->" dotNodeRef dotKV* : dotElem

-- Bidirectional edges (creates two edges)
syntax "edge" dotNodeRef "↔" dotNodeRef dotKV* : dotElem
syntax "edge" dotNodeRef "<->" dotNodeRef dotKV* : dotElem

-- Edge chains: chain "A" → "B" → "C" → "D"
-- Simplified to chain with explicit node list
syntax "chain" "[" str,+ "]" dotKV* : dotElem

-- Multi-target edges: fanout "hub" → ["a", "b", "c"]
syntax "fanout" dotNodeRef "→" "[" str,* "]" dotKV* : dotElem
syntax "fanout" dotNodeRef "->" "[" str,* "]" dotKV* : dotElem

-- Node/edge defaults
syntax "node_defaults" dotKV+ : dotElem
syntax "edge_defaults" dotKV+ : dotElem

-- Subgraph/cluster block syntax
declare_syntax_cat subgraphElem
syntax "node" str dotKV* : subgraphElem
syntax "edge" dotNodeRef "→" dotNodeRef dotKV* : subgraphElem
syntax "edge" dotNodeRef "->" dotNodeRef dotKV* : subgraphElem
syntax ident str : subgraphElem  -- subgraph attr like label "foo"

-- Cluster and subgraph
syntax "cluster" str "{" subgraphElem* "}" : dotElem
syntax "subgraph" str "{" subgraphElem* "}" : dotElem

-- Graph-level attributes
syntax "digraph" str : dotElem
syntax "graph" str : dotElem
syntax "strict" "digraph" str : dotElem  -- strict digraph (no multi-edges)
syntax "strict" "graph" str : dotElem    -- strict graph (no multi-edges)
syntax ident str : dotElem  -- generic attr like rankdir "TB"

-- Main graph block
syntax "dot" "{" dotElem* "}" : term

/-- Convert key-value pairs to attribute list with validation -/
def parseKVs (kvs : Lean.TSyntaxArray `dotKV) : Lean.MacroM (Lean.TSyntax `term) := do
  let attrs ← kvs.mapM fun kv => do
    match kv with
    | `(dotKV| $key:ident = $val:str) =>
      let keyStr := toString key.getId
      let valStr := val.getString
      -- Validate the attribute at macro expansion time
      Dot4.validateAttrM keyStr valStr
      `(Dot4.Attr.mk $(Lean.quote keyStr) $val)
    | _ => Lean.Macro.throwUnsupported
  `([ $[$attrs],* ])

/-- Parse a compass direction from identifier -/
def parseCompass (id : Lean.Ident) : Lean.MacroM (Lean.TSyntax `term) := do
  let name := toString id.getId
  match name with
  | "n" => `(Compass.n)
  | "ne" => `(Compass.ne)
  | "e" => `(Compass.e)
  | "se" => `(Compass.se)
  | "s" => `(Compass.s)
  | "sw" => `(Compass.sw)
  | "w" => `(Compass.w)
  | "nw" => `(Compass.nw)
  | "c" => `(Compass.c)
  | _ => `(Compass.c)  -- default to center

/-- Parse a port specification -/
def parsePort (p : Lean.TSyntax `dotPort) : Lean.MacroM (Lean.TSyntax `term) := do
  match p with
  | `(dotPort| : $id:ident) =>
    let name := toString id.getId
    -- Check if it's a compass direction
    if name ∈ ["n", "ne", "e", "se", "s", "sw", "w", "nw", "c"] then
      let compass ← parseCompass id
      `(Port.fromCompass $compass)
    else
      `(Port.fromName $(Lean.quote name))
  | _ => `(Port.mk none none)

/-- Parse node reference to get (id, port) -/
def parseNodeRef (ref : Lean.TSyntax `dotNodeRef) : Lean.MacroM (Lean.TSyntax `term × Lean.TSyntax `term) := do
  match ref with
  | `(dotNodeRef| $id:str) =>
    pure (id, ← `(Port.mk none none))
  | `(dotNodeRef| $id:str $p:dotPort) =>
    let port ← parsePort p
    pure (id, port)
  | `(dotNodeRef| $id:str $p1:dotPort $p2:dotPort) =>
    -- p1 is port name, p2 is compass
    match p1, p2 with
    | `(dotPort| : $name:ident), `(dotPort| : $compassId:ident) =>
      let dir ← parseCompass compassId
      let port ← `(Port.mk' $(Lean.quote (toString name.getId)) $dir)
      pure (id, port)
    | _, _ => pure (id, ← `(Port.mk none none))
  | _ => Lean.Macro.throwUnsupported

/-- Parse subgraph elements -/
def parseSubgraphElems (elems : Lean.TSyntaxArray `subgraphElem) (sgExpr : Lean.TSyntax `term)
    : Lean.MacroM (Lean.TSyntax `term) := do
  let mut result := sgExpr
  for elem in elems do
    result ← match elem with
      | `(subgraphElem| node $id:str $kvs:dotKV*) => do
        let attrList ← parseKVs kvs
        `(Subgraph.addNode $result { id := $id, attrs := $attrList })
      | `(subgraphElem| edge $src:dotNodeRef → $dst:dotNodeRef $kvs:dotKV*) => do
        let (srcId, srcPort) ← parseNodeRef src
        let (dstId, dstPort) ← parseNodeRef dst
        let attrList ← parseKVs kvs
        `(Subgraph.addEdge $result { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $attrList })
      | `(subgraphElem| edge $src:dotNodeRef -> $dst:dotNodeRef $kvs:dotKV*) => do
        let (srcId, srcPort) ← parseNodeRef src
        let (dstId, dstPort) ← parseNodeRef dst
        let attrList ← parseKVs kvs
        `(Subgraph.addEdge $result { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $attrList })
      | `(subgraphElem| $key:ident $val:str) =>
        `(Subgraph.withAttr $result (Attr.mk $(Lean.quote (toString key.getId)) $val))
      | _ => Lean.Macro.throwUnsupported
  pure result

macro_rules
  | `(dot { $elems* }) => do
    let mut graphExpr ← `(Graph.empty)

    for elem in elems do
      graphExpr ← match elem with
        | `(dotElem| digraph $name:str) =>
          `({ $graphExpr with name := $name, direction := .directed })
        | `(dotElem| graph $name:str) =>
          `({ $graphExpr with name := $name, direction := .undirected })
        | `(dotElem| strict digraph $name:str) =>
          `({ $graphExpr with name := $name, direction := .directed, «strict» := true })
        | `(dotElem| strict graph $name:str) =>
          `({ $graphExpr with name := $name, direction := .undirected, «strict» := true })
        | `(dotElem| rankdir $dir:str) =>
          `(Graph.withAttr $graphExpr (Attr.rankdir $dir))
        | `(dotElem| bgcolor $c:str) =>
          `(Graph.withAttr $graphExpr (Attr.bgcolor $c))
        | `(dotElem| fontname $f:str) =>
          `(Graph.withAttr $graphExpr (Attr.fontname $f))
        | `(dotElem| node_defaults $kvs:dotKV*) => do
          let attrList ← parseKVs kvs
          `(Graph.withNodeDefaults $graphExpr $attrList)
        | `(dotElem| edge_defaults $kvs:dotKV*) => do
          let attrList ← parseKVs kvs
          `(Graph.withEdgeDefaults $graphExpr $attrList)
        | `(dotElem| cluster $name:str { $sgElems* }) => do
          let sgExpr ← `(Subgraph.cluster $name)
          let sgExpr ← parseSubgraphElems sgElems sgExpr
          `(Graph.addSubgraph $graphExpr $sgExpr)
        | `(dotElem| subgraph $name:str { $sgElems* }) => do
          let sgExpr ← `(Subgraph.plain $name)
          let sgExpr ← parseSubgraphElems sgElems sgExpr
          `(Graph.addSubgraph $graphExpr $sgExpr)
        | `(dotElem| node $id:str $kvs:dotKV*) => do
          let attrList ← parseKVs kvs
          `(Graph.addNode $graphExpr { id := $id, attrs := $attrList })
        | `(dotElem| edge $src:dotNodeRef → $dst:dotNodeRef $kvs:dotKV*) => do
          let (srcId, srcPort) ← parseNodeRef src
          let (dstId, dstPort) ← parseNodeRef dst
          let attrList ← parseKVs kvs
          `(Graph.addEdge $graphExpr { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $attrList })
        | `(dotElem| edge $src:dotNodeRef -> $dst:dotNodeRef $kvs:dotKV*) => do
          let (srcId, srcPort) ← parseNodeRef src
          let (dstId, dstPort) ← parseNodeRef dst
          let attrList ← parseKVs kvs
          `(Graph.addEdge $graphExpr { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $attrList })
        -- Bidirectional edges (creates two edges in opposite directions)
        | `(dotElem| edge $src:dotNodeRef ↔ $dst:dotNodeRef $kvs:dotKV*) => do
          let (srcId, srcPort) ← parseNodeRef src
          let (dstId, dstPort) ← parseNodeRef dst
          let attrList ← parseKVs kvs
          `(Graph.addEdge (Graph.addEdge $graphExpr
              { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $attrList })
              { src := $dstId, dst := $srcId, srcPort := $dstPort, dstPort := $srcPort, attrs := $attrList })
        | `(dotElem| edge $src:dotNodeRef <-> $dst:dotNodeRef $kvs:dotKV*) => do
          let (srcId, srcPort) ← parseNodeRef src
          let (dstId, dstPort) ← parseNodeRef dst
          let attrList ← parseKVs kvs
          `(Graph.addEdge (Graph.addEdge $graphExpr
              { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $attrList })
              { src := $dstId, dst := $srcId, srcPort := $dstPort, dstPort := $srcPort, attrs := $attrList })
        -- Edge chains: chain ["A", "B", "C", "D"] creates A→B→C→D
        | `(dotElem| chain [ $nodes:str,* ] $kvs:dotKV*) => do
          let attrList ← parseKVs kvs
          let nodeArr := nodes.getElems
          if nodeArr.size < 2 then
            Lean.Macro.throwError "Edge chain requires at least 2 nodes"
          let mut result := graphExpr
          for i in [0:nodeArr.size - 1] do
            let src := nodeArr[i]!
            let dst := nodeArr[i + 1]!
            result ← `(Graph.addEdge $result { src := $src, dst := $dst, srcPort := Port.mk none none, dstPort := Port.mk none none, attrs := $attrList })
          pure result
        -- Multi-target edges: fanout "hub" → ["a", "b", "c"]
        | `(dotElem| fanout $src:dotNodeRef → [ $targets:str,* ] $kvs:dotKV*) => do
          let (srcId, srcPort) ← parseNodeRef src
          let attrList ← parseKVs kvs
          let mut result := graphExpr
          for target in targets.getElems do
            result ← `(Graph.addEdge $result { src := $srcId, dst := $target, srcPort := $srcPort, dstPort := Port.mk none none, attrs := $attrList })
          pure result
        | `(dotElem| fanout $src:dotNodeRef -> [ $targets:str,* ] $kvs:dotKV*) => do
          let (srcId, srcPort) ← parseNodeRef src
          let attrList ← parseKVs kvs
          let mut result := graphExpr
          for target in targets.getElems do
            result ← `(Graph.addEdge $result { src := $srcId, dst := $target, srcPort := $srcPort, dstPort := Port.mk none none, attrs := $attrList })
          pure result
        | `(dotElem| $key:ident $val:str) =>
          `(Graph.withAttr $graphExpr (Attr.mk $(Lean.quote (toString key.getId)) $val))
        | _ => Lean.Macro.throwUnsupported

    pure graphExpr

end Dot4

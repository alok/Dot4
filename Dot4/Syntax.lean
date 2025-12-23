import Dot4.Render
import Dot4.Elab
import Dot4.Record

/-!
# DOT DSL Syntax

Custom syntax for defining DOT graphs in Lean using the `dot { ... }` macro.
Supports nodes, edges, subgraphs, clusters, and graph attributes.
-/

namespace Dot4

/-- Key-value attribute syntax for DOT properties -/
declare_syntax_cat dotKV
/-- Attribute assignment: {lit}`key=str` -/
syntax ident "=" str : dotKV
/-- Attribute assignment: {lit}`key=ident` -/
syntax ident "=" ident : dotKV
/-- Attribute assignment: {lit}`key=num` -/
syntax ident "=" num : dotKV

/-- Port and compass point syntax for edge endpoints -/
declare_syntax_cat dotPort
/-- Port specification with colon prefix -/
syntax ":" ident : dotPort

/-- Node reference with optional port/compass specifications -/
declare_syntax_cat dotNodeRef
/-- Node reference with quoted string ID -/
syntax str : dotNodeRef
/-- Node reference with port -/
syntax str dotPort : dotNodeRef
/-- Node reference with port and compass point -/
syntax str dotPort dotPort : dotNodeRef
/-- Node reference with unquoted identifier -/
syntax ident : dotNodeRef
/-- Node reference with unquoted identifier and port -/
syntax ident dotPort : dotNodeRef
/-- Node reference with unquoted identifier, port, and compass point -/
syntax ident dotPort dotPort : dotNodeRef

/-- Graph element syntax for nodes, edges, subgraphs, and attributes -/
declare_syntax_cat dotElem

/-- Node declaration with quoted ID and attributes -/
syntax "node" str dotKV* : dotElem
/-- Node declaration with unquoted identifier and attributes -/
syntax "node" ident dotKV* : dotElem

/-- Arrow with target node for edge chains -/
declare_syntax_cat arrowNode
/-- Unicode arrow with target -/
syntax "→" dotNodeRef : arrowNode
/-- ASCII arrow with target -/
syntax "->" dotNodeRef : arrowNode

/-- Directed edge (single or chain) with Unicode/ASCII arrows -/
syntax "edge" dotNodeRef arrowNode+ dotKV* : dotElem

/-- Bidirectional edge with Unicode arrows -/
syntax "edge" dotNodeRef "↔" dotNodeRef dotKV* : dotElem
/-- Bidirectional edge with ASCII arrows -/
syntax "edge" dotNodeRef "<->" dotNodeRef dotKV* : dotElem

/-- Edge chain connecting multiple nodes in sequence -/
syntax "chain" "[" str,+ "]" dotKV* : dotElem

/-- Fan-out edge from one source to multiple targets with Unicode arrow -/
syntax "fanout" dotNodeRef "→" "[" str,* "]" dotKV* : dotElem
/-- Fan-out edge from one source to multiple targets with ASCII arrow -/
syntax "fanout" dotNodeRef "->" "[" str,* "]" dotKV* : dotElem

/-- Default attributes for all subsequent nodes -/
syntax "node_defaults" dotKV+ : dotElem
/-- Default attributes for all subsequent edges -/
syntax "edge_defaults" dotKV+ : dotElem

/-- Constraint to align nodes at the same rank -/
syntax "sameRank" "[" str,* "]" : dotElem

/-- Record field syntax for structured node labels -/
declare_syntax_cat recordField
/-- Record field with port name and label -/
syntax str ":" str : recordField
/-- Record field with label only -/
syntax str : recordField
/-- Nested row of record fields -/
syntax "{" recordField,* "}" : recordField

/-- Record-shaped node with box corners -/
syntax "record" str "[" recordField,* "]" dotKV* : dotElem
/-- Record-shaped node with rounded corners -/
syntax "mrecord" str "[" recordField,* "]" dotKV* : dotElem

/-- Subgraph element syntax for nodes, edges, and attributes within subgraphs -/
declare_syntax_cat subgraphElem
/-- Node declaration with quoted ID inside subgraph -/
syntax "node" str dotKV* : subgraphElem
/-- Node declaration with unquoted ID inside subgraph -/
syntax "node" ident dotKV* : subgraphElem
/-- Directed edge (single or chain) inside subgraph -/
syntax "edge" dotNodeRef arrowNode+ dotKV* : subgraphElem
/-- Node defaults inside subgraph -/
syntax "node_defaults" dotKV+ : subgraphElem
/-- Edge defaults inside subgraph -/
syntax "edge_defaults" dotKV+ : subgraphElem
/-- Subgraph attribute like label or style -/
syntax ident str : subgraphElem

/-- Cluster subgraph with visible boundary -/
syntax "cluster" str "{" subgraphElem* "}" : dotElem
/-- Plain subgraph without boundary -/
syntax "subgraph" str "{" subgraphElem* "}" : dotElem

/-- Directed graph declaration -/
syntax "digraph" str : dotElem
/-- Undirected graph declaration -/
syntax "graph" str : dotElem
/-- Strict directed graph (no multi-edges) -/
syntax "strict" "digraph" str : dotElem
/-- Strict undirected graph (no multi-edges) -/
syntax "strict" "graph" str : dotElem
/-- Generic graph attribute -/
syntax ident str : dotElem

/-- Main DOT graph block syntax -/
syntax "dot" "{" dotElem* "}" : term

/-- Convert key-value pairs to attribute list with validation -/
def parseKVs (kvs : Lean.TSyntaxArray `dotKV) : Lean.MacroM (Lean.TSyntax `term) := do
  let attrs ← kvs.mapM fun kv => do
    match kv with
    | `(dotKV| $key:ident = $val:str) =>
      let keyStr := toString key.getId
      let valStr := val.getString
      -- Validate the attribute, highlighting the value on error
      Dot4.validateAttrM keyStr valStr (some val.raw)
      `(Dot4.Attr.mk $(Lean.quote keyStr) $val)
    | `(dotKV| $key:ident = $val:ident) =>
      let keyStr := toString key.getId
      let valStr := toString val.getId
      Dot4.validateAttrM keyStr valStr (some val.raw)
      `(Dot4.Attr.mk $(Lean.quote keyStr) $(Lean.quote valStr))
    | `(dotKV| $key:ident = $val:num) =>
      let keyStr := toString key.getId
      let valStr := toString val.getNat
      Dot4.validateAttrM keyStr valStr (some val.raw)
      `(Dot4.Attr.mk $(Lean.quote keyStr) $(Lean.quote valStr))
    | _ => Lean.Macro.throwUnsupported
  `([ $[$attrs],* ])

/-- Result of parsing edge attributes, separating lhead/ltail from regular attrs -/
structure EdgeAttrResult where
  /-- Regular edge attributes -/
  attrs : Lean.TSyntax `term
  /-- Logical head cluster (lhead) for compound edges -/
  lhead : Option (Lean.TSyntax `term)
  /-- Logical tail cluster (ltail) for compound edges -/
  ltail : Option (Lean.TSyntax `term)

/-- Parse edge key-value pairs, extracting lhead/ltail separately -/
def parseEdgeKVs (kvs : Lean.TSyntaxArray `dotKV) : Lean.MacroM EdgeAttrResult := do
  let mut regularAttrs : Array (Lean.TSyntax `term) := #[]
  let mut lheadVal : Option (Lean.TSyntax `term) := none
  let mut ltailVal : Option (Lean.TSyntax `term) := none

  for kv in kvs do
    match kv with
    | `(dotKV| $key:ident = $val:str) =>
      let keyStr := toString key.getId
      let valStr := val.getString
      Dot4.validateAttrM keyStr valStr (some val.raw)
      if keyStr == "lhead" then
        lheadVal := some (← `(some $val))
      else if keyStr == "ltail" then
        ltailVal := some (← `(some $val))
      else
        regularAttrs := regularAttrs.push (← `(Dot4.Attr.mk $(Lean.quote keyStr) $val))
    | `(dotKV| $key:ident = $val:ident) =>
      let keyStr := toString key.getId
      let valStr := toString val.getId
      Dot4.validateAttrM keyStr valStr (some val.raw)
      if keyStr == "lhead" then
        lheadVal := some (← `(some $(Lean.quote valStr)))
      else if keyStr == "ltail" then
        ltailVal := some (← `(some $(Lean.quote valStr)))
      else
        regularAttrs := regularAttrs.push (← `(Dot4.Attr.mk $(Lean.quote keyStr) $(Lean.quote valStr)))
    | `(dotKV| $key:ident = $val:num) =>
      let keyStr := toString key.getId
      let valStr := toString val.getNat
      Dot4.validateAttrM keyStr valStr (some val.raw)
      regularAttrs := regularAttrs.push (← `(Dot4.Attr.mk $(Lean.quote keyStr) $(Lean.quote valStr)))
    | _ => Lean.Macro.throwUnsupported

  let attrList ← `([ $[$regularAttrs],* ])
  pure { attrs := attrList, lhead := lheadVal, ltail := ltailVal }

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

/-- Parse arrow node to get the target node ref -/
def parseArrowNode (an : Lean.TSyntax `arrowNode) : Lean.MacroM (Lean.TSyntax `dotNodeRef) := do
  match an with
  | `(arrowNode| → $ref:dotNodeRef) => pure ref
  | `(arrowNode| -> $ref:dotNodeRef) => pure ref
  | _ => Lean.Macro.throwUnsupported

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
  -- Unquoted identifier cases
  | `(dotNodeRef| $id:ident) =>
    let idStr := Lean.quote (toString id.getId)
    pure (← `($idStr), ← `(Port.mk none none))
  | `(dotNodeRef| $id:ident $p:dotPort) =>
    let idStr := Lean.quote (toString id.getId)
    let port ← parsePort p
    pure (← `($idStr), port)
  | `(dotNodeRef| $id:ident $p1:dotPort $p2:dotPort) =>
    let idStr := Lean.quote (toString id.getId)
    match p1, p2 with
    | `(dotPort| : $name:ident), `(dotPort| : $compassId:ident) =>
      let dir ← parseCompass compassId
      let port ← `(Port.mk' $(Lean.quote (toString name.getId)) $dir)
      pure (← `($idStr), port)
    | _, _ => pure (← `($idStr), ← `(Port.mk none none))
  | _ => Lean.Macro.throwUnsupported

/-- Parse a record field into a RecordCell term -/
partial def parseRecordField (field : Lean.TSyntax `recordField) : Lean.MacroM (Lean.TSyntax `term) := do
  match field with
  | `(recordField| $port:str : $label:str) =>
    `(RecordCell.field (some $port) $label)
  | `(recordField| $label:str) =>
    `(RecordCell.field none $label)
  | `(recordField| { $fields:recordField,* }) => do
    let fieldArr := fields.getElems
    let mut cellList ← `([])
    for f in fieldArr.reverse do
      let cell ← parseRecordField f
      cellList ← `($cell :: $cellList)
    `(RecordCell.row $cellList)
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
      | `(subgraphElem| node $id:ident $kvs:dotKV*) => do
        let idStr := Lean.quote (toString id.getId)
        let attrList ← parseKVs kvs
        `(Subgraph.addNode $result { id := $idStr, attrs := $attrList })
      | `(subgraphElem| edge $src:dotNodeRef $arrowNodes:arrowNode* $kvs:dotKV*) => do
        let edgeAttrs ← parseEdgeKVs kvs
        let lheadExpr := edgeAttrs.lhead.getD (← `(none))
        let ltailExpr := edgeAttrs.ltail.getD (← `(none))
        -- Collect all nodes
        let mut allNodes : Array (Lean.TSyntax `term × Lean.TSyntax `term) := #[]
        allNodes := allNodes.push (← parseNodeRef src)
        for an in arrowNodes do
          let targetRef ← parseArrowNode an
          allNodes := allNodes.push (← parseNodeRef targetRef)
        -- Create edges for consecutive pairs
        let mut sgResult := result
        for i in [0:allNodes.size - 1] do
          let (srcId, srcPort) := allNodes[i]!
          let (dstId, dstPort) := allNodes[i + 1]!
          sgResult ← `(Subgraph.addEdge $sgResult { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $(edgeAttrs.attrs), lhead := $lheadExpr, ltail := $ltailExpr })
        pure sgResult
      | `(subgraphElem| node_defaults $kvs:dotKV*) => do
        let attrList ← parseKVs kvs
        `(Subgraph.withNodeDefaults $result $attrList)
      | `(subgraphElem| edge_defaults $kvs:dotKV*) => do
        let attrList ← parseKVs kvs
        `(Subgraph.withEdgeDefaults $result $attrList)
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
        | `(dotElem| node $id:ident $kvs:dotKV*) => do
          let idStr := Lean.quote (toString id.getId)
          let attrList ← parseKVs kvs
          `(Graph.addNode $graphExpr { id := $idStr, attrs := $attrList })
        -- Edge chains: edge "A" -> "B" -> "C" creates A→B, B→C
        | `(dotElem| edge $src:dotNodeRef $arrowNodes:arrowNode* $kvs:dotKV*) => do
          let edgeAttrs ← parseEdgeKVs kvs
          let lheadExpr := edgeAttrs.lhead.getD (← `(none))
          let ltailExpr := edgeAttrs.ltail.getD (← `(none))
          -- Collect all nodes: src followed by targets from arrow nodes
          let mut allNodes : Array (Lean.TSyntax `term × Lean.TSyntax `term) := #[]
          allNodes := allNodes.push (← parseNodeRef src)
          for an in arrowNodes do
            let targetRef ← parseArrowNode an
            allNodes := allNodes.push (← parseNodeRef targetRef)
          -- Create edges for consecutive pairs
          let mut result := graphExpr
          for i in [0:allNodes.size - 1] do
            let (srcId, srcPort) := allNodes[i]!
            let (dstId, dstPort) := allNodes[i + 1]!
            result ← `(Graph.addEdge $result { src := $srcId, dst := $dstId, srcPort := $srcPort, dstPort := $dstPort, attrs := $(edgeAttrs.attrs), lhead := $lheadExpr, ltail := $ltailExpr })
          pure result
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
        -- Same rank constraint: sameRank ["A", "B", "C"]
        | `(dotElem| sameRank [ $nodes:str,* ]) => do
          let nodeArr := nodes.getElems
          -- Create an anonymous subgraph with rank=same containing all the nodes
          let mut sgExpr ← `(Subgraph.sameRank)
          for nodeId in nodeArr do
            sgExpr ← `(Subgraph.addNode $sgExpr (Node.new $nodeId))
          `(Graph.addSubgraph $graphExpr $sgExpr)
        -- Record node: record "id" ["port" : "label", "label2", ...]
        | `(dotElem| record $id:str [ $fields:recordField,* ] $kvs:dotKV*) => do
          let fieldArr := fields.getElems
          let mut cellList ← `([])
          for f in fieldArr.reverse do
            let cell ← parseRecordField f
            cellList ← `($cell :: $cellList)
          let attrList ← parseKVs kvs
          `(Graph.addRecordNode $graphExpr ({ id := $id, cells := $cellList, attrs := $attrList } : RecordNode))
        -- Mrecord (rounded corners): mrecord "id" [...]
        | `(dotElem| mrecord $id:str [ $fields:recordField,* ] $kvs:dotKV*) => do
          let fieldArr := fields.getElems
          let mut cellList ← `([])
          for f in fieldArr.reverse do
            let cell ← parseRecordField f
            cellList ← `($cell :: $cellList)
          let attrList ← parseKVs kvs
          `(Graph.addRecordNode $graphExpr ({ id := $id, cells := $cellList, rounded := true, attrs := $attrList } : RecordNode))
        | `(dotElem| $key:ident $val:str) =>
          `(Graph.withAttr $graphExpr (Attr.mk $(Lean.quote (toString key.getId)) $val))
        | _ => Lean.Macro.throwUnsupported

    pure graphExpr

end Dot4

import Dot4.Render
import Dot4.Elab
import Dot4.Record

/-!
# DOT DSL Syntax

Custom syntax for defining DOT graphs in Lean using the `dot { ... }` macro.
Supports nodes, edges, subgraphs, clusters, and graph attributes.
-/

namespace Dot4

/-- Value syntax for DOT attribute values (supports antiquotation with $(expr)) -/
declare_syntax_cat dotValue (behavior := symbol)
/-- String value -/
syntax str : dotValue
/-- Identifier value -/
syntax ident : dotValue
/-- Numeric value -/
syntax num : dotValue
/-- Float value -/
syntax scientific : dotValue

/-- Key-value attribute syntax for DOT properties -/
declare_syntax_cat dotKV
/-- Attribute assignment: {lit}`key=value` where value can be str, ident, num, scientific, or $(expr) -/
syntax ident "=" dotValue : dotKV

/-- Port and compass point syntax for edge endpoints -/
declare_syntax_cat dotPort
/-- Port specification with colon prefix -/
syntax ":" ident : dotPort

/-- Node ID syntax (supports antiquotation with $(expr)) -/
declare_syntax_cat dotNodeId (behavior := symbol)
/-- Node ID as quoted string -/
syntax str : dotNodeId
/-- Node ID as unquoted identifier -/
syntax ident : dotNodeId

/-- Node reference with optional port/compass specifications (supports antiquotation) -/
declare_syntax_cat dotNodeRef (behavior := symbol)
/-- Node reference -/
syntax dotNodeId : dotNodeRef
/-- Node reference with port -/
syntax dotNodeId dotPort : dotNodeRef
/-- Node reference with port and compass point -/
syntax dotNodeId dotPort dotPort : dotNodeRef

/-- Graph element syntax for nodes, edges, subgraphs, and attributes -/
declare_syntax_cat dotElem

/-- Node declaration with ID and attributes -/
syntax "node" dotNodeId dotKV* : dotElem

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
/-- Node declaration inside subgraph -/
syntax "node" dotNodeId dotKV* : subgraphElem
/-- Directed edge (single or chain) inside subgraph -/
syntax "edge" dotNodeRef arrowNode+ dotKV* : subgraphElem
/-- Node defaults inside subgraph -/
syntax "node_defaults" dotKV+ : subgraphElem
/-- Edge defaults inside subgraph -/
syntax "edge_defaults" dotKV+ : subgraphElem
/-- Subgraph attribute like label or style -/
syntax ident str : subgraphElem

/-- Cluster subgraph with visible boundary -/
syntax "cluster" dotNodeId "{" subgraphElem* "}" : dotElem
/-- Plain subgraph without boundary -/
syntax "subgraph" dotNodeId "{" subgraphElem* "}" : dotElem

/-- Directed graph declaration -/
syntax "digraph" dotNodeId : dotElem
/-- Undirected graph declaration -/
syntax "graph" dotNodeId : dotElem
/-- Strict directed graph (no multi-edges) -/
syntax "strict" "digraph" str : dotElem
/-- Strict undirected graph (no multi-edges) -/
syntax "strict" "graph" str : dotElem
/-- Generic graph attribute -/
syntax ident str : dotElem

/-- Main DOT graph block syntax -/
syntax "dot" "{" dotElem* "}" : term

/-- Parse a dotValue to get the value expression and optional literal for validation -/
def parseDotValue (v : Lean.TSyntax `dotValue) : Lean.MacroM (Lean.TSyntax `term × Option String) := do
  if v.raw.isAntiquot then
    -- Interpolated value: $(expr)
    let term := ⟨v.raw.getAntiquotTerm⟩
    pure (term, none)
  else
    match v with
    | `(dotValue| $val:str) =>
      pure (val, some val.getString)
    | `(dotValue| $val:ident) =>
      let valStr := toString val.getId
      pure (← `($(Lean.quote valStr)), some valStr)
    | `(dotValue| $val:num) =>
      let valStr := toString val.getNat
      pure (← `($(Lean.quote valStr)), some valStr)
    | `(dotValue| $val:scientific) =>
      let valStr := val.raw.getArgs[0]!.getAtomVal
      pure (← `($(Lean.quote valStr)), some valStr)
    | _ => Lean.Macro.throwUnsupported

/-- Convert key-value pairs to attribute list with validation -/
def parseKVs (kvs : Lean.TSyntaxArray `dotKV) : Lean.MacroM (Lean.TSyntax `term) := do
  let attrs ← kvs.mapM fun kv => do
    match kv with
    | `(dotKV| $key:ident = $val:dotValue) =>
      let keyStr := toString key.getId
      let (valExpr, valStrOpt) ← parseDotValue val
      -- Validate the attribute if we have a literal value
      if let some valStr := valStrOpt then
        Dot4.validateAttrM keyStr valStr (some val.raw)
      `(Dot4.Attr.mk $(Lean.quote keyStr) $valExpr)
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
    | `(dotKV| $key:ident = $val:dotValue) =>
      let keyStr := toString key.getId
      let (valExpr, valStrOpt) ← parseDotValue val
      -- Validate if literal
      if let some valStr := valStrOpt then
        Dot4.validateAttrM keyStr valStr (some val.raw)
      -- Check for lhead/ltail
      if keyStr == "lhead" then
        lheadVal := some (← `(some $valExpr))
      else if keyStr == "ltail" then
        ltailVal := some (← `(some $valExpr))
      else
        regularAttrs := regularAttrs.push (← `(Dot4.Attr.mk $(Lean.quote keyStr) $valExpr))
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

/-- Parse a dotNodeId to get the ID expression -/
def parseDotNodeId (nodeId : Lean.TSyntax `dotNodeId) : Lean.MacroM (Lean.TSyntax `term) := do
  if nodeId.raw.isAntiquot then
    -- Interpolated: $(expr)
    pure ⟨nodeId.raw.getAntiquotTerm⟩
  else
    match nodeId with
    | `(dotNodeId| $id:str) => pure id
    | `(dotNodeId| $id:ident) => `($(Lean.quote (toString id.getId)))
    | _ => Lean.Macro.throwUnsupported

/-- Parse node reference to get (id, port) -/
def parseNodeRef (ref : Lean.TSyntax `dotNodeRef) : Lean.MacroM (Lean.TSyntax `term × Lean.TSyntax `term) := do
  -- Check if the entire nodeRef is an antiquotation
  if ref.raw.isAntiquot then
    let term := ⟨ref.raw.getAntiquotTerm⟩
    pure (term, ← `(Port.mk none none))
  else
    match ref with
    | `(dotNodeRef| $id:dotNodeId) =>
      let idExpr ← parseDotNodeId id
      pure (idExpr, ← `(Port.mk none none))
    | `(dotNodeRef| $id:dotNodeId $p:dotPort) =>
      let idExpr ← parseDotNodeId id
      let port ← parsePort p
      pure (idExpr, port)
    | `(dotNodeRef| $id:dotNodeId $p1:dotPort $p2:dotPort) =>
      let idExpr ← parseDotNodeId id
      match p1, p2 with
      | `(dotPort| : $name:ident), `(dotPort| : $compassId:ident) =>
        let dir ← parseCompass compassId
        let port ← `(Port.mk' $(Lean.quote (toString name.getId)) $dir)
        pure (idExpr, port)
      | _, _ => pure (idExpr, ← `(Port.mk none none))
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
      | `(subgraphElem| node $id:dotNodeId $kvs:dotKV*) => do
        let idExpr ← parseDotNodeId id
        let attrList ← parseKVs kvs
        `(Subgraph.addNode $result { id := $idExpr, attrs := $attrList })
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
        | `(dotElem| digraph $name:dotNodeId) => do
          let nameExpr ← parseDotNodeId name
          `({ $graphExpr with name := $nameExpr, direction := .directed })
        | `(dotElem| graph $name:dotNodeId) => do
          let nameExpr ← parseDotNodeId name
          `({ $graphExpr with name := $nameExpr, direction := .undirected })
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
        | `(dotElem| cluster $name:dotNodeId { $sgElems* }) => do
          let nameExpr ← parseDotNodeId name
          let sgExpr ← `(Subgraph.cluster $nameExpr)
          let sgExpr ← parseSubgraphElems sgElems sgExpr
          `(Graph.addSubgraph $graphExpr $sgExpr)
        | `(dotElem| subgraph $name:dotNodeId { $sgElems* }) => do
          let nameExpr ← parseDotNodeId name
          let sgExpr ← `(Subgraph.plain $nameExpr)
          let sgExpr ← parseSubgraphElems sgElems sgExpr
          `(Graph.addSubgraph $graphExpr $sgExpr)
        | `(dotElem| node $id:dotNodeId $kvs:dotKV*) => do
          let idExpr ← parseDotNodeId id
          let attrList ← parseKVs kvs
          `(Graph.addNode $graphExpr { id := $idExpr, attrs := $attrList })
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

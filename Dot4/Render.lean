import Dot4.Basic

/-!
# DOT Code Generation

Converts Graph structures to Graphviz DOT format strings.
-/

namespace Dot4

/-- Quote character -/
def quote : String := "\""

/-- Escape a string for DOT format -/
def escapeString (s : String) : String :=
  s.replace "\\" "\\\\"
   |>.replace "\"" "\\\""
   |>.replace "\n" "\\n"

/-- Wrap in quotes with escaping -/
def quoted (s : String) : String :=
  quote ++ escapeString s ++ quote

/-- Render attributes list: [key="value", ...] -/
def renderAttrs (attrs : List Attr) : String :=
  if attrs.isEmpty then ""
  else
    let pairs := attrs.map fun a => a.key ++ "=" ++ quoted a.value
    " [" ++ ", ".intercalate pairs ++ "]"

/-- Render node definition with optional indent -/
def renderNode (n : Node) (indent : String := "    ") : String :=
  let labelAttr := match n.label with
    | some l => [Attr.mk "label" l]
    | none => []
  let allAttrs := labelAttr ++ n.attrs
  indent ++ quoted n.id ++ renderAttrs allAttrs ++ ";"

/-- Render port specification (e.g., :port:ne) -/
def renderPort (p : Port) : String :=
  match p.name, p.compass with
  | none, none => ""
  | some n, none => ":" ++ n
  | none, some dir => ":" ++ dir.toString
  | some n, some dir => ":" ++ n ++ ":" ++ dir.toString

/-- Render edge definition with optional indent -/
def renderEdge (e : Edge) (directed : Bool) (indent : String := "    ") : String :=
  let arrow := if directed then " -> " else " -- "
  let labelAttr := match e.label with
    | some l => [Attr.mk "label" l]
    | none => []
  -- Add lhead/ltail for cluster edge routing
  let lheadAttr := match e.lhead with
    | some h => [Attr.mk "lhead" h]
    | none => []
  let ltailAttr := match e.ltail with
    | some t => [Attr.mk "ltail" t]
    | none => []
  let allAttrs := labelAttr ++ lheadAttr ++ ltailAttr ++ e.attrs
  let srcStr := quoted e.src ++ renderPort e.srcPort
  let dstStr := quoted e.dst ++ renderPort e.dstPort
  indent ++ srcStr ++ arrow ++ dstStr ++ renderAttrs allAttrs ++ ";"

/-- Render a subgraph -/
def renderSubgraph (sg : Subgraph) (directed : Bool) : Array String :=
  let lines := #["    subgraph " ++ quoted sg.name ++ " {"]
  -- Subgraph attributes
  let lines := sg.attrs.foldl (fun acc a =>
    acc.push ("        " ++ a.key ++ "=" ++ quoted a.value ++ ";")) lines
  -- Nodes
  let lines := sg.nodes.foldl (fun acc n =>
    acc.push (renderNode n "        ")) lines
  -- Edges
  let lines := sg.edges.foldl (fun acc e =>
    acc.push (renderEdge e directed "        ")) lines
  lines.push "    }"

/-- Render graph to DOT format -/
def Graph.toDot (g : Graph) : String :=
  let strictPrefix := if g.strict then "strict " else ""
  let keyword := match g.direction with
    | .directed => "digraph"
    | .undirected => "graph"

  let lines := #[strictPrefix ++ keyword ++ " " ++ quoted g.name ++ " {"]

  -- Graph attributes
  let lines := g.attrs.foldl (fun acc a =>
    acc.push ("    " ++ a.key ++ "=" ++ quoted a.value ++ ";")) lines

  -- Node defaults
  let lines := if g.nodeDefaults.isEmpty then lines
    else lines.push ("    node" ++ renderAttrs g.nodeDefaults ++ ";")

  -- Edge defaults
  let lines := if g.edgeDefaults.isEmpty then lines
    else lines.push ("    edge" ++ renderAttrs g.edgeDefaults ++ ";")

  -- Blank line if we had any defaults/attrs
  let lines := if g.attrs.isEmpty && g.nodeDefaults.isEmpty && g.edgeDefaults.isEmpty
    then lines else lines.push ""

  -- Subgraphs
  let lines := g.subgraphs.foldl (fun acc sg =>
    let sgLines := renderSubgraph sg (g.direction == .directed)
    acc ++ sgLines) lines

  -- Blank line before nodes if we had subgraphs
  let lines := if g.subgraphs.isEmpty then lines else lines.push ""

  -- Nodes
  let lines := g.nodes.foldl (fun acc n => acc.push (renderNode n)) lines

  -- Edges
  let lines := g.edges.foldl (fun acc e =>
    acc.push (renderEdge e (g.direction == .directed))) lines

  let lines := lines.push "}"

  "\n".intercalate lines.toList

end Dot4

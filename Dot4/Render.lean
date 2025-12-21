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

/-- Render node definition -/
def renderNode (n : Node) : String :=
  let labelAttr := match n.label with
    | some l => [Attr.mk "label" l]
    | none => []
  let allAttrs := labelAttr ++ n.attrs
  "    " ++ n.id ++ renderAttrs allAttrs ++ ";"

/-- Render edge definition -/
def renderEdge (e : Edge) (directed : Bool) : String :=
  let arrow := if directed then " -> " else " -- "
  let labelAttr := match e.label with
    | some l => [Attr.mk "label" l]
    | none => []
  let allAttrs := labelAttr ++ e.attrs
  "    " ++ e.src ++ arrow ++ e.dst ++ renderAttrs allAttrs ++ ";"

/-- Render graph to DOT format -/
def Graph.toDot (g : Graph) : String :=
  let keyword := match g.direction with
    | .directed => "digraph"
    | .undirected => "graph"

  let lines := #[keyword ++ " " ++ g.name ++ " {"]

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

  -- Nodes
  let lines := g.nodes.foldl (fun acc n => acc.push (renderNode n)) lines

  -- Edges
  let lines := g.edges.foldl (fun acc e =>
    acc.push (renderEdge e (g.direction == .directed))) lines

  let lines := lines.push "}"

  "\n".intercalate lines.toList

end Dot4

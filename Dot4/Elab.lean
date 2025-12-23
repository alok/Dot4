import Dot4.Basic
import Dot4.Colors
import Dot4.Shapes
import Lean

/-!
# Custom Elaborator for Dot4 DSL

Provides compile-time validation of DOT attribute values while preserving
string syntax for copy-paste compatibility with Graphviz DOT files.

The elaborator parses string values and validates them against known
Graphviz attribute types, providing helpful error messages for typos.
-/

namespace Dot4

open Lean Elab Term Meta

/-- Validate and optionally transform an attribute key-value pair.
    Returns the validated Attr or throws an error for invalid values. -/
def validateAttr (key : String) (value : String) : Except String Attr :=
  match key with
  -- Shape validation
  | "shape" =>
    match Shape.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid shape '{value}'. Valid shapes: box, circle, diamond, ellipse, hexagon, octagon, pentagon, polygon, record, star, triangle, ..."

  -- Color validation (for color attributes)
  | "color" | "fillcolor" | "fontcolor" | "bgcolor" | "pencolor" =>
    if Color.isValid value then
      .ok (Attr.mk key value)
    else
      .error s!"Invalid color '{value}'. Use: hex (#RRGGBB), named color (red, blue, ...), or rgb(r,g,b)"

  -- Rankdir validation
  | "rankdir" =>
    match RankDir.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid rankdir '{value}'. Valid values: TB, BT, LR, RL"

  -- Rank type validation
  | "rank" =>
    match RankType.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid rank '{value}'. Valid values: same, min, source, max, sink"

  -- Arrow shape validation
  | "arrowhead" | "arrowtail" =>
    match ArrowShape.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid arrow shape '{value}'. Valid: normal, dot, diamond, vee, crow, box, ..."

  -- Edge direction validation
  | "dir" =>
    match EdgeDir.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid dir '{value}'. Valid values: forward, back, both, none"

  -- Splines validation
  | "splines" =>
    match SplineType.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid splines '{value}'. Valid: none, line, polyline, curved, ortho, spline"

  -- Layout engine validation
  | "layout" =>
    match LayoutEngine.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid layout '{value}'. Valid: dot, neato, twopi, circo, fdp, sfdp, osage, patchwork"

  -- Overlap mode validation
  | "overlap" =>
    match OverlapMode.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid overlap '{value}'. Valid: true, false, scale, ortho, compress, ..."

  -- Label location validation
  | "labelloc" =>
    match LabelLoc.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid labelloc '{value}'. Valid: t (top), c (center), b (bottom)"

  -- Label justification validation
  | "labeljust" =>
    match LabelJust.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid labeljust '{value}'. Valid: l (left), c (center), r (right)"

  -- Output order validation
  | "outputorder" =>
    match OutputMode.fromString? value with
    | some _ => .ok (Attr.mk key value)
    | none => .error s!"Invalid outputorder '{value}'. Valid: breadthfirst, nodesfirst, edgesfirst"

  -- Style validation (comma-separated)
  | "style" =>
    let styles := value.splitOn ","
    let valid := styles.all fun s =>
      let st := s.trim
      (NodeStyle.fromString? st).isSome ||
      (EdgeStyle.fromString? st).isSome ||
      st == "radial"  -- gradient style
    if valid then
      .ok (Attr.mk key value)
    else
      .error s!"Invalid style in '{value}'. Valid node styles: solid, dashed, dotted, bold, rounded, filled, striped, wedged, invis. Valid edge styles: solid, dashed, dotted, bold, invis, tapered"

  -- Compound edge cluster references
  | "lhead" | "ltail" =>
    if value.startsWith "cluster_" || value.startsWith "cluster" then
      .ok (Attr.mk key value)
    else
      .error s!"Invalid {key} '{value}'. Cluster names typically start with 'cluster_' (e.g., 'cluster_backend')"

  -- Unknown attributes pass through (Graphviz-compatible)
  | _ => .ok (Attr.mk key value)

/-- Check if an attribute value is valid for a given key -/
def isValidAttr (key value : String) : Bool :=
  match validateAttr key value with
  | .ok _ => true
  | .error _ => false

/-- Get validation error message, if any -/
def getAttrError (key value : String) : Option String :=
  match validateAttr key value with
  | .ok _ => none
  | .error msg => some msg

/-- Validate attributes at macro expansion time.
    This is called from Syntax.lean to check attributes in the DSL.
    The optional stx parameter allows error spans to highlight just the value. -/
def validateAttrM (key value : String) (stx? : Option Lean.Syntax := none) : Lean.MacroM Unit := do
  match validateAttr key value with
  | .ok _ => pure ()
  | .error msg =>
    match stx? with
    | some stx => Lean.Macro.throwErrorAt stx msg
    | none => Lean.Macro.throwError msg

/-- Parse and validate a style string, returning list of valid styles -/
def parseStyles (s : String) : List String :=
  s.splitOn "," |>.map String.trim |>.filter (· != "")

/-- Check if all styles in a comma-separated string are valid -/
def areStylesValid (s : String) : Bool :=
  let styles := parseStyles s
  styles.all fun st =>
    (NodeStyle.fromString? st).isSome ||
    (EdgeStyle.fromString? st).isSome ||
    st == "radial"

end Dot4

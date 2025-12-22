import Dot4.Basic

/-!
# Record Node DSL

Type-safe construction of Graphviz record nodes with fields and ports.

## Usage

```lean
open Dot4 in
-- Simple record with fields
def myRecord := record "struct" [
  .field "f0" "left",
  .field "f1" "middle",
  .field "f2" "right"
]

-- Nested record with rows
def nestedRecord := record "table" [
  .row [.field "a" "top", .field "b" "bottom"],
  .row [.field "c" "left", .field "d" "right"]
]
```

Generates DOT:
```dot
struct [shape=record, label="<f0> left|<f1> middle|<f2> right"]
table [shape=record, label="{<a> top|<b> bottom}|{<c> left|<d> right}"]
```
-/

namespace Dot4

/-- A field or row in a record node -/
inductive RecordCell where
  /-- A single field with optional port name and label -/
  | field (port : Option String) (label : String)
  /-- A nested row of cells (renders with {} in DOT) -/
  | row (cells : List RecordCell)
  deriving Repr, BEq

namespace RecordCell

/-- Create a field with both port and label -/
def field' (port : String) (label : String) : RecordCell :=
  .field (some port) label

/-- Create a field with just a label (no port) -/
def label (text : String) : RecordCell :=
  .field none text

/-- Escape special characters in record labels -/
def escapeLabel (s : String) : String :=
  s.replace "\\" "\\\\"
   |>.replace "|" "\\|"
   |>.replace "{" "\\{"
   |>.replace "}" "\\}"
   |>.replace "<" "\\<"
   |>.replace ">" "\\>"
   |>.replace " " "\\ "

/-- Render a cell to DOT record label syntax -/
partial def toLabel : RecordCell → String
  | .field none label => escapeLabel label
  | .field (some port) label => "<" ++ port ++ "> " ++ escapeLabel label
  | .row cells =>
    let inner := "|".intercalate (cells.map toLabel)
    "{" ++ inner ++ "}"

end RecordCell

/-- A complete record node -/
structure RecordNode where
  /-- Node ID -/
  id : String
  /-- The cells (fields and rows) in this record -/
  cells : List RecordCell
  /-- Use Mrecord (rounded corners) instead of record -/
  rounded : Bool := false
  /-- Additional attributes -/
  attrs : List Attr := []
  deriving Repr

namespace RecordNode

/-- Create a simple record node -/
def simple (id : String) (cells : List RecordCell) : RecordNode :=
  { id, cells }

/-- Create a rounded record node (Mrecord) -/
def withRounded (id : String) (cells : List RecordCell) : RecordNode :=
  { id, cells, rounded := true }

/-- Add an attribute to the record node -/
def withAttr (r : RecordNode) (a : Attr) : RecordNode :=
  { r with attrs := r.attrs ++ [a] }

/-- Generate the label string for this record -/
def toLabel (r : RecordNode) : String :=
  "|".intercalate (r.cells.map RecordCell.toLabel)

/-- Convert to a regular Node for inclusion in a Graph -/
def toNode (r : RecordNode) : Node :=
  let shapeAttr := Attr.mk "shape" (if r.rounded then "Mrecord" else "record")
  let labelAttr := Attr.mk "label" r.toLabel
  { id := r.id, attrs := [shapeAttr, labelAttr] ++ r.attrs }

/-- Get all port names defined in this record -/
partial def ports (r : RecordNode) : List String :=
  let rec cellPorts : RecordCell → List String
    | .field (some port) _ => [port]
    | .field none _ => []
    | .row cells => (cells.map cellPorts).flatten
  (r.cells.map cellPorts).flatten

/-- Check if a port exists in this record -/
def hasPort (r : RecordNode) (port : String) : Bool :=
  r.ports.contains port

end RecordNode

/-- Convenience function to create a record node -/
def mkRecord (id : String) (cells : List RecordCell) : RecordNode :=
  RecordNode.simple id cells

/-- Convenience function to create a rounded record node -/
def mkMrecord (id : String) (cells : List RecordCell) : RecordNode :=
  RecordNode.withRounded id cells

/-- Add a record node to a graph -/
def Graph.addRecordNode (g : Graph) (r : RecordNode) : Graph :=
  g.addNode r.toNode

end Dot4

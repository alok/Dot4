import Dot4.Basic

/-!
# Type-Safe Shapes for DOT

All valid Graphviz node shapes as an inductive type.
No more typos like `shape="circl"` - the compiler catches it!
-/

namespace Dot4

/-- All valid Graphviz node shapes -/
inductive Shape where
  -- Polygon-based shapes
  | box
  | polygon
  | ellipse
  | oval
  | circle
  | point
  | egg
  | triangle
  | plaintext
  | plain
  | diamond
  | trapezium
  | parallelogram
  | house
  | pentagon
  | hexagon
  | septagon
  | octagon
  | doublecircle
  | doubleoctagon
  | tripleoctagon
  | invtriangle
  | invtrapezium
  | invhouse
  | Mdiamond
  | Msquare
  | Mcircle
  -- Record shapes
  | record
  | Mrecord
  -- Special shapes
  | rect
  | rectangle
  | square
  | star
  | none
  | underline
  | cylinder
  | note
  | tab
  | folder
  | box3d
  | component
  | promoter
  | cds
  | terminator
  | utr
  | primersite
  | restrictionsite
  | fivepoverhang
  | threepoverhang
  | noverhang
  | assembly
  | signature
  | insulator
  | ribosite
  | rnastab
  | proteasesite
  | proteinstab
  | rpromoter
  | rarrow
  | larrow
  | lpromoter
  deriving Repr, BEq, Hashable

namespace Shape

def toString : Shape → String
  | box => "box"
  | polygon => "polygon"
  | ellipse => "ellipse"
  | oval => "oval"
  | circle => "circle"
  | point => "point"
  | egg => "egg"
  | triangle => "triangle"
  | plaintext => "plaintext"
  | plain => "plain"
  | diamond => "diamond"
  | trapezium => "trapezium"
  | parallelogram => "parallelogram"
  | house => "house"
  | pentagon => "pentagon"
  | hexagon => "hexagon"
  | septagon => "septagon"
  | octagon => "octagon"
  | doublecircle => "doublecircle"
  | doubleoctagon => "doubleoctagon"
  | tripleoctagon => "tripleoctagon"
  | invtriangle => "invtriangle"
  | invtrapezium => "invtrapezium"
  | invhouse => "invhouse"
  | Mdiamond => "Mdiamond"
  | Msquare => "Msquare"
  | Mcircle => "Mcircle"
  | record => "record"
  | Mrecord => "Mrecord"
  | rect => "rect"
  | rectangle => "rectangle"
  | square => "square"
  | star => "star"
  | none => "none"
  | underline => "underline"
  | cylinder => "cylinder"
  | note => "note"
  | tab => "tab"
  | folder => "folder"
  | box3d => "box3d"
  | component => "component"
  | promoter => "promoter"
  | cds => "cds"
  | terminator => "terminator"
  | utr => "utr"
  | primersite => "primersite"
  | restrictionsite => "restrictionsite"
  | fivepoverhang => "fivepoverhang"
  | threepoverhang => "threepoverhang"
  | noverhang => "noverhang"
  | assembly => "assembly"
  | signature => "signature"
  | insulator => "insulator"
  | ribosite => "ribosite"
  | rnastab => "rnastab"
  | proteasesite => "proteasesite"
  | proteinstab => "proteinstab"
  | rpromoter => "rpromoter"
  | rarrow => "rarrow"
  | larrow => "larrow"
  | lpromoter => "lpromoter"

end Shape

/-- Arrow head/tail shapes -/
inductive ArrowShape where
  | box
  | crow
  | curve
  | icurve
  | diamond
  | dot
  | inv
  | none
  | normal
  | tee
  | vee
  | odot
  | invdot
  | invodot
  | obox
  | odiamond
  deriving Repr, BEq

namespace ArrowShape

def toString : ArrowShape → String
  | box => "box"
  | crow => "crow"
  | curve => "curve"
  | icurve => "icurve"
  | diamond => "diamond"
  | dot => "dot"
  | inv => "inv"
  | none => "none"
  | normal => "normal"
  | tee => "tee"
  | vee => "vee"
  | odot => "odot"
  | invdot => "invdot"
  | invodot => "invodot"
  | obox => "obox"
  | odiamond => "odiamond"

end ArrowShape

/-- Edge direction for arrow display -/
inductive EdgeDir where
  | forward
  | back
  | both
  | none
  deriving Repr, BEq

namespace EdgeDir
def toString : EdgeDir → String
  | forward => "forward"
  | back => "back"
  | both => "both"
  | none => "none"
end EdgeDir

/-- Rank direction for graph layout -/
inductive RankDir where
  | TB  -- top to bottom
  | BT  -- bottom to top
  | LR  -- left to right
  | RL  -- right to left
  deriving Repr, BEq

namespace RankDir
def toString : RankDir → String
  | TB => "TB"
  | BT => "BT"
  | LR => "LR"
  | RL => "RL"
end RankDir

/-- Rank type for subgraph ranking -/
inductive RankType where
  | same
  | min
  | source
  | max
  | sink
  deriving Repr, BEq

namespace RankType
def toString : RankType → String
  | same => "same"
  | min => "min"
  | source => "source"
  | max => "max"
  | sink => "sink"
end RankType

/-- Node style -/
inductive NodeStyle where
  | solid
  | dashed
  | dotted
  | bold
  | rounded
  | diagonals
  | filled
  | striped
  | wedged
  | invis
  deriving Repr, BEq

namespace NodeStyle
def toString : NodeStyle → String
  | solid => "solid"
  | dashed => "dashed"
  | dotted => "dotted"
  | bold => "bold"
  | rounded => "rounded"
  | diagonals => "diagonals"
  | filled => "filled"
  | striped => "striped"
  | wedged => "wedged"
  | invis => "invis"

def combine (styles : List NodeStyle) : String :=
  ",".intercalate (styles.map toString)
end NodeStyle

/-- Edge style -/
inductive EdgeStyle where
  | solid
  | dashed
  | dotted
  | bold
  | invis
  | tapered
  deriving Repr, BEq

namespace EdgeStyle
def toString : EdgeStyle → String
  | solid => "solid"
  | dashed => "dashed"
  | dotted => "dotted"
  | bold => "bold"
  | invis => "invis"
  | tapered => "tapered"
end EdgeStyle

/-- Spline type for edge routing -/
inductive SplineType where
  | none
  | line
  | polyline
  | curved
  | ortho
  | spline
  deriving Repr, BEq

namespace SplineType
def toString : SplineType → String
  | none => "none"
  | line => "line"
  | polyline => "polyline"
  | curved => "curved"
  | ortho => "ortho"
  | spline => "spline"
end SplineType

/-- Output order for rendering -/
inductive OutputMode where
  | breadthfirst
  | nodesfirst
  | edgesfirst
  deriving Repr, BEq

namespace OutputMode
def toString : OutputMode → String
  | breadthfirst => "breadthfirst"
  | nodesfirst => "nodesfirst"
  | edgesfirst => "edgesfirst"
end OutputMode

-- Type-safe attribute constructors
namespace Attr

def shapeT (s : Shape) : Attr := Attr.mk "shape" s.toString
def arrowheadT (a : ArrowShape) : Attr := Attr.mk "arrowhead" a.toString
def arrowtailT (a : ArrowShape) : Attr := Attr.mk "arrowtail" a.toString
def dirT (d : EdgeDir) : Attr := Attr.mk "dir" d.toString
def rankdirT (r : RankDir) : Attr := Attr.mk "rankdir" r.toString
def rankT (r : RankType) : Attr := Attr.mk "rank" r.toString
def nodeStyleT (s : NodeStyle) : Attr := Attr.mk "style" s.toString
def nodeStylesT (ss : List NodeStyle) : Attr := Attr.mk "style" (NodeStyle.combine ss)
def edgeStyleT (s : EdgeStyle) : Attr := Attr.mk "style" s.toString
def splinesT (s : SplineType) : Attr := Attr.mk "splines" s.toString
def outputorderT (o : OutputMode) : Attr := Attr.mk "outputorder" o.toString

end Attr

end Dot4

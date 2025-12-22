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

/-- Parse a string to a Shape, returning none if invalid -/
def fromString? (s : String) : Option Shape :=
  match s with
  | "box" => some box
  | "polygon" => some polygon
  | "ellipse" => some ellipse
  | "oval" => some oval
  | "circle" => some circle
  | "point" => some point
  | "egg" => some egg
  | "triangle" => some triangle
  | "plaintext" => some plaintext
  | "plain" => some plain
  | "diamond" => some diamond
  | "trapezium" => some trapezium
  | "parallelogram" => some parallelogram
  | "house" => some house
  | "pentagon" => some pentagon
  | "hexagon" => some hexagon
  | "septagon" => some septagon
  | "octagon" => some octagon
  | "doublecircle" => some doublecircle
  | "doubleoctagon" => some doubleoctagon
  | "tripleoctagon" => some tripleoctagon
  | "invtriangle" => some invtriangle
  | "invtrapezium" => some invtrapezium
  | "invhouse" => some invhouse
  | "Mdiamond" => some Mdiamond
  | "Msquare" => some Msquare
  | "Mcircle" => some Mcircle
  | "record" => some record
  | "Mrecord" => some Mrecord
  | "rect" => some rect
  | "rectangle" => some rectangle
  | "square" => some square
  | "star" => some star
  | "none" => some none
  | "underline" => some underline
  | "cylinder" => some cylinder
  | "note" => some note
  | "tab" => some tab
  | "folder" => some folder
  | "box3d" => some box3d
  | "component" => some component
  | "promoter" => some promoter
  | "cds" => some cds
  | "terminator" => some terminator
  | "utr" => some utr
  | "primersite" => some primersite
  | "restrictionsite" => some restrictionsite
  | "fivepoverhang" => some fivepoverhang
  | "threepoverhang" => some threepoverhang
  | "noverhang" => some noverhang
  | "assembly" => some assembly
  | "signature" => some signature
  | "insulator" => some insulator
  | "ribosite" => some ribosite
  | "rnastab" => some rnastab
  | "proteasesite" => some proteasesite
  | "proteinstab" => some proteinstab
  | "rpromoter" => some rpromoter
  | "rarrow" => some rarrow
  | "larrow" => some larrow
  | "lpromoter" => some lpromoter
  | _ => Option.none

/-- List of all valid shape names for error messages -/
def allNames : List String := [
  "box", "polygon", "ellipse", "oval", "circle", "point", "egg", "triangle",
  "plaintext", "plain", "diamond", "trapezium", "parallelogram", "house",
  "pentagon", "hexagon", "septagon", "octagon", "doublecircle", "doubleoctagon",
  "tripleoctagon", "invtriangle", "invtrapezium", "invhouse", "Mdiamond",
  "Msquare", "Mcircle", "record", "Mrecord", "rect", "rectangle", "square",
  "star", "none", "underline", "cylinder", "note", "tab", "folder", "box3d",
  "component", "promoter", "cds", "terminator", "utr", "primersite",
  "restrictionsite", "fivepoverhang", "threepoverhang", "noverhang", "assembly",
  "signature", "insulator", "ribosite", "rnastab", "proteasesite", "proteinstab",
  "rpromoter", "rarrow", "larrow", "lpromoter"
]

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

def fromString? (s : String) : Option ArrowShape :=
  match s with
  | "box" => some box
  | "crow" => some crow
  | "curve" => some curve
  | "icurve" => some icurve
  | "diamond" => some diamond
  | "dot" => some dot
  | "inv" => some inv
  | "none" => some none
  | "normal" => some normal
  | "tee" => some tee
  | "vee" => some vee
  | "odot" => some odot
  | "invdot" => some invdot
  | "invodot" => some invodot
  | "obox" => some obox
  | "odiamond" => some odiamond
  | _ => Option.none

def allNames : List String := [
  "box", "crow", "curve", "icurve", "diamond", "dot", "inv", "none",
  "normal", "tee", "vee", "odot", "invdot", "invodot", "obox", "odiamond"
]

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

def fromString? (s : String) : Option EdgeDir :=
  match s with
  | "forward" => some forward
  | "back" => some back
  | "both" => some both
  | "none" => some none
  | _ => Option.none

def allNames : List String := ["forward", "back", "both", "none"]
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

def fromString? (s : String) : Option RankDir :=
  match s with
  | "TB" => some TB
  | "BT" => some BT
  | "LR" => some LR
  | "RL" => some RL
  | _ => Option.none

def allNames : List String := ["TB", "BT", "LR", "RL"]
end RankDir

-- RankType is defined in Basic.lean (used by Subgraph)

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

def fromString? (s : String) : Option NodeStyle :=
  match s with
  | "solid" => some solid
  | "dashed" => some dashed
  | "dotted" => some dotted
  | "bold" => some bold
  | "rounded" => some rounded
  | "diagonals" => some diagonals
  | "filled" => some filled
  | "striped" => some striped
  | "wedged" => some wedged
  | "invis" => some invis
  | _ => Option.none

def allNames : List String := [
  "solid", "dashed", "dotted", "bold", "rounded",
  "diagonals", "filled", "striped", "wedged", "invis"
]

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

def fromString? (s : String) : Option EdgeStyle :=
  match s with
  | "solid" => some solid
  | "dashed" => some dashed
  | "dotted" => some dotted
  | "bold" => some bold
  | "invis" => some invis
  | "tapered" => some tapered
  | _ => Option.none

def allNames : List String := ["solid", "dashed", "dotted", "bold", "invis", "tapered"]

def combine (styles : List EdgeStyle) : String :=
  ",".intercalate (styles.map toString)
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

def fromString? (s : String) : Option SplineType :=
  match s with
  | "none" => some none
  | "line" => some line
  | "polyline" => some polyline
  | "curved" => some curved
  | "ortho" => some ortho
  | "spline" => some spline
  | _ => Option.none

def allNames : List String := ["none", "line", "polyline", "curved", "ortho", "spline"]
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

def fromString? (s : String) : Option OutputMode :=
  match s with
  | "breadthfirst" => some breadthfirst
  | "nodesfirst" => some nodesfirst
  | "edgesfirst" => some edgesfirst
  | _ => Option.none

def allNames : List String := ["breadthfirst", "nodesfirst", "edgesfirst"]
end OutputMode

/-! ## New Enums for Complete Graphviz Coverage -/

/-- Layout engine selection -/
inductive LayoutEngine where
  | dot      -- hierarchical/directed graphs
  | neato    -- spring model, undirected
  | twopi    -- radial layout
  | circo    -- circular layout
  | fdp      -- force-directed placement
  | sfdp     -- scalable force-directed
  | osage    -- array-based layout
  | patchwork -- squarified treemaps
  deriving Repr, BEq

namespace LayoutEngine
def toString : LayoutEngine → String
  | dot => "dot"
  | neato => "neato"
  | twopi => "twopi"
  | circo => "circo"
  | fdp => "fdp"
  | sfdp => "sfdp"
  | osage => "osage"
  | patchwork => "patchwork"

def fromString? (s : String) : Option LayoutEngine :=
  match s with
  | "dot" => some dot
  | "neato" => some neato
  | "twopi" => some twopi
  | "circo" => some circo
  | "fdp" => some fdp
  | "sfdp" => some sfdp
  | "osage" => some osage
  | "patchwork" => some patchwork
  | _ => Option.none

def allNames : List String := ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "osage", "patchwork"]
end LayoutEngine

/-- Overlap removal mode for layouts -/
inductive OverlapMode where
  | «true»    -- allow overlap
  | «false»   -- remove overlap
  | scale     -- scale to avoid overlap
  | scalexy   -- scale separately in X and Y
  | ortho     -- orthogonal projection
  | orthoxy   -- ortho in X first
  | orthoyx   -- ortho in Y first
  | compress  -- compress after overlap removal
  | vpsc      -- variable placement with constraints
  | voronoi   -- voronoi-based
  | ipsep     -- iterative proximity separation
  | prism     -- proximity graph-based
  deriving Repr, BEq

namespace OverlapMode
def toString : OverlapMode → String
  | «true» => "true"
  | «false» => "false"
  | scale => "scale"
  | scalexy => "scalexy"
  | ortho => "ortho"
  | orthoxy => "orthoxy"
  | orthoyx => "orthoyx"
  | compress => "compress"
  | vpsc => "vpsc"
  | voronoi => "voronoi"
  | ipsep => "ipsep"
  | prism => "prism"

def fromString? (s : String) : Option OverlapMode :=
  match s with
  | "true" => some «true»
  | "false" => some «false»
  | "scale" => some scale
  | "scalexy" => some scalexy
  | "ortho" => some ortho
  | "orthoxy" => some orthoxy
  | "orthoyx" => some orthoyx
  | "compress" => some compress
  | "vpsc" => some vpsc
  | "voronoi" => some voronoi
  | "ipsep" => some ipsep
  | "prism" => some prism
  | _ => Option.none

def allNames : List String := [
  "true", "false", "scale", "scalexy", "ortho", "orthoxy",
  "orthoyx", "compress", "vpsc", "voronoi", "ipsep", "prism"
]
end OverlapMode

/-- Label location for graph/cluster labels -/
inductive LabelLoc where
  | t  -- top
  | c  -- center
  | b  -- bottom
  deriving Repr, BEq

namespace LabelLoc
def toString : LabelLoc → String
  | t => "t"
  | c => "c"
  | b => "b"

def fromString? (s : String) : Option LabelLoc :=
  match s with
  | "t" => some t
  | "c" => some c
  | "b" => some b
  | "top" => some t
  | "center" => some c
  | "bottom" => some b
  | _ => Option.none

def allNames : List String := ["t", "c", "b"]
end LabelLoc

/-- Label justification -/
inductive LabelJust where
  | l  -- left
  | c  -- center
  | r  -- right
  deriving Repr, BEq

namespace LabelJust
def toString : LabelJust → String
  | l => "l"
  | c => "c"
  | r => "r"

def fromString? (s : String) : Option LabelJust :=
  match s with
  | "l" => some l
  | "c" => some c
  | "r" => some r
  | "left" => some l
  | "center" => some c
  | "right" => some r
  | _ => Option.none

def allNames : List String := ["l", "c", "r"]
end LabelJust

/-- Cluster mode for compound graphs -/
inductive ClusterMode where
  | «local»   -- clusters are local
  | global    -- clusters share namespace
  | «none»    -- no special cluster handling
  deriving Repr, BEq

namespace ClusterMode
def toString : ClusterMode → String
  | «local» => "local"
  | global => "global"
  | «none» => "none"

def fromString? (s : String) : Option ClusterMode :=
  match s with
  | "local" => some «local»
  | "global" => some global
  | "none" => some «none»
  | _ => Option.none

def allNames : List String := ["local", "global", "none"]
end ClusterMode

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
def edgeStylesT (ss : List EdgeStyle) : Attr := Attr.mk "style" (EdgeStyle.combine ss)
def splinesT (s : SplineType) : Attr := Attr.mk "splines" s.toString
def outputorderT (o : OutputMode) : Attr := Attr.mk "outputorder" o.toString

-- New typed constructors
def layoutT (l : LayoutEngine) : Attr := Attr.mk "layout" l.toString
def overlapT (o : OverlapMode) : Attr := Attr.mk "overlap" o.toString
def labellocT (l : LabelLoc) : Attr := Attr.mk "labelloc" l.toString
def labeljustT (j : LabelJust) : Attr := Attr.mk "labeljust" j.toString
def clustermodeT (c : ClusterMode) : Attr := Attr.mk "clusterrank" c.toString

end Attr

end Dot4

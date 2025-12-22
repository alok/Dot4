import Dot4.Basic

/-!
# Type-Safe Shapes and Enums for DOT

Type-safe representations of Graphviz enumerated attributes.
No more typos like {lit}`shape="circl"` - the compiler catches it!

See:
- [Node Shapes](https://graphviz.org/doc/info/shapes.html)
- [Arrow Shapes](https://graphviz.org/doc/info/arrows.html)
- [Attributes](https://graphviz.org/doc/info/attrs.html)
-/

namespace Dot4

-- Enum constructors match DOT values exactly, so they're self-documenting
set_option linter.missingDocs false

/-- All valid Graphviz node shapes.
See <https://graphviz.org/doc/info/shapes.html> -/
inductive Shape where
  -- Polygon-based shapes
  /-- Rectangular box shape -/ | box
  /-- Regular polygon shape -/ | polygon
  /-- Ellipse shape -/ | ellipse
  /-- Oval shape (synonym for ellipse) -/ | oval
  /-- Circular shape -/ | circle
  /-- Point shape (single pixel) -/ | point
  /-- Egg shape -/ | egg
  /-- Triangle shape -/ | triangle
  /-- Plain text with no border -/ | plaintext
  /-- Plain text (synonym for plaintext) -/ | plain
  /-- Diamond shape -/ | diamond
  /-- Trapezium shape -/ | trapezium
  /-- Parallelogram shape -/ | parallelogram
  /-- House shape (pentagon with flat base) -/ | house
  /-- Pentagon shape -/ | pentagon
  /-- Hexagon shape -/ | hexagon
  /-- Septagon (7-sided polygon) -/ | septagon
  /-- Octagon shape -/ | octagon
  /-- Double circle outline -/ | doublecircle
  /-- Double octagon outline -/ | doubleoctagon
  /-- Triple octagon outline -/ | tripleoctagon
  /-- Inverted triangle (pointing down) -/ | invtriangle
  /-- Inverted trapezium -/ | invtrapezium
  /-- Inverted house -/ | invhouse
  /-- Diamond with Mrecord margins -/ | Mdiamond
  /-- Square with Mrecord margins -/ | Msquare
  /-- Circle with Mrecord margins -/ | Mcircle
  -- Record shapes
  /-- Record-based node with horizontal/vertical layout -/ | record
  /-- Record with rounded corners -/ | Mrecord
  -- Special shapes
  /-- Rectangle (synonym for box) -/ | rect
  /-- Rectangle (synonym for box) -/ | rectangle
  /-- Square shape -/ | square
  /-- Star shape -/ | star
  /-- No shape (invisible) -/ | none
  /-- Underline shape -/ | underline
  /-- Cylinder shape -/ | cylinder
  /-- Note shape (dog-eared rectangle) -/ | note
  /-- Folder tab shape -/ | tab
  /-- Folder shape -/ | folder
  /-- 3D box with perspective -/ | box3d
  /-- Component shape (UML) -/ | component
  /-- Promoter shape (molecular biology) -/ | promoter
  /-- Coding sequence shape (molecular biology) -/ | cds
  /-- Terminator shape (molecular biology) -/ | terminator
  /-- Untranslated region shape (molecular biology) -/ | utr
  /-- Primer binding site shape (molecular biology) -/ | primersite
  /-- Restriction site shape (molecular biology) -/ | restrictionsite
  /-- 5' overhang shape (molecular biology) -/ | fivepoverhang
  /-- 3' overhang shape (molecular biology) -/ | threepoverhang
  /-- No overhang shape (molecular biology) -/ | noverhang
  /-- Assembly shape (molecular biology) -/ | assembly
  /-- Signature shape (molecular biology) -/ | signature
  /-- Insulator shape (molecular biology) -/ | insulator
  /-- Ribosome binding site shape (molecular biology) -/ | ribosite
  /-- RNA stability element shape (molecular biology) -/ | rnastab
  /-- Protease site shape (molecular biology) -/ | proteasesite
  /-- Protein stability element shape (molecular biology) -/ | proteinstab
  /-- Right-facing promoter shape (molecular biology) -/ | rpromoter
  /-- Right-facing arrow shape (molecular biology) -/ | rarrow
  /-- Left-facing arrow shape (molecular biology) -/ | larrow
  /-- Left-facing promoter shape (molecular biology) -/ | lpromoter
  deriving Repr, BEq, Hashable

namespace Shape

/-- Convert a Shape to its DOT string representation -/
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

/-- List of all valid shape names as strings -/
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

/-- Arrow head/tail shapes.
See <https://graphviz.org/doc/info/arrows.html> -/
inductive ArrowShape where
  /-- Box arrow head -/ | box
  /-- Crow's foot arrow (multiple lines) -/ | crow
  /-- Curved arrow -/ | curve
  /-- Inverted curve arrow -/ | icurve
  /-- Diamond arrow head -/ | diamond
  /-- Dot (filled circle) arrow -/ | dot
  /-- Inverted arrow head -/ | inv
  /-- No arrow head -/ | none
  /-- Normal arrow head (default) -/ | normal
  /-- Tee arrow (perpendicular line) -/ | tee
  /-- Vee arrow (open triangle) -/ | vee
  /-- Outlined dot arrow -/ | odot
  /-- Inverted filled dot -/ | invdot
  /-- Inverted outlined dot -/ | invodot
  /-- Outlined box arrow -/ | obox
  /-- Outlined diamond arrow -/ | odiamond
  deriving Repr, BEq

namespace ArrowShape

/-- Convert an ArrowShape to its DOT string representation -/
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

/-- Parse a string to an ArrowShape, returning none if invalid -/
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

/-- List of all valid arrow shape names as strings -/
def allNames : List String := [
  "box", "crow", "curve", "icurve", "diamond", "dot", "inv", "none",
  "normal", "tee", "vee", "odot", "invdot", "invodot", "obox", "odiamond"
]

end ArrowShape

/-- Edge direction for arrow display -/
inductive EdgeDir where
  /-- Arrow points from tail to head (default) -/ | forward
  /-- Arrow points from head to tail -/ | back
  /-- Arrows point in both directions -/ | both
  /-- No arrows -/ | none
  deriving Repr, BEq

namespace EdgeDir
/-- Convert an EdgeDir to its DOT string representation -/
def toString : EdgeDir → String
  | forward => "forward"
  | back => "back"
  | both => "both"
  | none => "none"

/-- Parse a string to an EdgeDir, returning none if invalid -/
def fromString? (s : String) : Option EdgeDir :=
  match s with
  | "forward" => some forward
  | "back" => some back
  | "both" => some both
  | "none" => some none
  | _ => Option.none

/-- List of all valid edge direction names as strings -/
def allNames : List String := ["forward", "back", "both", "none"]
end EdgeDir

/-- Rank direction for graph layout -/
inductive RankDir where
  /-- Top to bottom layout -/ | TB
  /-- Bottom to top layout -/ | BT
  /-- Left to right layout -/ | LR
  /-- Right to left layout -/ | RL
  deriving Repr, BEq

namespace RankDir
/-- Convert a RankDir to its DOT string representation -/
def toString : RankDir → String
  | TB => "TB"
  | BT => "BT"
  | LR => "LR"
  | RL => "RL"

/-- Parse a string to a RankDir, returning none if invalid -/
def fromString? (s : String) : Option RankDir :=
  match s with
  | "TB" => some TB
  | "BT" => some BT
  | "LR" => some LR
  | "RL" => some RL
  | _ => Option.none

/-- List of all valid rank direction names as strings -/
def allNames : List String := ["TB", "BT", "LR", "RL"]
end RankDir

-- RankType is defined in Basic.lean (used by Subgraph)

/-- Node style attributes -/
inductive NodeStyle where
  /-- Solid outline (default) -/ | solid
  /-- Dashed outline -/ | dashed
  /-- Dotted outline -/ | dotted
  /-- Bold outline -/ | bold
  /-- Rounded corners -/ | rounded
  /-- Diagonal corner lines -/ | diagonals
  /-- Filled with background color -/ | filled
  /-- Striped fill pattern -/ | striped
  /-- Wedged fill pattern -/ | wedged
  /-- Invisible node -/ | invis
  deriving Repr, BEq

namespace NodeStyle
/-- Convert a NodeStyle to its DOT string representation -/
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

/-- Parse a string to a NodeStyle, returning none if invalid -/
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

/-- List of all valid node style names as strings -/
def allNames : List String := [
  "solid", "dashed", "dotted", "bold", "rounded",
  "diagonals", "filled", "striped", "wedged", "invis"
]

/-- Combine multiple node styles into a comma-separated string -/
def combine (styles : List NodeStyle) : String :=
  ",".intercalate (styles.map toString)
end NodeStyle

/-- Edge style attributes -/
inductive EdgeStyle where
  /-- Solid line (default) -/ | solid
  /-- Dashed line -/ | dashed
  /-- Dotted line -/ | dotted
  /-- Bold line -/ | bold
  /-- Invisible edge -/ | invis
  /-- Tapered line (width varies) -/ | tapered
  deriving Repr, BEq

namespace EdgeStyle
/-- Convert an EdgeStyle to its DOT string representation -/
def toString : EdgeStyle → String
  | solid => "solid"
  | dashed => "dashed"
  | dotted => "dotted"
  | bold => "bold"
  | invis => "invis"
  | tapered => "tapered"

/-- Parse a string to an EdgeStyle, returning none if invalid -/
def fromString? (s : String) : Option EdgeStyle :=
  match s with
  | "solid" => some solid
  | "dashed" => some dashed
  | "dotted" => some dotted
  | "bold" => some bold
  | "invis" => some invis
  | "tapered" => some tapered
  | _ => Option.none

/-- List of all valid edge style names as strings -/
def allNames : List String := ["solid", "dashed", "dotted", "bold", "invis", "tapered"]

/-- Combine multiple edge styles into a comma-separated string -/
def combine (styles : List EdgeStyle) : String :=
  ",".intercalate (styles.map toString)
end EdgeStyle

/-- Spline type for edge routing -/
inductive SplineType where
  /-- No edge routing -/ | none
  /-- Straight line routing -/ | line
  /-- Polyline routing (straight segments) -/ | polyline
  /-- Curved bezier spline routing -/ | curved
  /-- Orthogonal routing (right angles) -/ | ortho
  /-- Spline routing (default) -/ | spline
  deriving Repr, BEq

namespace SplineType
/-- Convert a SplineType to its DOT string representation -/
def toString : SplineType → String
  | none => "none"
  | line => "line"
  | polyline => "polyline"
  | curved => "curved"
  | ortho => "ortho"
  | spline => "spline"

/-- Parse a string to a SplineType, returning none if invalid -/
def fromString? (s : String) : Option SplineType :=
  match s with
  | "none" => some none
  | "line" => some line
  | "polyline" => some polyline
  | "curved" => some curved
  | "ortho" => some ortho
  | "spline" => some spline
  | _ => Option.none

/-- List of all valid spline type names as strings -/
def allNames : List String := ["none", "line", "polyline", "curved", "ortho", "spline"]
end SplineType

/-- Output order for rendering -/
inductive OutputMode where
  /-- Breadth-first traversal order -/ | breadthfirst
  /-- Render all nodes before edges -/ | nodesfirst
  /-- Render all edges before nodes -/ | edgesfirst
  deriving Repr, BEq

namespace OutputMode
/-- Convert an OutputMode to its DOT string representation -/
def toString : OutputMode → String
  | breadthfirst => "breadthfirst"
  | nodesfirst => "nodesfirst"
  | edgesfirst => "edgesfirst"

/-- Parse a string to an OutputMode, returning none if invalid -/
def fromString? (s : String) : Option OutputMode :=
  match s with
  | "breadthfirst" => some breadthfirst
  | "nodesfirst" => some nodesfirst
  | "edgesfirst" => some edgesfirst
  | _ => Option.none

/-- List of all valid output mode names as strings -/
def allNames : List String := ["breadthfirst", "nodesfirst", "edgesfirst"]
end OutputMode

/-! ## New Enums for Complete Graphviz Coverage -/

/-- Layout engine selection -/
inductive LayoutEngine where
  /-- Hierarchical layout for directed graphs -/ | dot
  /-- Spring model layout for undirected graphs -/ | neato
  /-- Radial layout -/ | twopi
  /-- Circular layout -/ | circo
  /-- Force-directed placement -/ | fdp
  /-- Scalable force-directed placement -/ | sfdp
  /-- Array-based layout -/ | osage
  /-- Squarified treemap layout -/ | patchwork
  deriving Repr, BEq

namespace LayoutEngine
/-- Convert a LayoutEngine to its DOT string representation -/
def toString : LayoutEngine → String
  | dot => "dot"
  | neato => "neato"
  | twopi => "twopi"
  | circo => "circo"
  | fdp => "fdp"
  | sfdp => "sfdp"
  | osage => "osage"
  | patchwork => "patchwork"

/-- Parse a string to a LayoutEngine, returning none if invalid -/
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

/-- List of all valid layout engine names as strings -/
def allNames : List String := ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "osage", "patchwork"]
end LayoutEngine

/-- Overlap removal mode for layouts -/
inductive OverlapMode where
  /-- Allow overlap -/ | «true»
  /-- Remove overlap -/ | «false»
  /-- Scale uniformly to avoid overlap -/ | scale
  /-- Scale separately in X and Y -/ | scalexy
  /-- Orthogonal projection overlap removal -/ | ortho
  /-- Orthogonal in X direction first -/ | orthoxy
  /-- Orthogonal in Y direction first -/ | orthoyx
  /-- Compress after overlap removal -/ | compress
  /-- Variable placement with separation constraints -/ | vpsc
  /-- Voronoi-based overlap removal -/ | voronoi
  /-- Iterative proximity separation -/ | ipsep
  /-- Proximity graph-based overlap removal -/ | prism
  deriving Repr, BEq

namespace OverlapMode
/-- Convert an OverlapMode to its DOT string representation -/
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

/-- Parse a string to an OverlapMode, returning none if invalid -/
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

/-- List of all valid overlap mode names as strings -/
def allNames : List String := [
  "true", "false", "scale", "scalexy", "ortho", "orthoxy",
  "orthoyx", "compress", "vpsc", "voronoi", "ipsep", "prism"
]
end OverlapMode

/-- Label location for graph/cluster labels -/
inductive LabelLoc where
  /-- Top placement -/ | t
  /-- Center placement -/ | c
  /-- Bottom placement -/ | b
  deriving Repr, BEq

namespace LabelLoc
/-- Convert a LabelLoc to its DOT string representation -/
def toString : LabelLoc → String
  | t => "t"
  | c => "c"
  | b => "b"

/-- Parse a string to a LabelLoc, returning none if invalid -/
def fromString? (s : String) : Option LabelLoc :=
  match s with
  | "t" => some t
  | "c" => some c
  | "b" => some b
  | "top" => some t
  | "center" => some c
  | "bottom" => some b
  | _ => Option.none

/-- List of all valid label location names as strings -/
def allNames : List String := ["t", "c", "b"]
end LabelLoc

/-- Label justification -/
inductive LabelJust where
  /-- Left justification -/ | l
  /-- Center justification -/ | c
  /-- Right justification -/ | r
  deriving Repr, BEq

namespace LabelJust
/-- Convert a LabelJust to its DOT string representation -/
def toString : LabelJust → String
  | l => "l"
  | c => "c"
  | r => "r"

/-- Parse a string to a LabelJust, returning none if invalid -/
def fromString? (s : String) : Option LabelJust :=
  match s with
  | "l" => some l
  | "c" => some c
  | "r" => some r
  | "left" => some l
  | "center" => some c
  | "right" => some r
  | _ => Option.none

/-- List of all valid label justification names as strings -/
def allNames : List String := ["l", "c", "r"]
end LabelJust

/-- Cluster mode for compound graphs -/
inductive ClusterMode where
  /-- Clusters use local namespace -/ | «local»
  /-- Clusters share global namespace -/ | global
  /-- No special cluster handling -/ | «none»
  deriving Repr, BEq

namespace ClusterMode
/-- Convert a ClusterMode to its DOT string representation -/
def toString : ClusterMode → String
  | «local» => "local"
  | global => "global"
  | «none» => "none"

/-- Parse a string to a ClusterMode, returning none if invalid -/
def fromString? (s : String) : Option ClusterMode :=
  match s with
  | "local" => some «local»
  | "global" => some global
  | "none" => some «none»
  | _ => Option.none

/-- List of all valid cluster mode names as strings -/
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

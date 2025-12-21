import Dot4.Basic

/-!
# Type-Safe Colors for DOT

Provides type-safe color specifications for Graphviz DOT graphs.
Supports named colors, hex codes, RGB, RGBA, and HSV.
-/

namespace Dot4

/-- Color specification for DOT attributes -/
inductive Color where
  | named (name : String)       -- X11 color name
  | hex (code : String)         -- #RRGGBB or #RRGGBBAA
  | rgb (r g b : UInt8)         -- RGB values 0-255
  | rgba (r g b a : UInt8)      -- RGBA with alpha
  | hsv (h s v : Float)         -- HSV (0-1 range)
  deriving Repr, BEq

namespace Color

/-- Convert a nibble to hex character -/
private def hexChar (x : Nat) : Char :=
  if x < 10 then Char.ofNat (48 + x) else Char.ofNat (55 + x)

/-- Convert a byte to two hex characters -/
private def toHex (n : UInt8) : String :=
  let hi := n.toNat / 16
  let lo := n.toNat % 16
  String.ofList [hexChar hi, hexChar lo]

/-- Convert color to DOT format string -/
def toString : Color → String
  | named n => n
  | hex h => if h.startsWith "#" then h else "#" ++ h
  | rgb r g b => s!"#{toHex r}{toHex g}{toHex b}"
  | rgba r g b a => s!"#{toHex r}{toHex g}{toHex b}{toHex a}"
  | hsv h s v => s!"{h} {s} {v}"

-- Basic colors
def black : Color := named "black"
def white : Color := named "white"
def red : Color := named "red"
def green : Color := named "green"
def blue : Color := named "blue"
def yellow : Color := named "yellow"
def cyan : Color := named "cyan"
def magenta : Color := named "magenta"
def orange : Color := named "orange"
def purple : Color := named "purple"
def pink : Color := named "pink"
def brown : Color := named "brown"
def gray : Color := named "gray"
def grey : Color := named "grey"

-- Light variants
def lightblue : Color := named "lightblue"
def lightgreen : Color := named "lightgreen"
def lightgray : Color := named "lightgray"
def lightgrey : Color := named "lightgrey"
def lightyellow : Color := named "lightyellow"
def lightpink : Color := named "lightpink"
def lightcyan : Color := named "lightcyan"

-- Dark variants
def darkblue : Color := named "darkblue"
def darkgreen : Color := named "darkgreen"
def darkgray : Color := named "darkgray"
def darkgrey : Color := named "darkgrey"
def darkred : Color := named "darkred"
def darkorange : Color := named "darkorange"
def darkcyan : Color := named "darkcyan"

-- Grays
def gray10 : Color := named "gray10"
def gray20 : Color := named "gray20"
def gray30 : Color := named "gray30"
def gray40 : Color := named "gray40"
def gray50 : Color := named "gray50"
def gray60 : Color := named "gray60"
def gray70 : Color := named "gray70"
def gray80 : Color := named "gray80"
def gray90 : Color := named "gray90"

-- Popular web colors
def coral : Color := named "coral"
def crimson : Color := named "crimson"
def gold : Color := named "gold"
def indigo : Color := named "indigo"
def ivory : Color := named "ivory"
def khaki : Color := named "khaki"
def lavender : Color := named "lavender"
def lime : Color := named "lime"
def maroon : Color := named "maroon"
def navy : Color := named "navy"
def olive : Color := named "olive"
def orchid : Color := named "orchid"
def plum : Color := named "plum"
def salmon : Color := named "salmon"
def sienna : Color := named "sienna"
def silver : Color := named "silver"
def tan : Color := named "tan"
def teal : Color := named "teal"
def tomato : Color := named "tomato"
def turquoise : Color := named "turquoise"
def violet : Color := named "violet"
def wheat : Color := named "wheat"

-- Special
def transparent : Color := named "transparent"
def none : Color := named "none"

-- Solarized palette (popular for diagrams)
def solarizedBase03 : Color := hex "#002b36"
def solarizedBase02 : Color := hex "#073642"
def solarizedBase01 : Color := hex "#586e75"
def solarizedBase00 : Color := hex "#657b83"
def solarizedBase0 : Color := hex "#839496"
def solarizedBase1 : Color := hex "#93a1a1"
def solarizedBase2 : Color := hex "#eee8d5"
def solarizedBase3 : Color := hex "#fdf6e3"
def solarizedYellow : Color := hex "#b58900"
def solarizedOrange : Color := hex "#cb4b16"
def solarizedRed : Color := hex "#dc322f"
def solarizedMagenta : Color := hex "#d33682"
def solarizedViolet : Color := hex "#6c71c4"
def solarizedBlue : Color := hex "#268bd2"
def solarizedCyan : Color := hex "#2aa198"
def solarizedGreen : Color := hex "#859900"

-- Nord palette
def nordPolarNight0 : Color := hex "#2e3440"
def nordPolarNight1 : Color := hex "#3b4252"
def nordPolarNight2 : Color := hex "#434c5e"
def nordPolarNight3 : Color := hex "#4c566a"
def nordSnowStorm0 : Color := hex "#d8dee9"
def nordSnowStorm1 : Color := hex "#e5e9f0"
def nordSnowStorm2 : Color := hex "#eceff4"
def nordFrost0 : Color := hex "#8fbcbb"
def nordFrost1 : Color := hex "#88c0d0"
def nordFrost2 : Color := hex "#81a1c1"
def nordFrost3 : Color := hex "#5e81ac"
def nordAuroraRed : Color := hex "#bf616a"
def nordAuroraOrange : Color := hex "#d08770"
def nordAuroraYellow : Color := hex "#ebcb8b"
def nordAuroraGreen : Color := hex "#a3be8c"
def nordAuroraPurple : Color := hex "#b48ead"

-- Catppuccin Mocha palette
def catppuccinRosewater : Color := hex "#f5e0dc"
def catppuccinFlamingo : Color := hex "#f2cdcd"
def catppuccinPink : Color := hex "#f5c2e7"
def catppuccinMauve : Color := hex "#cba6f7"
def catppuccinRed : Color := hex "#f38ba8"
def catppuccinMaroon : Color := hex "#eba0ac"
def catppuccinPeach : Color := hex "#fab387"
def catppuccinYellow : Color := hex "#f9e2af"
def catppuccinGreen : Color := hex "#a6e3a1"
def catppuccinTeal : Color := hex "#94e2d5"
def catppuccinSky : Color := hex "#89dceb"
def catppuccinSapphire : Color := hex "#74c7ec"
def catppuccinBlue : Color := hex "#89b4fa"
def catppuccinLavender : Color := hex "#b4befe"
def catppuccinText : Color := hex "#cdd6f4"
def catppuccinBase : Color := hex "#1e1e2e"
def catppuccinMantle : Color := hex "#181825"
def catppuccinCrust : Color := hex "#11111b"

end Color

-- Extend Attr namespace with color-aware constructors
namespace Attr

def colorC (c : Color) : Attr := Attr.mk "color" c.toString
def fillcolorC (c : Color) : Attr := Attr.mk "fillcolor" c.toString
def fontcolorC (c : Color) : Attr := Attr.mk "fontcolor" c.toString
def bgcolorC (c : Color) : Attr := Attr.mk "bgcolor" c.toString

end Attr

end Dot4

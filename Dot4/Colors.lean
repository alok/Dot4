import Dot4.Basic
import HexLuthor.Elab

/-!
# Type-Safe Colors for DOT

Provides type-safe color specifications for Graphviz DOT graphs.
Supports named colors, hex codes (via HexLuthor), RGB, RGBA, and HSV.
-/

namespace Dot4

open HexLuthor

/-- Color specification for DOT attributes -/
inductive Color where
  | named (name : String)       -- X11 color name
  | hex (color : Hex)           -- Hex color via HexLuthor
  | rgb (r g b : UInt8)         -- RGB values 0-255
  | rgba (r g b a : UInt8)      -- RGBA with alpha
  | hsv (h s v : Float)         -- HSV (0-1 range)
  deriving Repr, BEq

namespace Color

/-- Convert color to DOT format string -/
def toString : Color → String
  | named n => n
  | hex h => h.toHexString
  | rgb r g b => Hex.rgb r g b |>.toHexString
  | rgba r g b a => Hex.rgba r g b a |>.toHexString
  | hsv h s v => s!"{h} {s} {v}"

/-- Create a color from a HexLuthor Hex value -/
def fromHex (h : Hex) : Color := .hex h

/-- Coercion from Hex to Color -/
instance : Coe Hex Color := ⟨.hex⟩

/-- Check if a hex character is valid -/
private def isHexChar (c : Char) : Bool :=
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

/-- Validate that a color string is valid for Graphviz -/
def isValid (s : String) : Bool :=
  -- Hex colors: #RRGGBB or #RRGGBBAA
  if s.startsWith "#" then
    let hexPart := s.drop 1
    (hexPart.length == 6 || hexPart.length == 8) && hexPart.all isHexChar
  -- rgb() syntax
  else if s.startsWith "rgb(" && s.endsWith ")" then
    true  -- Pass through, Graphviz handles it
  -- Named colors (relaxed - accept any alphanumeric)
  else
    s.all (fun c => c.isAlpha || c.isDigit)

-- Basic colors (named)
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

-- Solarized palette (using HexLuthor hex literals with coercion)
def solarizedBase03 : Color := #x002B36
def solarizedBase02 : Color := #x073642
def solarizedBase01 : Color := #x586E75
def solarizedBase00 : Color := #x657B83
def solarizedBase0 : Color := #x839496
def solarizedBase1 : Color := #x93A1A1
def solarizedBase2 : Color := #xEEE8D5
def solarizedBase3 : Color := #xFDF6E3
def solarizedYellow : Color := #xB58900
def solarizedOrange : Color := #xCB4B16
def solarizedRed : Color := #xDC322F
def solarizedMagenta : Color := #xD33682
def solarizedViolet : Color := #x6C71C4
def solarizedBlue : Color := #x268BD2
def solarizedCyan : Color := #x2AA198
def solarizedGreen : Color := #x859900

-- Nord palette
def nordPolarNight0 : Color := #x2E3440
def nordPolarNight1 : Color := #x3B4252
def nordPolarNight2 : Color := #x434C5E
def nordPolarNight3 : Color := #x4C566A
def nordSnowStorm0 : Color := #xD8DEE9
def nordSnowStorm1 : Color := #xE5E9F0
def nordSnowStorm2 : Color := #xECEFF4
def nordFrost0 : Color := #x8FBCBB
def nordFrost1 : Color := #x88C0D0
def nordFrost2 : Color := #x81A1C1
def nordFrost3 : Color := #x5E81AC
def nordAuroraRed : Color := #xBF616A
def nordAuroraOrange : Color := #xD08770
def nordAuroraYellow : Color := #xEBCB8B
def nordAuroraGreen : Color := #xA3BE8C
def nordAuroraPurple : Color := #xB48EAD

-- Catppuccin Mocha palette
def catppuccinRosewater : Color := #xF5E0DC
def catppuccinFlamingo : Color := #xF2CDCD
def catppuccinPink : Color := #xF5C2E7
def catppuccinMauve : Color := #xCBA6F7
def catppuccinRed : Color := #xF38BA8
def catppuccinMaroon : Color := #xEBA0AC
def catppuccinPeach : Color := #xFAB387
def catppuccinYellow : Color := #xF9E2AF
def catppuccinGreen : Color := #xA6E3A1
def catppuccinTeal : Color := #x94E2D5
def catppuccinSky : Color := #x89DCEB
def catppuccinSapphire : Color := #x74C7EC
def catppuccinBlue : Color := #x89B4FA
def catppuccinLavender : Color := #xB4BEFE
def catppuccinText : Color := #xCDD6F4
def catppuccinBase : Color := #x1E1E2E
def catppuccinMantle : Color := #x181825
def catppuccinCrust : Color := #x11111B

end Color

-- Extend Attr namespace with color-aware constructors
namespace Attr

def colorC (c : Color) : Attr := Attr.mk "color" c.toString
def fillcolorC (c : Color) : Attr := Attr.mk "fillcolor" c.toString
def fontcolorC (c : Color) : Attr := Attr.mk "fontcolor" c.toString
def bgcolorC (c : Color) : Attr := Attr.mk "bgcolor" c.toString

/-- Create color attribute directly from a Hex value -/
def colorH (h : Hex) : Attr := Attr.mk "color" h.toHexString
def fillcolorH (h : Hex) : Attr := Attr.mk "fillcolor" h.toHexString
def fontcolorH (h : Hex) : Attr := Attr.mk "fontcolor" h.toHexString
def bgcolorH (h : Hex) : Attr := Attr.mk "bgcolor" h.toHexString

end Attr

end Dot4

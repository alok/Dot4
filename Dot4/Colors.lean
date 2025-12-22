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
  /-- X11 color name (e.g., "red", "lightblue") -/
  | named (name : String)
  /-- Hex color via HexLuthor (e.g., `#xRRGGBB`) -/
  | hex (color : Hex)
  /-- RGB values 0-255 -/
  | rgb (r g b : UInt8)
  /-- RGBA with alpha channel 0-255 -/
  | rgba (r g b a : UInt8)
  /-- HSV color (hue, saturation, value in 0-1 range) -/
  | hsv (h s v : Float)
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

section ColorConstants

-- Basic colors
/-- Black -/
def black : Color := named "black"
/-- White -/
def white : Color := named "white"
/-- Red -/
def red : Color := named "red"
/-- Green -/
def green : Color := named "green"
/-- Blue -/
def blue : Color := named "blue"
/-- Yellow -/
def yellow : Color := named "yellow"
/-- Cyan -/
def cyan : Color := named "cyan"
/-- Magenta -/
def magenta : Color := named "magenta"
/-- Orange -/
def orange : Color := named "orange"
/-- Purple -/
def purple : Color := named "purple"
/-- Pink -/
def pink : Color := named "pink"
/-- Brown -/
def brown : Color := named "brown"
/-- Gray -/
def gray : Color := named "gray"
/-- Grey -/
def grey : Color := named "grey"

-- Light variants
/-- Light blue -/
def lightblue : Color := named "lightblue"
/-- Light green -/
def lightgreen : Color := named "lightgreen"
/-- Light gray -/
def lightgray : Color := named "lightgray"
/-- Light grey -/
def lightgrey : Color := named "lightgrey"
/-- Light yellow -/
def lightyellow : Color := named "lightyellow"
/-- Light pink -/
def lightpink : Color := named "lightpink"
/-- Light cyan -/
def lightcyan : Color := named "lightcyan"

-- Dark variants
/-- Dark blue -/
def darkblue : Color := named "darkblue"
/-- Dark green -/
def darkgreen : Color := named "darkgreen"
/-- Dark gray -/
def darkgray : Color := named "darkgray"
/-- Dark grey -/
def darkgrey : Color := named "darkgrey"
/-- Dark red -/
def darkred : Color := named "darkred"
/-- Dark orange -/
def darkorange : Color := named "darkorange"
/-- Dark cyan -/
def darkcyan : Color := named "darkcyan"

-- Grays
/-- Gray 10% -/
def gray10 : Color := named "gray10"
/-- Gray 20% -/
def gray20 : Color := named "gray20"
/-- Gray 30% -/
def gray30 : Color := named "gray30"
/-- Gray 40% -/
def gray40 : Color := named "gray40"
/-- Gray 50% -/
def gray50 : Color := named "gray50"
/-- Gray 60% -/
def gray60 : Color := named "gray60"
/-- Gray 70% -/
def gray70 : Color := named "gray70"
/-- Gray 80% -/
def gray80 : Color := named "gray80"
/-- Gray 90% -/
def gray90 : Color := named "gray90"

-- Popular web colors
/-- Coral -/
def coral : Color := named "coral"
/-- Crimson -/
def crimson : Color := named "crimson"
/-- Gold -/
def gold : Color := named "gold"
/-- Indigo -/
def indigo : Color := named "indigo"
/-- Ivory -/
def ivory : Color := named "ivory"
/-- Khaki -/
def khaki : Color := named "khaki"
/-- Lavender -/
def lavender : Color := named "lavender"
/-- Lime -/
def lime : Color := named "lime"
/-- Maroon -/
def maroon : Color := named "maroon"
/-- Navy -/
def navy : Color := named "navy"
/-- Olive -/
def olive : Color := named "olive"
/-- Orchid -/
def orchid : Color := named "orchid"
/-- Plum -/
def plum : Color := named "plum"
/-- Salmon -/
def salmon : Color := named "salmon"
/-- Sienna -/
def sienna : Color := named "sienna"
/-- Silver -/
def silver : Color := named "silver"
/-- Tan -/
def tan : Color := named "tan"
/-- Teal -/
def teal : Color := named "teal"
/-- Tomato -/
def tomato : Color := named "tomato"
/-- Turquoise -/
def turquoise : Color := named "turquoise"
/-- Violet -/
def violet : Color := named "violet"
/-- Wheat -/
def wheat : Color := named "wheat"

-- Special
/-- Transparent -/
def transparent : Color := named "transparent"
/-- None -/
def none : Color := named "none"

/-! #### Solarized Palette

Ethan Schoonover's precision color scheme. See <https://ethanschoonover.com/solarized/> -/
/-- Solarized base03 (#002B36) -/
def solarizedBase03 : Color := #x002B36
/-- Solarized base02 (#073642) -/
def solarizedBase02 : Color := #x073642
/-- Solarized base01 (#586E75) -/
def solarizedBase01 : Color := #x586E75
/-- Solarized base00 (#657B83) -/
def solarizedBase00 : Color := #x657B83
/-- Solarized base0 (#839496) -/
def solarizedBase0 : Color := #x839496
/-- Solarized base1 (#93A1A1) -/
def solarizedBase1 : Color := #x93A1A1
/-- Solarized base2 (#EEE8D5) -/
def solarizedBase2 : Color := #xEEE8D5
/-- Solarized base3 (#FDF6E3) -/
def solarizedBase3 : Color := #xFDF6E3
/-- Solarized yellow (#B58900) -/
def solarizedYellow : Color := #xB58900
/-- Solarized orange (#CB4B16) -/
def solarizedOrange : Color := #xCB4B16
/-- Solarized red (#DC322F) -/
def solarizedRed : Color := #xDC322F
/-- Solarized magenta (#D33682) -/
def solarizedMagenta : Color := #xD33682
/-- Solarized violet (#6C71C4) -/
def solarizedViolet : Color := #x6C71C4
/-- Solarized blue (#268BD2) -/
def solarizedBlue : Color := #x268BD2
/-- Solarized cyan (#2AA198) -/
def solarizedCyan : Color := #x2AA198
/-- Solarized green (#859900) -/
def solarizedGreen : Color := #x859900

/-! #### Nord Palette

Arctic, north-bluish color palette. See <https://www.nordtheme.com/> -/
/-- Nord Polar Night 0 (#2E3440) -/
def nordPolarNight0 : Color := #x2E3440
/-- Nord Polar Night 1 (#3B4252) -/
def nordPolarNight1 : Color := #x3B4252
/-- Nord Polar Night 2 (#434C5E) -/
def nordPolarNight2 : Color := #x434C5E
/-- Nord Polar Night 3 (#4C566A) -/
def nordPolarNight3 : Color := #x4C566A
/-- Nord Snow Storm 0 (#D8DEE9) -/
def nordSnowStorm0 : Color := #xD8DEE9
/-- Nord Snow Storm 1 (#E5E9F0) -/
def nordSnowStorm1 : Color := #xE5E9F0
/-- Nord Snow Storm 2 (#ECEFF4) -/
def nordSnowStorm2 : Color := #xECEFF4
/-- Nord Frost 0 (#8FBCBB) -/
def nordFrost0 : Color := #x8FBCBB
/-- Nord Frost 1 (#88C0D0) -/
def nordFrost1 : Color := #x88C0D0
/-- Nord Frost 2 (#81A1C1) -/
def nordFrost2 : Color := #x81A1C1
/-- Nord Frost 3 (#5E81AC) -/
def nordFrost3 : Color := #x5E81AC
/-- Nord Aurora red (#BF616A) -/
def nordAuroraRed : Color := #xBF616A
/-- Nord Aurora orange (#D08770) -/
def nordAuroraOrange : Color := #xD08770
/-- Nord Aurora yellow (#EBCB8B) -/
def nordAuroraYellow : Color := #xEBCB8B
/-- Nord Aurora green (#A3BE8C) -/
def nordAuroraGreen : Color := #xA3BE8C
/-- Nord Aurora purple (#B48EAD) -/
def nordAuroraPurple : Color := #xB48EAD

/-! #### Catppuccin Mocha Palette

Soothing pastel theme. See <https://catppuccin.com/> -/
/-- Catppuccin Mocha rosewater (#F5E0DC) -/
def catppuccinRosewater : Color := #xF5E0DC
/-- Catppuccin Mocha flamingo (#F2CDCD) -/
def catppuccinFlamingo : Color := #xF2CDCD
/-- Catppuccin Mocha pink (#F5C2E7) -/
def catppuccinPink : Color := #xF5C2E7
/-- Catppuccin Mocha mauve (#CBA6F7) -/
def catppuccinMauve : Color := #xCBA6F7
/-- Catppuccin Mocha red (#F38BA8) -/
def catppuccinRed : Color := #xF38BA8
/-- Catppuccin Mocha maroon (#EBA0AC) -/
def catppuccinMaroon : Color := #xEBA0AC
/-- Catppuccin Mocha peach (#FAB387) -/
def catppuccinPeach : Color := #xFAB387
/-- Catppuccin Mocha yellow (#F9E2AF) -/
def catppuccinYellow : Color := #xF9E2AF
/-- Catppuccin Mocha green (#A6E3A1) -/
def catppuccinGreen : Color := #xA6E3A1
/-- Catppuccin Mocha teal (#94E2D5) -/
def catppuccinTeal : Color := #x94E2D5
/-- Catppuccin Mocha sky (#89DCEB) -/
def catppuccinSky : Color := #x89DCEB
/-- Catppuccin Mocha sapphire (#74C7EC) -/
def catppuccinSapphire : Color := #x74C7EC
/-- Catppuccin Mocha blue (#89B4FA) -/
def catppuccinBlue : Color := #x89B4FA
/-- Catppuccin Mocha lavender (#B4BEFE) -/
def catppuccinLavender : Color := #xB4BEFE
/-- Catppuccin Mocha text (#CDD6F4) -/
def catppuccinText : Color := #xCDD6F4
/-- Catppuccin Mocha base (#1E1E2E) -/
def catppuccinBase : Color := #x1E1E2E
/-- Catppuccin Mocha mantle (#181825) -/
def catppuccinMantle : Color := #x181825
/-- Catppuccin Mocha crust (#11111B) -/
def catppuccinCrust : Color := #x11111B

end ColorConstants
end Color

/-! ### Color Attribute Constructors

Convenience functions for creating color-related DOT attributes. -/
namespace Attr

/-- Create a {name}`color` attribute from a {lean}`Color` value -/
def colorC (c : Color) : Attr := Attr.mk "color" c.toString
/-- Create a {name}`fillcolor` attribute from a {lean}`Color` value -/
def fillcolorC (c : Color) : Attr := Attr.mk "fillcolor" c.toString
/-- Create a {name}`fontcolor` attribute from a {lean}`Color` value -/
def fontcolorC (c : Color) : Attr := Attr.mk "fontcolor" c.toString
/-- Create a {name}`bgcolor` attribute from a {lean}`Color` value -/
def bgcolorC (c : Color) : Attr := Attr.mk "bgcolor" c.toString

/-- Create a {name}`color` attribute directly from a {lean}`Hex` value -/
def colorH (h : Hex) : Attr := Attr.mk "color" h.toHexString
/-- Create a {name}`fillcolor` attribute directly from a {lean}`Hex` value -/
def fillcolorH (h : Hex) : Attr := Attr.mk "fillcolor" h.toHexString
/-- Create a {name}`fontcolor` attribute directly from a {lean}`Hex` value -/
def fontcolorH (h : Hex) : Attr := Attr.mk "fontcolor" h.toHexString
/-- Create a {name}`bgcolor` attribute directly from a {lean}`Hex` value -/
def bgcolorH (h : Hex) : Attr := Attr.mk "bgcolor" h.toHexString

end Attr

end Dot4

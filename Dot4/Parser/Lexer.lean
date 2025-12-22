import Std.Internal.Parsec
import Std.Internal.Parsec.String

/-!
# DOT Lexer

Tokenizer for DOT language using Lean's Parsec library.
Handles keywords, identifiers, strings, symbols, and comments.
-/

namespace Dot4.Parser

open Std.Internal.Parsec String

set_option linter.missingDocs false

/-- DOT tokens -/
inductive Token where
  -- Keywords
  | strict
  | digraph
  | graph
  | subgraph
  | node
  | edge
  -- Identifiers and literals
  | ident (value : String)
  | number (value : String)
  | quotedString (value : String)
  -- Symbols
  | lbrace      -- {
  | rbrace      -- }
  | lbracket    -- [
  | rbracket    -- ]
  | equals      -- =
  | semicolon   -- ;
  | comma       -- ,
  | colon       -- :
  | arrow       -- ->
  | dashdash    -- --
  -- End of input
  | eof
  deriving Repr, BEq, Inhabited

namespace Token

def toString : Token → String
  | .strict => "strict"
  | .digraph => "digraph"
  | .graph => "graph"
  | .subgraph => "subgraph"
  | .node => "node"
  | .edge => "edge"
  | .ident s => s!"ident({s})"
  | .number s => s!"number({s})"
  | .quotedString s => s!"string({s})"
  | .lbrace => "{"
  | .rbrace => "}"
  | .lbracket => "["
  | .rbracket => "]"
  | .equals => "="
  | .semicolon => ";"
  | .comma => ","
  | .colon => ":"
  | .arrow => "->"
  | .dashdash => "--"
  | .eof => "EOF"

instance : ToString Token := ⟨Token.toString⟩

end Token

/-- Check if character can start an unquoted identifier -/
def isIdentStart (c : Char) : Bool :=
  c.isAlpha || c == '_'

/-- Check if character can continue an unquoted identifier -/
def isIdentCont (c : Char) : Bool :=
  c.isAlphanum || c == '_'

/-- Unescape a DOT string (inverse of escapeString) -/
def unescapeString (s : String) : String :=
  let rec go (chars : List Char) (acc : List Char) : List Char :=
    match chars with
    | [] => acc.reverse
    | '\\' :: 'n' :: rest => go rest ('\n' :: acc)
    | '\\' :: 'l' :: rest => go rest ('\n' :: acc)  -- DOT left-justified newline
    | '\\' :: 'r' :: rest => go rest ('\r' :: acc)  -- DOT right-justified newline
    | '\\' :: '"' :: rest => go rest ('"' :: acc)
    | '\\' :: '\\' :: rest => go rest ('\\' :: acc)
    | c :: rest => go rest (c :: acc)
  String.ofList (go s.toList [])

/-- Skip to end of block comment -/
partial def skipToEndComment : Parser Unit := do
  let _ ← many (satisfy fun c => c != '*')
  let _ ← pchar '*'
  (pchar '/' *> pure ()) <|> skipToEndComment

/-- Skip C-style block comment -/
def blockComment : Parser Unit := do
  let _ ← pstring "/*"
  skipToEndComment

/-- Skip C++-style line comment // ... -/
def lineComment : Parser Unit := do
  let _ ← pstring "//"
  let _ ← many (satisfy fun c => c != '\n')
  pure ()

/-- Skip at least one whitespace character -/
def ws1 : Parser Unit := do
  let _ ← satisfy Char.isWhitespace
  let _ ← many (satisfy Char.isWhitespace)
  pure ()

/-- Skip whitespace and comments -/
partial def skipWsAndComments : Parser Unit := do
  let _ ← many (ws1 <|> blockComment <|> lineComment)
  pure ()

/-- Parse unquoted identifier -/
def unquotedIdent : Parser String := do
  let first ← satisfy isIdentStart
  let rest ← many (satisfy isIdentCont)
  pure (first.toString ++ String.ofList rest.toList)

/-- Parse quoted string contents (between quotes) -/
partial def quotedStringContents : Parser String := do
  let chars ← go []
  pure (String.ofList chars.reverse)
where
  go (acc : List Char) : Parser (List Char) :=
    (do
      let c ← satisfy (· != '"')
      if c == '\\' then
        let escaped ← satisfy fun _ => true  -- any char after backslash
        go (escaped :: c :: acc)
      else
        go (c :: acc))
    <|> pure acc

/-- Parse quoted string with escape handling -/
def quotedString : Parser String := do
  let _ ← pchar '"'
  let contents ← quotedStringContents
  let _ ← pchar '"'
  pure (unescapeString contents)

/-- Parse a numeric literal (integer or float, possibly negative) -/
def numericLiteral : Parser String := do
  let neg ← (pchar '-' *> pure "-") <|> pure ""
  let digits ← many1 digit
  let intPart := String.ofList digits.toList
  let fracPart ← (do
    let _ ← pchar '.'
    let frac ← many digit
    pure ("." ++ String.ofList frac.toList)) <|> pure ""
  pure (neg ++ intPart ++ fracPart)

/-- Map keyword string to token -/
def keywordOrIdent (s : String) : Token :=
  match s with
  | "strict" => .strict
  | "digraph" => .digraph
  | "graph" => .graph
  | "subgraph" => .subgraph
  | "node" => .node
  | "edge" => .edge
  | _ => .ident s

/-- Parse a single token -/
def nextToken : Parser Token := do
  skipWsAndComments
  -- Try EOF first
  (eof *> pure Token.eof) <|> do
    -- Peek at first character to decide what to parse
    let c ← satisfy fun _ => true
    match c with
    | '{' => pure .lbrace
    | '}' => pure .rbrace
    | '[' => pure .lbracket
    | ']' => pure .rbracket
    | '=' => pure .equals
    | ';' => pure .semicolon
    | ',' => pure .comma
    | ':' => pure .colon
    | '-' =>
      (pchar '>' *> pure .arrow) <|>
      (pchar '-' *> pure .dashdash) <|>
      (do  -- Negative number
        let digits ← many1 digit
        let intPart := String.ofList digits.toList
        let fracPart ← (do
          let _ ← pchar '.'
          let frac ← many digit
          pure ("." ++ String.ofList frac.toList)) <|> pure ""
        pure (.number ("-" ++ intPart ++ fracPart)))
    | '"' =>
      -- Already consumed opening quote, need special handling
      -- Actually we shouldn't have consumed it - let's use attempt
      fail "should not reach here"
    | _ =>
      if c.isDigit || c == '.' then
        -- Parse rest of number
        let rest ← many (satisfy fun c => c.isDigit || c == '.')
        pure (.number (c.toString ++ String.ofList rest.toList))
      else if isIdentStart c then
        -- Parse rest of identifier
        let rest ← many (satisfy isIdentCont)
        let id := c.toString ++ String.ofList rest.toList
        pure (keywordOrIdent id)
      else
        fail s!"unexpected character: {c}"

/-- Parse a single token (with better quoted string handling) -/
def nextTokenV2 : Parser Token := do
  skipWsAndComments
  (eof *> pure Token.eof)
  <|> (pchar '{' *> pure .lbrace)
  <|> (pchar '}' *> pure .rbrace)
  <|> (pchar '[' *> pure .lbracket)
  <|> (pchar ']' *> pure .rbracket)
  <|> (pchar '=' *> pure .equals)
  <|> (pchar ';' *> pure .semicolon)
  <|> (pchar ',' *> pure .comma)
  <|> (pchar ':' *> pure .colon)
  <|> (pstring "->" *> pure .arrow)
  <|> (pstring "--" *> pure .dashdash)
  <|> (do
        let s ← quotedString
        pure (.quotedString s))
  <|> (do
        let n ← numericLiteral
        pure (.number n))
  <|> (do
        let id ← unquotedIdent
        pure (keywordOrIdent id))
  <|> fail "unexpected input"

/-- Tokenize entire input -/
partial def tokenizeLoop (acc : List Token) : Parser (List Token) := do
  let tok ← nextTokenV2
  if tok == .eof then
    pure (acc.reverse ++ [tok])
  else
    tokenizeLoop (tok :: acc)

def tokenize : Parser (List Token) := tokenizeLoop []

/-- Run tokenizer on a string -/
def tokenizeString (input : String) : Except String (List Token) :=
  Parser.run tokenize input

end Dot4.Parser

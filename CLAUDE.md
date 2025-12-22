# Dot4 Development Notes

## Project Overview

Dot4 is a type-safe Graphviz DOT DSL for Lean 4, providing compile-time validation of graph definitions.

## Pending Work: RES-743 DOT Parser

### Goal
Implement `Graph.fromDot : String → Except String Graph` for round-trip support.

### DOT Grammar Reference
```
graph     : [ 'strict' ] ( 'graph' | 'digraph' ) [ ID ] '{' stmt_list '}'
stmt_list : [ stmt [ ';' ] stmt_list ]
stmt      : node_stmt | edge_stmt | attr_stmt | ID '=' ID | subgraph
attr_stmt : ( 'graph' | 'node' | 'edge' ) attr_list
attr_list : '[' [ a_list ] ']' [ attr_list ]
a_list    : ID '=' ID [ (';' | ',') ] [ a_list ]
edge_stmt : (node_id | subgraph) edgeRHS [ attr_list ]
edgeRHS   : edgeop (node_id | subgraph) [ edgeRHS ]
node_stmt : node_id [ attr_list ]
node_id   : ID [ port ]
port      : ':' ID [ ':' compass_pt ] | ':' compass_pt
subgraph  : [ 'subgraph' [ ID ] ] '{' stmt_list '}'
compass_pt: 'n' | 'ne' | 'e' | 'se' | 's' | 'sw' | 'w' | 'nw' | 'c' | '_'
ID        : alphanumeric | numeral | quoted_string | HTML_string
```

### Implementation Approach

**Phase 1: Core Parser (MVP)**
1. Tokenizer for DOT syntax
   - Identifiers (quoted and unquoted)
   - Keywords: `digraph`, `graph`, `subgraph`, `node`, `edge`, `strict`
   - Symbols: `{`, `}`, `[`, `]`, `=`, `;`, `,`, `->`, `--`, `:`
   - String literals (double-quoted, with escape handling)

2. Basic parser using Lean's `Parsec`
   ```lean
   import Lean.Data.Parsec
   open Lean Parsec

   def parseGraph : Parsec Graph := do
     let _ ← optWs
     let directed ← (pstring "digraph" *> pure true) <|> (pstring "graph" *> pure false)
     let _ ← ws
     let name ← parseId
     let _ ← optWs *> pchar '{'
     let stmts ← parseStmtList
     let _ ← optWs *> pchar '}'
     pure (buildGraph directed name stmts)
   ```

3. Statement parsing
   - Node statements: `ID [attrs]`
   - Edge statements: `ID -> ID [attrs]`
   - Attribute statements: `graph/node/edge [attrs]`
   - Subgraphs: `subgraph ID { ... }`

**Phase 2: Full Attribute Support**
- Parse all attribute key-value pairs
- Handle comma and semicolon separators
- Support for port syntax (`:port:compass`)

**Phase 3: Advanced Features**
- HTML labels (`<...>` syntax)
- Escape sequences in strings
- Numeric IDs
- Strict mode

### Key Files to Modify
- Create `Dot4/Parser.lean` for the parser implementation
- Export via `Dot4.lean`
- Add tests in `test/TestParser.lean`

### Testing Strategy
```lean
-- Round-trip test
#eval do
  let original := dot { digraph "Test"; node "A"; node "B"; edge "A" → "B" }
  let dotStr := original.toDot
  match Graph.fromDot dotStr with
  | .ok parsed =>
    -- Compare structure (order-independent)
    assert (parsed.getNodeIds.toFinset == original.getNodeIds.toFinset)
  | .error e => IO.println s!"Parse error: {e}"
```

### Useful References
- Graphviz DOT language spec: https://graphviz.org/doc/info/lang.html
- Lean Parsec docs: `Lean.Data.Parsec`
- Existing Dot4 structure in `Basic.lean`

### Considerations
- Error messages should include line/column info
- Consider streaming parser for large graphs
- HTML label parsing is complex (nested tags)
- Graph equality for testing needs to ignore attribute order

## Build Commands
```bash
lake build                    # Full build
lake build Dot4.Parser       # Build just parser module
lake exe dot4                # Run main executable
```

## Architecture Notes

### Current Modules
- `Basic.lean` - Core types (Graph, Node, Edge, Attr, Subgraph)
- `Syntax.lean` - DSL macro expansion
- `Elab.lean` - Compile-time validation
- `Render.lean` - DOT output generation
- `Validation.lean` - Graph algorithms and validation
- `Colors.lean`, `Shapes.lean` - Type-safe enums
- `Widget.lean` - VS Code visualization
- `Advanced.lean` - Graph transformations

### Type Structure
```lean
structure Graph where
  name : String
  directed : Bool
  strict : Bool
  nodes : List Node
  edges : List Edge
  subgraphs : List Subgraph
  attrs : List Attr
```

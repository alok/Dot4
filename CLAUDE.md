# Dot4 Development Notes

## Project Overview

Dot4 is a type-safe Graphviz DOT DSL for Lean 4, providing compile-time validation of graph definitions.

## Build Commands
```bash
lake build                    # Full build
lake build Dot4.Parser       # Build just parser module
lake exe dot4                # Run main executable
```

## Architecture Notes

### Current Modules
- `Basic.lean` - Core types (Graph, Node, Edge, Attr, Subgraph, SourceRange)
- `Syntax.lean` - DSL macro expansion
- `Elab.lean` - Compile-time validation
- `Render.lean` - DOT output generation
- `Validation.lean` - Graph algorithms and validation
- `Colors.lean`, `Shapes.lean` - Type-safe enums
- `Widget.lean` - VS Code visualization with go-to-definition support
- `Advanced.lean` - Graph transformations
- `Parser/` - DOT string parser (`Graph.fromDot` for round-trip)

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

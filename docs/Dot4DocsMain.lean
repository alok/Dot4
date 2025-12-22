/-
Dot4 Documentation Main Entry Point
-/

import VersoManual
import Dot4Docs

open Verso Doc
open Verso.Genre Manual

def config : Config where
  emitTeX := false
  emitHtmlSingle := false
  emitHtmlMulti := true
  htmlDepth := 2

def main := manualMain (%doc Dot4Docs) (config := config)

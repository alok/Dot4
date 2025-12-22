import Lake
open Lake DSL

package Dot4 where
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩,
    ⟨`autoImplicit, true⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩,
    ⟨`doc.verso, true⟩
  ]
  -- Disable reservoir fetching to force local builds
  preferReleaseBuild := false


require proofwidgets from git
  "https://github.com/leanprover-community/ProofWidgets4" @ "v0.0.83"

require hexluthor from git
  "https://github.com/alok/HexLuthor" @ "main"

lean_lib Dot4

@[default_target]
lean_exe dot4 where
  root := `Main

lean_exe «dot4-test» where
  root := `test.Tests

lean_exe «test-new-features» where
  root := `test.TestNewFeatures

lean_exe «test-parser» where
  root := `test.TestParser

-- Widget build configuration
def npmCmd : String :=
  if System.Platform.isWindows then "npm.cmd" else "npm"

target buildWidget (_pkg : NPackage __name__) : Unit := do
  let widgetDir := __dir__ / "widget"
  -- Install npm dependencies
  let installResult ← IO.Process.output {
    cwd := widgetDir
    cmd := npmCmd
    args := #["install", "--silent", "--no-progress"]
  }
  if installResult.exitCode != 0 then
    IO.eprintln s!"npm install failed: {installResult.stderr}"
    return .nil

  -- Build the widget
  let buildResult ← IO.Process.output {
    cwd := widgetDir
    cmd := npmCmd
    args := #["run", "build"]
  }
  if buildResult.exitCode != 0 then
    IO.eprintln s!"npm build failed: {buildResult.stderr}"
  return .nil

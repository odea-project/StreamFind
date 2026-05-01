# ASM Reading / Writing Slide Notes

## Slide Goal
- Show how ASM JSON files are read, inspected, validated, and written.
- Explain the split between generic JSON tooling and ASM-specific behavior.
- Highlight the Rcpp entry points used from R.

## Suggested Slide Message
- `src/json/` contains the generic JSON framework.
- `src/asm/` adds ASM-specific file layout, subtree access, and payload selection.
- Validation is schema-driven and uses a vendored validator library.
- The design avoids unnecessary duplication while keeping the ASM layer efficient.

## What to Show Visually
- A two-layer diagram:
  - top layer: `src/asm/`
  - bottom layer: `src/json/`
- A flow from JSON file -> index -> subtree access -> validation -> Rcpp output.
- A small example path such as `/weighing aggregate document/weighing document/0/...`.
- A split illustration showing:
  - generic JSON reading/writing
  - ASM-aware primary data extraction

## Possible Graphical Assets
- Folder icons for `src/json/` and `src/asm/`.
- Arrow diagram showing data flow from file to R.
- Tree or graph icon for SAX indexing.
- Document icon for JSON input and output.
- Shield/check icon for schema validation.
- Stack or layered blocks icon for the generic-vs-ASM separation.
- Small code callout boxes for:
  - `rcpp_json_read_file()`
  - `rcpp_json_read_subtree()`
  - `rcpp_asm_read_primary_data()`
  - `rcpp_asm_validate_file()`

## Short Speaker Notes
- The generic layer handles JSON parsing, pointer navigation, indexing, and validation.
- The ASM layer reuses the generic layer but adds model-aware path selection.
- SAX indexing gives a lightweight structure for navigating large files.
- Validation is separated from reading so files can be checked before deeper processing.

## Optional Slide Layout
1. Title at the top.
2. Left side: architecture diagram.
3. Right side: bullet list of exported functions.
4. Bottom: validation and tooling callouts.

## Example Export Groups
- Generic JSON:
  - `rcpp_json_read_file()`
  - `rcpp_json_read_subtree()`
  - `rcpp_json_list_children()`
- ASM-specific:
  - `rcpp_asm_read_file()`
  - `rcpp_asm_read_subtree()`
  - `rcpp_asm_read_primary_data()`
  - `rcpp_asm_list_children()`
  - `rcpp_asm_validate_file()`

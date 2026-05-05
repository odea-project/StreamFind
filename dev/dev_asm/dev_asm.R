library(StreamFind)

asm_root <- "E:/example_files/asm"
asm_data <- file.path(asm_root, "data")
asm_schemas <- file.path(asm_root, "schemas")

balance_json <- file.path(asm_data, "balance.json")
balance_schema <- file.path(asm_schemas, "balance.embed.schema.json")

lcms_json <- file.path(asm_data, "lc-ms.tabular.json")
lcms_schema <- file.path(asm_schemas, "lc-ms.tabular.embed.schema.json")

show_json <- function(x) {
  cat(jsonlite::prettify(x), "\n")
}

# Generic JSON usage -----------------------------------------------------------

# Read the full JSON document.
show_json(rcpp_json_read_file(balance_json))

# Read one subtree by JSON Pointer.
show_json(rcpp_json_read_subtree(balance_json, "/weighing aggregate document"))

# List child keys or array indices at a given path.
rcpp_json_list_children(balance_json, "/weighing aggregate document")

# Extract a single numeric value.
sample_weight <- jsonlite::fromJSON(
  rcpp_json_read_subtree(
    balance_json,
    "/weighing aggregate document/weighing document/0/measurement aggregate document/measurement document/0/sample weight/value"
  )
)
sample_weight

# ASM-specific usage -----------------------------------------------------------

# Read the full ASM file.
show_json(rcpp_asm_read_file(balance_json))

# Read an ASM subtree.
show_json(rcpp_asm_read_subtree(balance_json, "/weighing aggregate document/weighing document"))

# Inspect indexed child paths.
rcpp_asm_list_children(balance_json, "/weighing aggregate document/weighing document/0/measurement aggregate document")

# Inspect the full SAX index as a table.
asm_index <- rcpp_asm_index_table(balance_json)
print(head(asm_index, 10))

# Read the primary ASM payload selected by the ASM wrapper.
show_json(rcpp_asm_read_primary_data(balance_json))

# ASM validation ---------------------------------------------------------------

# Validate one ASM file against its schema bundle.
rcpp_asm_validate_file(balance_json, balance_schema, asm_schemas)
rcpp_asm_validate_file(lcms_json, lcms_schema, asm_schemas)

# Validate a small representative set of example files.
example_files <- c(
  "balance.json",
  "lc-ms.tabular.json",
  "gc-ms.tabular.json",
  "gas-chromatography.tabular.json"
)

validate_example <- function(file_name) {
  json_file <- file.path(asm_data, file_name)
  schema_file <- file.path(asm_schemas, sub("\\.json$", ".embed.schema.json", file_name))

  if (!file.exists(schema_file)) {
    message("Skipping missing schema: ", basename(schema_file))
    return(invisible(FALSE))
  }

  if (rcpp_asm_validate_file(json_file, schema_file, asm_schemas)) {
    message("Validated: ", file_name)
  } else {
    message("Validation failed: ", file_name)
  }
  invisible(NULL)
}

invisible(lapply(example_files, validate_example))

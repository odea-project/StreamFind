asm_root <- "E:/example_files/asm"
asm_data <- file.path(asm_root, "data")
asm_schemas <- file.path(asm_root, "schemas")
balance_json <- file.path(asm_data, "balance.json")
balance_schema <- file.path(asm_schemas, "balance.embed.schema.json")

# Generic JSON helpers work on any JSON file.
# - `rcpp_json_read_file()` loads and returns the full JSON document.
# - `rcpp_json_read_subtree()` extracts a JSON-pointer subtree.
# - `rcpp_json_list_children()` lists the immediate child keys or indices.
cat(jsonlite::prettify(rcpp_json_read_file(balance_json)))
cat(jsonlite::prettify(rcpp_json_read_subtree(balance_json, "/weighing aggregate document")))
rcpp_json_list_children(balance_json, "/weighing aggregate document")

# The ASM index can also be exposed as a table for inspection and slide visuals.
asm_index <- rcpp_asm_index_table(balance_json)
print(head(asm_index, 10))

sample_weight <- jsonlite::fromJSON(
  rcpp_json_read_subtree(
    balance_json,
    "/weighing aggregate document/weighing document/0/measurement aggregate document/measurement document/0/sample weight/value"
  )
)
sample_weight

# ASM helpers add index-aware navigation for the balance data layout.
# - `rcpp_asm_read_subtree()` reads a subtree using the ASM path layout.
# - `rcpp_asm_read_primary_data()` picks the main payload section.
# - `rcpp_asm_list_children()` shows indexed ASM child pointers.
cat(
  jsonlite::prettify(
    rcpp_asm_read_subtree(balance_json, "/weighing aggregate document")
  )
)

cat(
  jsonlite::prettify(
    rcpp_asm_read_subtree(balance_json, "/weighing aggregate document/weighing document")
  )
)

sample_weight <- jsonlite::fromJSON(
  rcpp_asm_read_subtree(
    balance_json,
    "/weighing aggregate document/weighing document/0/measurement aggregate document/measurement document/0/sample weight/value"
  )
)
sample_weight + 2

cat(
  jsonlite::prettify(
    rcpp_asm_read_subtree(balance_json, "/weighing aggregate document/device system document/device document")
  )
)

rcpp_asm_list_children(balance_json, "/weighing aggregate document/weighing document/0/measurement aggregate document")

# Pull the primary payload using the ASM-specific index.
cat(jsonlite::prettify(rcpp_asm_read_primary_data(balance_json)))

lcms_json <- file.path(asm_data, "lc-ms.tabular.json")
lcms_schema <- file.path(asm_schemas, "lc-ms.tabular.embed.schema.json")

# Same ASM helpers on another example.
rcpp_asm_validate_file(lcms_json, lcms_schema, asm_schemas)
rcpp_asm_read_primary_data(lcms_json)

examples <- c(
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

  rcpp_asm_validate_file(json_file, schema_file, asm_schemas)
  message("Validated: ", file_name)
  invisible(TRUE)
}

invisible(lapply(examples, validate_example))

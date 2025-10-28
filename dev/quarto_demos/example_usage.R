# Example usage of the report_quarto function

# Load your StreamFind package (assuming it's loaded)
# library(StreamFind)

# Basic MS Demo -----
dev_file_path <- "C:/Users/apoli/Documents/example_files"
dev_files <- list.files(dev_file_path, pattern = "mzML", full.names = TRUE)[1:3]
engine <- MassSpecEngine$new(
  metadata = list(
    name = "Example Analysis",
    author = "Your Name",
    date = Sys.time()
  ),
  analyses = dev_files
)
engine$report_quarto(
  template = "dev/quarto_demos/dev_basic_engine_html_doc.qmd",
  output_file = NULL
)

# LC-DAD Quantification ------
lcdad_engine <- MassSpecEngine$new(
  metadata = list(
    name = "Example Quantification with LC-DAD",
    author = "Ricardo Cunha",
    date = Sys.time()
  ),
  analyses = list.files("demo_data/lc_dad_quantification", pattern = "mzML", full.names = TRUE)
)

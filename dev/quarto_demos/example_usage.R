# Example usage of the report_quarto function

# Load your StreamFind package (assuming it's loaded)
# library(StreamFind)

# Create or load an engine
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

# Add some data, run some analyses, etc.
# engine$Analyses <- your_analyses
# engine$run(your_processing_step)
                                                                        
# Generate the report
# engine$report_quarto(
#   template = "example_report_template.qmd",
#   output_file = "my_streamfind_report.html",
#   output_dir = "reports/"
# )

# Alternative with additional quarto options
# engine$report_quarto(
#   template = "example_report_template.qmd",
#   output_file = "detailed_report.html",
#   execute = TRUE,
#   cache = FALSE
# )
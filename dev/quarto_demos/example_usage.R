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

lcdad_engine <- MassSpecEngine$new()
lcdad_engine$load("lcdada_quant.rds")


names(lcdad_engine$Results)
names(lcdad_engine$Results[[1]])
# create a app_mod_WorkflowAssembler_MassSpecResults_Chromatograms.R file with a module for displaying results of chromatograms and a data.table with peraks (if available) similar as implemented in


# LC-HRMS Identification -----
ident_engine <- MassSpecEngine$new(
  metadata = list(
    name = "Example Intact Monoclonal Antibody Identification with LC-HRMS",
    author = "Ricardo Cunha",
    date = Sys.time()
  ),
  analyses = list.files("demo_data/lc_dad_quantification", pattern = "mzML", full.names = TRUE)[c(6:8, 16:18)]
)

ident_engine$save("ident_engine.rds")

ident_engine <- MassSpecEngine$new()
ident_engine$load("ident_engine.rds")

names(ident_engine$Analyses$results)
names(ident_engine$Analyses$results[[1]]$peaks)





info()

run_app()

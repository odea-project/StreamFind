# Example usage of the report_quarto function

# Load your StreamFind package (assuming it's loaded)
# library(StreamFind)

# Basic MS Demo -----
# dev_file_path <- "C:/Users/apoli/Documents/example_files"
# dev_files <- list.files(dev_file_path, pattern = "mzML", full.names = TRUE)[1:3]
# engine <- MassSpecEngine$new(
#   metadata = list(
#     name = "Example Analysis",
#     author = "Your Name",
#     date = Sys.time()
#   ),
#   analyses = dev_files
# )

# engine$report_quarto(
#   template = "dev/quarto_demos/dev_basic_engine_html_doc.qmd",
#   output_file = NULL
# )

# LC-DAD Quantification ------
# lcdad_engine <- MassSpecEngine$new(
#   metadata = list(
#     name = "Example Quantification with LC-DAD",
#     author = "Ricardo Cunha",
#     date = Sys.time()
#   ),
#   analyses = list.files("demo_data/lc_dad_quantification", pattern = "mzML", full.names = TRUE),
#   workflow = "demo_data/demo_lc_dad_quantification_workflow.json"
# )

# lcdad_engine$save("lcdada_quant.rds")

# library(StreamFind)
# lcdad_engine <- MassSpecEngine$new()
# lcdad_engine$load("lcdada_quant.rds")


# create a app_mod_WorkflowAssembler_MassSpecResults_Chromatograms.R file with a module for displaying results of chromatograms and a data.table with peraks (if available) similar as implemented in


# LC-HRMS Identification -----
# ident_engine <- MassSpecEngine$new(
#   metadata = list(
#     name = "Example Intact Monoclonal Antibody Identification with LC-HRMS",
#     author = "Ricardo Cunha",
#     date = Sys.time()
#   ),
#   analyses = list.files("demo_data/lc_dad_quantification", pattern = "mzML", full.names = TRUE)[c(6:8, 16:18)]
# )

# ident_engine$save("ident_engine.rds")
# ident_engine <- MassSpecEngine$new()
# ident_engine$load("ident_engine.rds")

# Raman Spectroscopy -----
# raman_engine <- RamanEngine$new(
#   metadata = list(
#     name = "Example Raman Spectroscopy Analysis",
#     author = "Ricardo Cunha",
#     date = Sys.time()
#   ),
#   analyses = list.files("demo_data/raman_quality_evaluation", pattern = "sif", full.names = TRUE),
#   workflow = "demo_data/demo_raman_quality_evaluation_workflow.json"
# )

# raman_engine$save("raman_engine.rds")
# raman_engine <- RamanEngine$new()
# raman_engine$load("raman_engine.rds")


# LC-HRMS NTS -----
library(StreamFind)
nts_engine <- MassSpecEngine$new(
  metadata = list(
    name = "Example LC-HRMS Non-Targeted Screening Analysis",
    author = "Ricardo Cunha",
    date = Sys.time()
  ),
  analyses = list.files("demo_data/data/lc_hrms_nts", pattern = "mzML", full.names = TRUE),
  workflow = "demo_data/workflows/lc_hrms_nts_workflow_with_tp_generation.json"
)

nts_engine$save("nts_engine.rds")

nts_engine <- MassSpecEngine$new()
nts_engine$load("demos/nts_engine.rds")
nts_engine$Workflow <- "demos/workflows/demo_lc_hrms_nts_openms_biotransformer.json"

# nts_engine$Workflow[[18]] <- MassSpecMethod_GenerateTransformationProducts_cts(
#   parents = data.table::data.table(
#     name = "Gabapentine",
#     SMILES = "C1CCC(CC1)(CC(=O)O)CN"
#   ),
#   use_suspects = FALSE,
#   use_compounds = FALSE,
#   transLibrary = "combined_photolysis_abiotic_hydrolysis",
#   generations = 3,
#   errorRetries = 5,
#   skipInvalid = TRUE,
#   prefCalcChemProps = TRUE,
#   neutralChemProps = TRUE,
#   neutralizeTPs = TRUE,
#   calcLogP = "rcdk",
#   calcSims = TRUE,
#   fpType = "extended",
#   fpSimMethod = "tanimoto",
#   parallel = FALSE
# )

show(nts_engine$Workflow)
nts_engine$run_workflow()

engine <- nts_engine$clone()

run(MassSpecMethod_GenerateTransformationProducts_cts(
  parents = data.table::data.table(
    name = "Gabapentine",
    SMILES = "C1CCC(CC1)(CC(=O)O)CN"
  ),
  use_suspects = FALSE,
  use_compounds = FALSE,
  transLibrary = "combined_photolysis_abiotic_hydrolysis",
  generations = 3,
  errorRetries = 5,
  skipInvalid = TRUE,
  prefCalcChemProps = FALSE,
  neutralChemProps = FALSE,
  neutralizeTPs = TRUE,
  calcLogP = "rcdk",
  calcSims = FALSE,
  fpType = "extended",
  fpSimMethod = "tanimoto",
  parallel = FALSE
), nts_engine)

# 747.4769 plus oxagen addition

unique(get_suspects(nts_engine$MassSpecResults_NonTargetAnalysis)[, c("name", "SMILES")])[5, ]
names(engine$Results$MassSpecResults_TransformationProducts$transformation_products)
lapply(engine$Results$MassSpecResults_TransformationProducts$transformation_products, nrow)
engine$Results$MassSpecResults_TransformationProducts$transformation_products[[13]]

plot_transformation_products_network(
  engine$Results$MassSpecResults_TransformationProducts,
  parents = "Tramadol",
  parentsReplicate = "influent",
  productsReplicate = "effluent"
)

engine$Results$MassSpecResults_TransformationProducts$parents
unique(get_suspects(nts_engine$MassSpecResults_NonTargetAnalysis)[, c("name", "SMILES")])
get_groups(nts_engine$MassSpecResults_NonTargetAnalysis, groups = "M250_R509_8661")
StreamFind::run_app()

readRDS("C:/Users/apoli/Documents/demos/demo_02/nts_engine_res.rds")

engine <- MassSpecEngine$new()
engine$load("C:/Users/apoli/Documents/demos/demo_02/nts_engine_res.rds")
engine$run_app()

#' @title Create a suspect screening CSV template
#' @description Writes a template CSV for suspect screening with the required
#' columns and example rows.
#' @param file Character (length 1). Output CSV file path.
#' @return Invisibly returns the file path.
#' @export
#'
#' @examples
#' \dontrun{
#' get_template_suspect_screening_csv(file = "suspects_template.csv")
#' }
get_template_suspect_screening_csv <- function(file) {
  checkmate::assert_character(file, len = 1)

  cols <- c(
    "name", "formula", "mass", "rt", "SMILES", "InChI", "InChIKey", "xLogP",
    "ms2_positive", "ms2_negative"
  )
  tpl <- data.table::data.table(matrix(nrow = 0, ncol = length(cols)))
  data.table::setnames(tpl, cols)

  tpl <- data.table::rbindlist(list(
    tpl,
    data.table::data.table(
      name = "4N-Acetylsulfadiazine",
      formula = "C12H12N4O3S",
      mass = 292.063011244,
      rt = 905,
      SMILES = "O=C(NC1=CC=C(C=C1)S(=O)(=O)NC2=NC=CC=N2)C",
      InChI = "InChI=1S/C12H12N4O3S/c1-9(17)15-10-3-5-11(6-4-10)20(18,19)16-12-13-7-2-8-14-12/h2-8H,1H3,(H,15,17)(H,13,14,16)",
      InChIKey = "NJIZUWGMNCUKGU-UHFFFAOYSA-N",
      xLogP = -1.394,
      ms2_positive = "43.01817 198; 67.02847 149; 77.0392 161; 79.0528 198; 80.04926 240; 92.04987 465; 93.03298 392; 93.05672 188; 94.03891 196; 94.06343 147; 96.05484 731; 106.06538 153; 108.04403 1185; 118.06546 260; 134.06009 748; 136.07448 809; 156.00804 178; 198.02271 842; 227.09206 362; 293.06976 2114",
      ms2_negative = ""
    ),
    data.table::data.table(
      name = "Metoprolol",
      formula = "C15H25NO3",
      mass = 267.18344366,
      rt = 915,
      SMILES = "OC(COC1=CC=C(C=C1)CCOC)CNC(C)C",
      InChI = "InChI=1S/C15H25NO3/c1-12(2)16-10-14(17)11-19-15-6-4-13(5-7-15)8-9-18-3/h4-7,12,14,16-17H,8-11H2,1-3H3",
      InChIKey = "IUBSYMUCCVWXPE-UHFFFAOYSA-N",
      xLogP = 0.702,
      ms2_positive = "41.03796 409; 44.04892 164; 55.01881 190; 56.04974 3540; 72.08056 1546; 74.06022 1468; 79.05399 454; 86.0965 172; 91.05418 768; 98.09674 612; 103.05396 989; 105.07011 889; 115.05419 212; 116.10728 1267; 121.06458 802; 131.04967 167; 132.05734 181; 133.06448 649; 141.06825 223; 165.09148 133; 176.10712 319; 191.1057 746; 218.15479 200; 226.14108 245; 226.14691 254; 250.1799 374; 268.19141 7121",
      ms2_negative = ""
    )
  ), fill = TRUE)

  data.table::fwrite(tpl, file)
  invisible(file)
}

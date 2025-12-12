
library(data.table)
all_files_dir <- "C:\\Users\\apoli\\Documents\\example_files\\peak_finding_files_ex"
#all_files_dir <- "D:\\peak_finding_files_ex"

all_files <- list.files(all_files_dir, full.names = TRUE, recursive = TRUE)

files_merck <- all_files[grepl("merck/Beispieldaten Routine", all_files) & grepl("\\.mzML$", all_files)]
cvs_merck <- all_files[grepl("merck/Beispieldaten Routine", all_files) & grepl("\\.csv$", all_files)]

files_merck_2 <- all_files[grepl("merck/More Data OLED Routine for Ricardo", all_files) & grepl("\\.mzML$", all_files)]
cvs_merck_2 <- all_files[grepl("merck/More Data OLED Routine for Ricardo", all_files) & grepl("\\.csv$", all_files)]
acc_ids_merck_2 <- sub(".*(ACC1_\\d+).*", "\\1", basename(cvs_merck_2))

merck_ex1 <- "ACC1_26393"
files_merck_ex1 <- files_merck[grepl(merck_ex1, files_merck)]
files_merck_ex1_centroid <- files_merck[grepl(paste0("centroid_", merck_ex1), files_merck)]
db_merck_ex1 <- fread(cvs_merck[grepl(merck_ex1, cvs_merck)], encoding = "UTF-8")
db_merck_ex1[] <- lapply(db_merck_ex1, function(x) if (is.character(x)) iconv(x, from = "", to = "UTF-8", sub = "") else x)
db_merck_ex1$name <- paste0("Peak ", db_merck_ex1$MW, " ", db_merck_ex1$rt)
db_merck_ex1$example <- merck_ex1
setcolorder(db_merck_ex1, c("example", "name", "MW", "rt", "mz", "formula", "ppm"))

merck_ex2 <- "ACC1_25777"
files_merck_ex2 <- files_merck[grepl(merck_ex2, files_merck)]
files_merck_ex2_centroid <- files_merck[grepl(paste0("centroid_", merck_ex2), files_merck)]
db_merck_ex2 <- fread(cvs_merck[grepl(merck_ex2, cvs_merck)], encoding = "UTF-8")
db_merck_ex2[] <- lapply(db_merck_ex2, function(x) if (is.character(x)) iconv(x, from = "", to = "UTF-8", sub = "") else x)
db_merck_ex2$name <- paste0("Peak ", db_merck_ex2$MW, " ", db_merck_ex2$rt)
db_merck_ex2$example <- merck_ex2
setcolorder(db_merck_ex2, c("example", "name", "MW", "rt", "mz", "formula", "ppm"))

merck_ex3 <- "ACC1_28127"
files_merck_ex3 <- files_merck[grepl(merck_ex3, files_merck)]
files_merck_ex3_centroid <- files_merck[grepl(paste0("centroid_", merck_ex3), files_merck)]
db_merck_ex3 <- fread(cvs_merck[grepl(merck_ex3, cvs_merck)], encoding = "UTF-8")
db_merck_ex3[] <- lapply(db_merck_ex3, function(x) if (is.character(x)) iconv(x, from = "", to = "UTF-8", sub = "") else x)
db_merck_ex3$name <- paste0("Peak ", db_merck_ex3$MW, " ", db_merck_ex3$rt)
db_merck_ex3$example <- merck_ex3
setcolorder(db_merck_ex3, c("example", "name", "MW", "rt", "mz", "formula", "ppm"))

merck_ex4 <- "ACC1_28142"
files_merck_ex4 <- files_merck[grepl(merck_ex4, files_merck)]
files_merck_ex4_centroid <- files_merck[grepl(paste0("centroid_", merck_ex4), files_merck)]
db_merck_ex4 <- fread(cvs_merck[grepl(merck_ex4, cvs_merck)], encoding = "UTF-8")
db_merck_ex4[] <- lapply(db_merck_ex4, function(x) if (is.character(x)) iconv(x, from = "", to = "UTF-8", sub = "") else x)
db_merck_ex4$name <- paste0("Peak ", db_merck_ex4$MW, " ", db_merck_ex4$rt)
db_merck_ex4$example <- merck_ex4
setcolorder(db_merck_ex4, c("example", "name", "MW", "rt", "mz", "formula", "ppm"))

files_merck_ex <- data.table(
  example = rep(c(merck_ex1, merck_ex2, merck_ex3, merck_ex4), each = 2),
  blank = FALSE,
  file_name = c(
    basename(files_merck_ex1_centroid),
    basename(files_merck_ex2_centroid),
    basename(files_merck_ex3_centroid),
    basename(files_merck_ex4_centroid)
  ),
  file_path = c(files_merck_ex1_centroid, files_merck_ex2_centroid, files_merck_ex3_centroid, files_merck_ex4_centroid)
)
files_merck_ex$blank <- grepl("blank", files_merck_ex$file_name)

db_merck_ex <- rbindlist(list(db_merck_ex1, db_merck_ex2, db_merck_ex3, db_merck_ex4))

merck2_ex <- lapply(acc_ids_merck_2, function(acc_id) {
  files_ex <- files_merck_2[grepl(acc_id, files_merck_2)]
  db_ex <- fread(cvs_merck_2[grepl(acc_id, cvs_merck_2)], encoding = "UTF-8")
  db_ex[] <- lapply(db_ex, function(x) if (is.character(x)) iconv(x, from = "", to = "UTF-8", sub = "") else x)
  db_ex$name <- paste0("Peak ", db_ex$MW, " ", db_ex$rt)
  db_ex$example <- acc_id
  setcolorder(db_ex, c("example", "name", "MW", "rt", "mz", "formula", "ppm"))
  
  files_dt <- data.table(
    example = rep(acc_id, length(files_ex)),
    blank = FALSE,
    file_name = basename(files_ex),
    file_path = files_ex
  )
  files_dt$blank <- grepl("blank", files_dt$file_name)
  
  list(files = files_dt, db = db_ex)
})

merck2_files_ex <- rbindlist(lapply(merck2_ex, function(x) x$files))
merck2_db_ex <- rbindlist(lapply(merck2_ex, function(x) x$db))

# files_tof_cent <- all_files[grepl("tof_centroid", all_files)]
# files_tof_cent <- files_tof_cent[!grepl("wastewater", files_tof_cent)]
# files_tof_prof <- all_files[grepl("tof_profile", all_files)]
# files_orb_cent <- all_files[grepl("orbitrap_centroid", all_files)]
# files_orb_prof <- all_files[grepl("orbitrap_profile", all_files)]
# files_tof_ww <- all_files[grepl("wastewater", all_files)]

# files <- c(
#   files_tof_cent[1],
#   files_tof_prof[1],
#   files_orb_cent[7],
#   files_orb_prof[7],
#   files_tof_ww[11],
#   #files_merck_ex[1:2],
#   files_merck_ex_centroid[1:2]
# )

# db_all <- StreamFindData::get_ms_tof_spiked_chemicals()
# db_all <- db_all[grepl("S", db_all$tag), ]
# cols <- c("name", "formula", "mass", "rt", "tag")
# db_is <- db_all[db_all$tag %in% "IS", ]
# db_is <- db_is[, cols, with = FALSE]
# db_is <- db_is[!db_is$name %in% c("Ibuprofen-d3", "Naproxen-d3"), ]
# db_is_with_mz <- data.table::copy(db_is)
# db_is_with_mz$mz <- db_is_with_mz$mass + 1.007276
# db_is_with_mz$mass <- NULL
# db <- db_all[db_all$tag %in% "S", ]
# db <- db[, cols, with = FALSE]
# db_with_mz <- data.table::copy(db)
# db_with_mz$mz <- db_with_mz$mass + 1.007276
# db_with_mz$mass <- NULL
# db_with_ms2 <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
# db_with_ms2 <- db_with_ms2[db_with_ms2$tag %in% "S", ]
# db_with_ms2 <- db_with_ms2[, c("name", "formula", "mass", "SMILES", "rt", "polarity", "fragments"), with = FALSE]
# db_with_ms2$polarity[db_with_ms2$polarity == 1] <- "positive"
# db_with_ms2$polarity[is.na(db_with_ms2$polarity)] <- "positive"
# db_with_ms2$polarity[db_with_ms2$polarity == -1] <- "negative"

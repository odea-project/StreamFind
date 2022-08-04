
### Example file paths --------------------------------------------------------------------------------------

dir1 <- "C:\\Users\\Ricardo\\Documents\\R_NTS_article\\msfiles"
files1 <- list.files(dir1, full.names = TRUE)

dir2 <- "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles"
files2 <- list.files(dir2, full.names = TRUE)

fl <- files1[1:3]
files <- fl


### RaMS ---------------------

library(RaMS)

fl <- choose.files()

fl <- c(
  "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles\\09_Sample_Sciex_MRM_Chromatograms_Nitrosamines_10ngml.mzML",
  "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles\\10_Sample_Sciex_MRM_Spectra_Nitrosamines_10ngml.mzML"
)

test <- xml2::as_list(xml2::read_xml(fl[1]))
xml2::xml_structure(test, indent = 1)


test$indexedmzML$mzML$run$chromatogramList[[3]]$binaryDataArrayList

test <- grabMSdata(file = fl[1], grab_what = "TIC")

msdata <- grabMSdata(files = fl[2], grab_what = "MS1") #rtrange = c(1100/60, 1200/60)

data_nodes <- xml2::xml_find_all(files[1])



targets4



files



test <- mzR::openMSfile(fl[1])

test_chrom <- chromatogramHeader(test)
test_chrom <- chromatograms(test)

test <- mzR::close(test)


### Classes Hierarchy ---------------------------------------------------------------------------------------

test <- new("msAnalysis", file = fl[1])




test1 <- new("msData")

test1@analyses <- list(
  a = new("msAnalysis", file = files1[1]),
  b = new("msAnalysis", file = files1[1])
)

analyses(test1)

test2 <- new("msmsData", test1)

is(test1)










sapply(test1@samples, function(x) x@replicate)

rpl <- rep("control", 2)
names(rpl) <- sapply(test1@samples, function(x) x@name)



test1@samples <- lapply(test1@samples, function(x, rpl) {

  rpl_n <- NA

  if (x@name %in% names(rpl)) {
    rpl_n <- rpl[x@name]
  }

  if (!is.na(rpl_n)) x@replicate <- rpl_n

  return(x)

}, rpl = rpl)

sp1 <- test1@samples[[1]]
mt <- sp1@otherInfo






summary(lapply(test1@samples, function(x) x@scans))

test

msd <- newStreamProject(files = fl)

is(msd)

sampleNames(msd)

data.table::rbindlist(msd@samples[[1]])


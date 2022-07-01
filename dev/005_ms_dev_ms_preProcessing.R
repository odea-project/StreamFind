
### Example file paths --------------------------------------------------------------------------------------

dir1 <- "C:\\Users\\Ricardo\\Documents\\R_NTS_article\\msfiles"
files1 <- list.files(dir1, full.names = TRUE)

dir2 <- "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles"
files2 <- list.files(dir2, full.names = TRUE)

fl <- files1[1:3]
files <- fl

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


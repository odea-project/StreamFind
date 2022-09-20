
# load files
files <- list.files(choose.dir(), full.names = TRUE)

fl <- files[10]

a1 <- newStreamSet(file = files[1])
a1 <- loadRawData(a1)


a1 <- newAnalysis(file = files[1])




streamFind:::mzML_loadMetadata(fl)

streamFind:::mzML_loadMetadata(fl)






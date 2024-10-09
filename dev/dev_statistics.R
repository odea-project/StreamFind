

data <- data.table::fread("E:/Dev_actuals_ms_statistic/221227_MSScr6500plus.csv")
for (i in 3:ncol(data)) data[[i]] <- as.numeric(data[[i]])
if (any(is.na(data))) {
  message("There are missing values in the data!")
  data <- data[complete.cases(data), ]
}
analyses_names <- as.character(paste0(data$Date, " ", data$Time))
data <- as.data.frame(data[, -c(1, 2)])
rownames(data) <- analyses_names

st <- StreamFind::StatisticEngine$new(data = data[1:100, ])
st$process(StatisticSettings_PrepareData_autoscale())
st$plot_data(analyses = 1:100, transpose = TRUE, interactive = TRUE)
st$process(StatisticSettings_MakeModel_pca_mdatools())
st$plot_model_scores()
st$plot_model_loadings()


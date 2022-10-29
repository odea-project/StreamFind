
foldChangePrototype <- function(fts, IN, OUT, filtered = FALSE, 
                                constantLevel = 3000, noise = 500) {

  temp <- data.table::copy(fts)
  
  cols_basic <- c("id", "index", "rt", "mz", "filtered", "filter")
  col_in <- IN
  col_out <- OUT
  
  cols_keep <- c(cols_basic, col_in, col_out)
  
  temp <- temp[, ..cols_keep]
  
  if (!filtered) {
    temp <- temp[!temp$filtered, ]
  }
  
  temp <- temp[!(unlist(temp[, ..col_out]) == 0 & unlist(temp[, ..col_in]) == 0), ]
  
  temp$fold <-  unlist(temp[, ..col_in]) - unlist(temp[, ..col_out])
  
  temp$cat <- NA_character_
  
  temp[fold > constantLevel, cat := "L"]
  temp[fold < -constantLevel, cat := "H"]
  temp[unlist(temp[, ..col_in]) <= noise, cat := "N"]
  temp[unlist(temp[, ..col_out]) <= noise, cat := "R"]
  temp[fold <= constantLevel &
          fold >= -constantLevel & 
              unlist(temp[, ..col_in]) > noise &
                  unlist(temp[, ..col_out]) > noise, cat := "C"]
  
  temp$constantLevel <- constantLevel
  
  temp$step <- col_out #paste0(col_in, "_TO_", col_out)
  
  temp$cat <- factor(temp$cat, levels = c("N","H", "C", "L","R"),
    labels = c("New", "Higher", "Constant", "Lower","Removed"))
  
  temp <- dplyr::select(temp, step, dplyr::everything())
  
  return(data.table::copy(temp))
}


plotEfficiencyPrototype <- function(temp, title = NULL, xaxis = NULL) {

  steps <- unique(temp$step)
  n_steps <- length(steps)
  
  if (n_steps > 1) {
    temp_melt <- split.data.frame(temp, temp$step)
    temp_melt <- lapply(temp_melt, function(x) {
      temp_2 <- x %>% dplyr::count(cat)
      temp_2 <- reshape2::melt(temp_2, id.vars = "cat")
      temp_2$variable <- unique(x$step)
      temp_2$value <- (temp_2$value / sum(temp_2$value)) * 100
      return(temp_2)
    })
    temp_melt <- data.table::rbindlist(temp_melt)
  } else{
    temp_melt <- temp %>% dplyr::count(cat)
    temp_melt <- reshape2::melt(temp_melt, id.vars = "cat")
    temp_melt$variable <- unique(temp$step)
    temp_melt$value <- (temp_melt$value / sum(temp_melt$value)) * 100
  }
  
  plot_eff <- ggplot2::ggplot(data = temp_melt) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(mapping = ggplot2::aes(x = variable, y = value, fill = cat), 
                      stat = "identity", width = 0.4, alpha = 1/1.3) +
    #ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    #ggplot2::theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
    ggplot2::scale_fill_manual(name = "Category", drop = FALSE,
      values = c(
        "Constant" = "#87CEFA",
        "Higher" = "#FFA500",
        "Lower" = "#90EE90",
        "Removed" = "#006400",
        "New" = "#8B0000"
      )
    ) +
    ggplot2::ylab("Features (%)") +
    ggplot2::xlab(xaxis) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Constant level threshold set at ", unique(temp$constantLevel), " counts")) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8))
  
  return(plot_eff)
}

summaryEfficiencyPrototype <- function(temp) {
  
  steps <- unique(temp$step)
  n_steps <- length(steps)
  
  if (n_steps > 1) {
    temp_melt <- split.data.frame(temp, temp$step)
    temp_melt <- lapply(temp_melt, function(x) {
      temp_2 <- x %>% dplyr::count(cat)
      temp_2 <- reshape2::melt(temp_2, id.vars = "cat")
      temp_2$variable <- unique(x$step)
      temp_2$value_perc <- (temp_2$value / sum(temp_2$value)) * 100
      return(temp_2)
    })
    temp_melt <- data.table::rbindlist(temp_melt)
  } else{
    temp_melt <- temp %>% dplyr::count(cat)
    temp_melt <- reshape2::melt(temp_melt, id.vars = "cat")
    temp_melt$variable <- unique(temp$step)
    temp_melt$value_perc <- (temp_melt$value / sum(temp_melt$value)) * 100
  }
  
  return(temp_melt)
}


plotFoldChangePrototype <- function(temp, yUnit = "mz", size = NULL, title = NULL) {
  
  step <- data.table::copy(temp)
  
  if (is.null(title)) title <- unique(step$step)
  
  step$fold_plot <- 0
  step <- dplyr::mutate(step, fold_plot = fold/10^5)
  step <- dplyr::mutate(step, fold_plot = ifelse(fold_plot > 2, 2.25, fold_plot))
  step <- dplyr::mutate(step, fold_plot = ifelse(fold_plot < -2, -2.25, fold_plot))
  
  if (is.null(size)) size <- 1
  
  if (yUnit == "rt") step$mz <- step$rt
  
  plot <- ggplot2::ggplot(data = step) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = -fold_plot, y = mz, colour = cat), size = size, alpha = 1/1.2) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_line(colour = "#C0C0C0", size = 0.1),
          legend.position =  "bottom",
          legend.background = ggplot2::element_blank(),
          legend.direction = "horizontal") +
    ggplot2::labs(title = title,
         subtitle = paste0("Constant level threshold set at ", unique(step$constantLevel), " counts"),
         colour = "Category") +
    ggplot2::xlab(expression("Intensity ( In - Out )  x10"^"5")) +
    ggplot2::ylab(ifelse(yUnit == "mz", expression(italic("m/z")), expression("Retention time (sec)"))) +
    ggplot2::scale_color_manual(
      values = c(
        "Constant" = "#87CEFA",
        "Higher" = "#FFA500",
        "Lower" = "#90EE90",
        "Removed" = "#006400",
        "New" = "#8B0000"
      )
    ) +
    ggplot2::scale_x_continuous(breaks = seq(-2, 2, 1)) +
    ggplot2::expand_limits(x = c(-2.5, 2.5)) +
    ggplot2::geom_vline(xintercept = -2, size = 1) +   
    ggplot2::geom_vline(xintercept = -constantLevel/10^5, size = 0.5) +
    ggplot2::geom_vline(xintercept = constantLevel/10^5, size = 0.5) +
    ggplot2::geom_vline(xintercept = 2, size = 1)
  
  return(plot)
  
}







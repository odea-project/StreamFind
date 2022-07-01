

#' @title getColors
#'
#' @description Function to produce colors for a character vector.
#'
#' @param obj A character vector to associate with the colors.
#'
#' @return A vector of colors. The vector is named according the \code{obj}.
#'
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr count
#'
getColors <- function(obj) {

  colors <- c(brewer.pal(8, "Greys")[6],
              brewer.pal(8, "Greens")[6],
              brewer.pal(8, "Blues")[6],
              brewer.pal(8, "Oranges")[6],
              brewer.pal(8, "Purples")[6],
              brewer.pal(8, "PuRd")[6],
              brewer.pal(8, "YlOrRd")[6],
              brewer.pal(8, "PuBuGn")[6],
              brewer.pal(8, "GnBu")[6],
              brewer.pal(8, "BuPu")[6],
              brewer.pal(8, "Dark2"))

  Ncol <- length(unique(obj))

  if (Ncol > 18) {
    colors <- colorRampPalette(colors)(Ncol)
  }

  if (length(unique(obj)) < length(obj)) {
    Vcol <- colors[seq_len(Ncol)]
    Ncol <- length(obj)
    count <- dplyr::count(data.frame(n = seq_len(Ncol), char = obj), char)
    Vcol <- rep(Vcol, times = count[, "n"])
    names(Vcol) <- obj
  } else {
    Vcol <- colors[seq_len(Ncol)]
    names(Vcol) <- obj
  }

  return(Vcol)
}

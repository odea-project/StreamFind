#logo header
sourcepath <- system.file(package = "streamFindData", dir = "extdata")
logo <- file.path(sourcepath, "logo_iuta.png")
width <- 100
height <- width*(216/207)
img <- htmltools::img(
  src = knitr::image_uri(logo),
  alt = "logo",
  style =
    paste0("position:absolute;
      top:6%;
      margin-left:inherit;
      padding-right:1020px;
      width:", 100, "px;
      height:", 100*(216/207), "px;"
    )
)

htmlhead <- paste0('<script> document.write(\'<div class = "logo">', img, '</div>\') </script> ')
#projectpath <- "C:\\Users\\MZmine\\Documents\\R Projects\\streamFindData\\inst\\extdata"
projectpath <- "/home/ricardo/Documents/Projects_r/streamFindData"
readr::write_lines(htmlhead, file = paste0(projectpath, "/logo_iuta_template_report.html"))
readr::write_lines(htmlhead, file = paste0(sourcepath, "/logo_iuta_template_report.html"))

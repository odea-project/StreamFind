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
      margin-left:90%;
      width:", width, "px;
      height:", height, "px;"
    )
)

htmlhead <- paste0('<script> document.write(\'<div class = "logo">', img, '</div>\') </script> ')
projectpath <- "C:\\Users\\MZmine\\Documents\\R Projects\\streamFindData\\inst\\extdata"
readr::write_lines(htmlhead, file = paste0(projectpath, "\\logo_iuta_template_report.html"))
readr::write_lines(htmlhead, file = paste0(sourcepath, "\\logo_iuta_template_report.html"))

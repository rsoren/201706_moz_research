#
# save_as_markdown.R
#
# Reed Sorensen
# August 2017
#
# Function for saving an R script as R Markdown and HTML files
# 

save_as_markdown <- function(relative_path) {
  
  require(knitr)
  
  wd_sav <- paste0(getwd(), '/')
  
  try({
    setwd(dirname(paste0(wd_sav, relative_path)))
    knitr::spin(paste0(wd_sav, relative_path))
  })
  
  setwd(wd_sav)
}


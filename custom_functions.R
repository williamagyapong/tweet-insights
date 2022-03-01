#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose: Stores custom functions                                             #
# Author: William Ofosu Agyapong
# Created: Feb 26, 2022
# Last updated: Feb 26, 2022                                                   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


css_dep <- function() {
  htmltools::htmlDependency(
    name = "custom-css",
    version = "1.0",
    src = c(href = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/www/")),
    stylesheet = "custom.css"
  )
}

#----- load custom css codes
# tags$head(
#   tags$link(
#     rel = "stylesheet",
#     type = "text/css",
#     href = "www/custom.css"
#   )
# ),

internal_css <- function() {
  return(
    ".content {

      }
      .footer {

      }
    "
  )
}

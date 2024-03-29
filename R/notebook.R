#' Convert ipynb to Rmarkdown or R script 
#' 
#' Just a wrapper for rmarkdown and knitr
#' 
#' @param file path of input
#' @param to R or Rmd
#' @export
py2r <- function(file, name = 'name', ext = "R",...) 
{
  filepth <- paste0(tempdir(), name, ext)
  if (ext == 'Rmd')
    rmarkdown:::convert_ipynb(file, filepth)
  if (ext == 'R')
    knitr::purl(file, documentation = 2)
}

#' Display HTML with common options
#' 
#' @param x a data frame
#' @param ... options to DT::datatable
dttb <- function(x,...) {
  DT::datatable(
    x, 
    extensions = c('Buttons', 'FixedColumns'),
    options = list(
      dom = c('Bfrtip'),
      scrollX = TRUE,
      fixedColumns = TRUE, 
      buttons = c('csv', 'excel')
    ),
    ...
  )
}
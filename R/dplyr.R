#' Plot a 2D matrix as lines
#'
#' @param a matrix which columns are to plotted
#'
#' @return
#' @export
#'
#' @examples
long_matrix <- function(m)
{
  if (length(dim(m)) != 2)
    stop("2D only pls")
  m %>% 
    as.data.frame() %>%
    rownames_to_column() %>% 
    pivot_longer(-rowname)
}

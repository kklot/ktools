#' Plot a 2D matrix as image
#'
#' @param a matrix which columns are to plotted
#'
#' @return
#' @export
#'
#' @examples
gg_img <- function(m)
{
  if (length(dim(m)) != 2)
    stop("2D only pls")
  n_cols <- ncol(m)
  m %>% 
    as_tibble(.name_repair = "universal") %>%
    rownames_to_column() %>% 
    mutate(rowname=as.numeric(rowname)) %>% 
    pivot_longer(-rowname) %>%
    ggplot() + 
    geom_tile(aes(name, -rowname, fill=value))
}

#' Plot a 2D matrix as lines
#'
#' @param a matrix which columns are to plotted
#'
#' @return
#' @export
#'
#' @examples
gg_mat <- function(m)
{
  if (length(dim(m)) != 2)
    stop("2D only pls")
  n_cols <- ncol(m)
  m %>% 
    as_tibble(.name_repair = "universal") %>%
    mutate(x = 1:n()) %>% 
    pivot_longer(-x) %>%
    ggplot() +
    geom_line(aes(x, value, color=name))
}

#' Prepare ggplot by groups
#'
#' @param .data data contains variables
#' @param x x-axis
#' @param y y-axis
#' @param g grouping variabe
#'
#' @return
#' @export
#'
#' @examples
gg_group <- function(.data, x, y, g)
{
  .data %>% 
    ggplot(aes(x = !!rlang::enquo(x), 
               y = !!rlang::enquo(y), 
               color = !!rlang::enquo(g)))
}
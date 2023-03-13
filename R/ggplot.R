#' Sizing the figure in Python notebook IRKernel
#' 
#' too lazy to type the long options?
#' 
#' @param w repr.plot.width
#' @param h repr.plot.height
#' @examples
#' \dontrun{
#' sizing(7, 4)
#' }
#' @export
sizing <- function(w = 7, h = 4) {
    options(repr.plot.height = h, repr.plot.width = w) 
}

#' Unwrap a facet_wrap plot
#' 
#' Useful when there are a lot of panels in \code{\link[ggplot2]{facet_wrap})
#' and you want to browse (and optionally save) them one by one
#' 
#' @param g the ggplot2 object created as normal
#' @param plot whether to plot (or just save by turn the next args to TRUE)
#' @param save whether to save the plot to disk
#' @param path where to save them?
#' @inheritDotParams ggplot2::ggsave
#' @return invisible a list of ggplot objects, one for each panels
#' @examples
#' \dontrun{
#' g <- iris %>%
#'   ggplot(aes(Sepal.Length)) +
#'   geom_histogram() +
#'   facet_wrap(~Species)
#' facet_unwrap(g)
#' }
#' @export
facet_unwrap <- function(g, plot = TRUE, save = FALSE, path = '.', ...) {
    on.exit(devAskNewPage(ask = FALSE))
    devAskNewPage(ask = TRUE)
    gb <- ggplot2::ggplot_build(g)
    fc <- names(gb$layout$facet_params$facets)
    cb <- g$data %>%
      dplyr::select(tidyselect::all_of(fc)) %>%
      dplyr::distinct()
    cb[1, , drop = FALSE]
    message("There are ", nrow(cb), " plots")
    o <- list()
    for (r in 1:nrow(cb)) {
      p <- g %+% dplyr::semi_join(g$data, cb[r, , drop = F], by = tidyselect::all_of(fc))
      o[[r]] <- p
      print(p)
      if (save) {
        name <- paste0(path, "/", paste0(cb[1, ], collapse = "_"))
        dots <- list(...)
        if (exists("device", dots)) {
          name <- paste0(name, dots$device)
        } else {
          name <- paste0(name, "pdf")
        }
        ggplot2::ggsave(name, p, ...)
      }
    }
    invisible(o)
}

#' Plot a 2D matrix as image
#'
#' @param a matrix which columns are to plotted
#'
#' @return
#' @export
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
gg_group <- function(.data, x, y, g)
{
  .data %>% 
    ggplot(aes(x = !!rlang::enquo(x), 
               y = !!rlang::enquo(y), 
               color = !!rlang::enquo(g)))
}
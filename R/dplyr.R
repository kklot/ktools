#' Tabulate 2 variables with count and percentage
#'
#' @param .data data
#' @param row row variable
#' @param col column variable
#' @param digits round the percentages
#'
#' @export
tbl_AB <- function(.data, row, col, digits = 2) {
  row <- rlang::enquo(row)
  col <- rlang::enquo(col)
  .data %>%
    count(!!row, !!col) %>%
    pivot_wider(names_from = !!col, values_from = n) %>%
    mutate(across(-!!row, \(x) replace_na(x, replace = 0))) %>%
    mutate(
      n = rowSums(across(-!!row)),
      across(-c(!!row, n), \(x) round(x / n, 2), .names = "row_prop_{.col}")
    ) %>%
    relocate(n, .after = last_col()) %>%
    arrange(desc(n))
}

#' Matrix with names to long table with rowname as column
#'
#' @param m matrix with rowname
#' @export
long_matrix <- function(m)
{
  if (length(dim(m)) != 2)
    stop("2D only pls")
  m %>% 
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
      tidyr::pivot_longer(-rowname)
}

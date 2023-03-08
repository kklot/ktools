#' K-ring smoothing
#'
#' @param df data with h3 column
#' @param hex character name of the h3 column
#' @param metric character name of the metric column (continous variable)
#' @param k ring's radius
#' @param only_na fill missing values only, otherwise smooth all
#' @export
kring_smooth <- function(df, hex, metric, k = 1, only_na = TRUE) {
  rs <- k
  if (inherits(df, "sf"))
    df <- sf::st_drop_geometry(df)
  type <- typeof(df[1, metric])
  knb <- lapply(df[, hex], function(x) h3::k_ring(x, rs))
  est <- lapply(knb, function(x) {
    casev <- df[df[, hex] %in% x, metric]
    if (type == "double")
      o <- sum(casev, na.rm = T) / (1 + 3 * rs * (rs + 1))
    else
      o <- most_frequent(casev)
    o
  })
  if (type == 'character')
    est <- sapply(est, \(x) ifelse(identical(x, character(0)), NA_character_, x))
  if (only_na) {
    notna <- which(!is.na(df[, metric]))
    est[notna] <- df[notna, metric]
  }
  est
}

#' Find most frequent occurrences with rle
#'
#' @param x a vector
#' @export
most_frequent <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    o <- rle(sort(x))
    o$values[which.max(o$lengths)]
}

#' Find most frequent occurrences in the neighbors and fill in the missing
#' hexagon
#'
#' @param .data data with h3 columm
#' @param ids h3 ids to find
#' @param var variable to fill
#' @param h3var name of the h3 column
#' @param radius_max maximum of radius of hexagons to find
#' @param mode use the same number of neighbours (same) or only increase neighbors
#' in those hexagon with missing data at the smaller radius (progressive)
#' @details
#' progressive maximum the local influences while the other treats all missing
#' hexagon the same.
#' @export 
hex_fill <- function(.data, ids, var, h3var = "h3", radius_max = 3, mode = c("same", "progressive")) {
    var <- rlang::enquo(var)
    h3var <- rlang::ensym(h3var)
    type <- class(.data %>% head(1) %>% dplyr::pull(!!var))
    use_r <- 1
    fill_in <- function(x) {
        o <- .data %>%
            dplyr::filter(!!h3var %in% x) %>%
            dplyr::pull(!!var) %>%
            na.omit()
        if (length(o) == 0) {
            o <- ifelse(type == "character", NA_character_, NA_real_)
        }
        if (length(o) > 1) {
            o <- most_frequent(o)
        }
        o
    }
    mode <- match.arg(mode)
    done <- FALSE
    if (mode == "same") {
        while (!done) {
            for (i in 1:radius_max) {
                use_r <- i
                hex_to_get <- lapply(ids, h3::hex_ring, radius = i)
                o <- unlist(lapply(hex_to_get, fill_in))
                done <- all(!is.na(o))
            }
        }
    } else {
        while (!done) {
            ids_ <- ids
            o <- vector(type, length(ids))
            names(o) <- ids
            names(ids_) <- ids
            for (i in 1:radius_max) {
                use_r <- i
                hex_to_get <- lapply(ids_, h3::hex_ring, radius = i)
                o_ <- unlist(lapply(hex_to_get, fill_in))
                done <- all(!is.na(o_))
                good <- names(which(!is.na(o_)))
                still_mis <- names(which(is.na(o_)))
                ids_ <- ids_[still_mis]
                o[good] <- o_[good]
            }
        }
    }
    attr(o, "radius") <-  use_r
    o
}


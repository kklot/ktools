#' Strip unicode characters
#' 
#' the stripped one can be used in, for example, selecting columns.
#' 
#' @param x string with unicode such as "<U+2019>"
#' @export 
strip_U <- function(x) gsub("<U\\+(....)>", "\\\\u\\1", x) %>%  stringi::stri_unescape_unicode()

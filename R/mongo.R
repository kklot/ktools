#' Convert a R named list of variable and condition to mongo query
#' 
#' To fillter the data (select rows)
#' 
#' @param ... write a list of varible to query
#' @examples
#' \dontrun{
#' # not there is only one `=`
#' as_query(interview = TRUE)
#' }
#' @export
as_query <- function(...) {
    jsonlite::toJSON(rlang::list2(...), auto_unbox = T, pretty = T)
}

#' Convert a R list of variable to mongo fields
#'
#' To filter the data (select columns)
#'
#' @param ... write a list of varible to get
#' @examples
#' \dontrun{
#' as_field(var1, var2)
#' }
#' @export
as_field <- function(...) {
  names <- char(...)
  trues <- rep(T, length(names))
  names(trues) <- names
  trues <- c(trues, "_id" = FALSE)
  jsonlite::toJSON(as.list(trues), auto_unbox = T, pretty = T)
}

#' Convert an R named list to mongo `OR`
#'
#' @param ... a named list
#' @examples
#' \dontrunt{
#' query = as_or(interview = "Main interview", interview = "COVID-19 CATI interview (ca)")
#' }
#' @export
as_or <- function(...) {
    i <- list(...)
    conds <- toJSON(i[1], auto_unbox = T)
    for (e in 2:length(i)) {
        conds <- paste0(conds, ",", toJSON(i[e], auto_unbox = T))
    }
    paste0(
        '{"$or": [ ',
        conds,
        " ]}"
    )
}
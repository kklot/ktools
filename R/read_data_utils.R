#' Query variable and labels from data read with haven
#' 
#' Query variable and labels from data read with haven
#' 
#' @param d data
#' @param p character regex pattern for \link[base](grep)
#' @param ... extra arguments to \link[base](grep)
#' @export
query_label <- function(d, p, ...) {
  tab = label_table(d)
  position = grep(p, tab[, 'label'], 1)
  tab[position, ]
}

#' Generate lookup table for haven read spss/stata
#' 
#' Generate lookup table for haven read spss/stata
#' 
#' @param d data
#' @export
label_table <- function(d) {
  lapply(names(d), function(x) {
      lab <- attributes(d[[x]])$label
      lab <- ifelse(is.null(lab), "", lab)
      data.table::data.table(name=x, label=lab)
  }) %>% data.table::rbindlist()
}


#' Read variable coding of a variable from stata file
#' 
#' Read variable coding of a variable from stata file
#' 
#' @param data data
#' @param var_name character name of variable
#' @export
getcoding_readstata13 <- function(data, var_name) {
  meta_obj <- getmeta_readstata13(data)
  a = meta_obj[meta_obj$variable==var_name, 'labels'] %>% strsplit('\\|') %>% unlist
  b = meta_obj[meta_obj$variable==var_name, 'values'] %>% strsplit('\\|') %>% unlist
  data.frame(labels=a, values=as.numeric(b))
}

#' Read meta data from stata file
#' 
#' This includes labels, coding values
#' 
#' @param data data
#' @export
getmeta_readstata13 <- function(data) {
    a = data.frame(
        variable = colnames(data),
        desc = attr(data, 'var.labels')
    )
    b = attr(data, 'label.table') %>% 
        sapply(function(x) {
            c(names(x) %>% paste0(collapse='|'), 
                unname(x) %>% paste0(collapse='|'))}) %>% 
        t %>% data.frame %>% tibble::rownames_to_column() %>% 
        set_colnames(c('variable', 'labels', 'values')) %>% 
        dplyr::mutate(variable = tolower(variable))
    ab <- a %>% dplyr::full_join(b, 'variable')
    class(ab) <- c(class(ab), "meta_readstata13")
    ab
}

#' Query data frame read with readstata13
#' 
#' @param d Data
#' @param x quoted regex such as '\\d+' to find digits
#' @param in_labels search in labels or in var names 
query_readstata13 <- function(d, x, in_labels=TRUE) {
  if (in_labels) {
    a = d %>% attr('var.labels') %>% grep(x,.,1,,1)
    b = d %>% attr('var.labels') %>% grep(x,.,1,,0)
    o = data.frame(
      DESC=a, ID=b, NAME=colnames(d)[b], 
      stringsAsFactors = FALSE
    )
  } else {
    o = colnames(d) %>% grep(x,.,1,,1)
  }
  return(o)
}

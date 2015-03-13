library(stringr)

#' Multiple string substitution
#'
#' @param str - string which needs substitutions
#' @param subsl - list of substitutions, in the form list(c('orig', 'replace'), ...)
#' @param replace.all=F - see str_replace vs. str_replace_all
str_replace_multi <- function(str, subsl, replace.all=F) {
  if (replace.all) {
    replace.fcn <- str_replace_all
  } else {
    replace.fcn <- str_replace
  }
  if (length(subsl) > 1) {
    return(replace.fcn(str_replace_multi(str, subsl[2:length(subsl)], replace.all),
                       subsl[[1]][1], subsl[[1]][2]))
  } else {
    return(replace.fcn(str,
                       subsl[[1]][1], subsl[[1]][2]))
  }
}

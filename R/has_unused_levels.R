
##' Logical test of whether a factor has unused levels.
##'
##' @title Test for unused factor levels
##' @param f factor
##' @return Logical \code{TRUE} or \code{FALSE} value.
##' @author Alexander Zwart <alec.zwart at csiro.au>
##' @export
##'
has_unused_levels <- function(f)
{
  stopifnot(is.factor(f))
  return(nlevels(f) > nlevels(droplevels(f)))
}


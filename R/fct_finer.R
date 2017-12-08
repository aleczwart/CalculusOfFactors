
##' Test whether a factor is \emph{finer} or \emph{coarser} than
##' another factor.  Bailey (2008) defines factor \code{f} to be
##' \emph{finer} than factor \code{g} when each distinct level of
##' \code{f} is entirely contained within a level of \code{g}, and
##' \code{f} and \code{g} are not equivalent factors (see
##' \code{\link{fct_equiv}}).  If \code{f} is finer than \code{g},
##' then \code{g} is described as being \emph{coarser} than \code{f}.
##'
##' Function \code{fct_finer} tests whether its first argument is
##' finer than its second.  Function \code{fct_coarser} tests whether
##' its first argument is coarser than its second.
##' \code{\%is_finer_than\%} and \code{\%is_coarser_than\%} are binary
##' operator aliases for \code{fct_finer} and \code{fct_coarser},
##' respectively.
##'
##' Note that when \code{f} is finer than \code{g}, then \code{f} is
##' equivalent to the \emph{interaction} between \code{f} and \code{g}
##' - hence, \code{fct_equiv(f,interaction(f,g))} returns \code{TRUE}.
##'
##' Consistent with R's general philosophy, factors containing
##' \code{NA} values will return an \code{NA} result from
##' these functions.
##'
##' @title Factor relationships
##' @param f factor
##' @param g factor, of length equal to \code{f}.
##' @return Logical \code{TRUE} or \code{FALSE} value.
##' @references Bailey, R. (2008). The calculus of factors. In
##' \emph{Design of Comparative Experiments} (Cambridge Series in
##' Statistical and Probabilistic Mathematics,
##' pp. 169-218). Cambridge: Cambridge University
##' Press. doi:10.1017/CBO9780511611483.011
##' @author Alexander Zwart <alec.zwart at csiro.au>
##' @seealso Factor equivalence function \code{\link{fct_equiv}} and its binary
##' operator aliases \code{\link{\%is_equiv_to\%}} and
##' \code{\link{\%is_ordered_equiv_to\%}}.
##' @export
##'
##' @examples
##' ## Define factors f and g:
##' f <- factor(c(1,1,2,2,3,3,4,4,5,5,6),levels=1:6,labels=LETTERS[1:6])
##' f
##'
##' g <- factor(c(1,1,1,1,2,2,2,2,3,3,3),levels=1:3,labels=LETTERS[7:9])
##' g
##'
##' ## f is finer than g:
##' fct_finer(f,g)
##'
##' ## f is therefore not equivalent to g:
##' fct_equiv(f,g)
##'
##' ## g is not finer than f:
##' fct_finer(g,f)
##'
##' ## Rather - g is 'coarser' than f:
##' fct_coarser(g,f)
##'
##' ## Since f is finer than g, f is equivalent to the interaction
##' ## between f and g:
##' fct_equiv(f,interaction(f,g,drop=TRUE))
##'
fct_finer <- function(f,g)
  {
    stopifnot(is.factor(f))
    stopifnot(is.factor(g))
    stopifnot(length(f)==length(g))
    ## R convention - if an operation producing a scalar results
    ## involves missings, return NA.
    if (any(is.na(f)) | any(is.na(g))) return(NA)
    ## Can't think of any issues re unused levels when assessing
    ## nesting, so we drop any unused levels for testing:
    tt <- table(droplevels(f),droplevels(g))
    rslt <- all(apply(tt,1,function(x) sum(x > 0))==1) &
     all(apply(tt,2,function(x) sum(x > 0))>=1) &
      !suppressWarnings(fct_equiv(f,g))
    ##
    return(rslt)
  }

##' @rdname fct_finer
##' @export
fct_coarser <- function(f,g) return(fct_finer(g,f))

##' @rdname fct_finer
##' @export
`%is_finer_than%` <- function(f,g) fct_finer(f,g)

##' @rdname fct_finer
##' @export
`%is_coarser_than%` <- function(f,g) fct_finer(g,f)



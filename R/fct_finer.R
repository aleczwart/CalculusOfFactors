


## TODO are there any issues around unused levels when assessing
## finer/coarser?  I don't think so...

##' Test whether a factor is 'finer' than another factor.  Bailey
##' (2008) defines factor \code{f} to be finer than factor \code{g}
##' when each distinct level of \code{f} is entirely contained within
##' a level of \code{g}, and \code{f} and \code{g} are not equivalent
##' factors.  If \code{f} is finer than \code{g}, then \code{g} is
##' described as being 'coarser' than \code{f}.
##'
##' Function \code{fct_finer} (along with its binary operator alias
##' \code{\link{\%is_finer_than\%}}) tests whether a factor is 'finer'
##' than another factor.  Bailey (2008) defines factor \code{f} to be
##' finer than factor \code{g} when each distinct level of \code{f} is
##' entirely contained within a level of \code{g}, and \code{f} and
##' \code{g} are not equivalent factors (see \code{\link{fct_equiv}}
##' for the definition of factor equivalence).  If \code{f} is finer
##' than \code{g}, then \code{g} is described as being 'coarser' than
##' \code{f}.
##'
##' Note that when \code{f} is finer than \code{g}, then \code{f} is
##' \emph{equivalent} to the \emph{interaction} between \code{f} and
##' \code{g} - hence, \code{fct_equiv(f,interaction(f,g))} returns
##' \code{TRUE}.
##'
##' Note that, consistent with R's general philosophy, factors
##' containing \code{NA} values will return an \code{NA} result from
##' \code{fct_finer}.
##'
##' @title Factor relationships - 'finer' and 'coarser'
##' @param f factor
##' @param g factor, of length equal to \code{f}.
##' @return Logical \code{TRUE} or \code{FALSE} value.
##' @references Bailey, R. (2008). The calculus of factors. In
##' \emph{Design of Comparative Experiments} (Cambridge Series in
##' Statistical and Probabilistic Mathematics,
##' pp. 169-218). Cambridge: Cambridge University
##' Press. doi:10.1017/CBO9780511611483.011
##' @author Alexander Zwart <alec.zwart at csiro.au>
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

fct_coarser <- function(f,g) return(fct_finer(g,f))

`%is_finer_than%` <- function(f,g) fct_finer(f,g)

`%is_coarser_than%` <- function(f,g) fct_finer(g,f)



##f <- factor(c(1,1,2,2,1,1,2,2,1,1,2),levels=1:2,labels=LETTERS[1:2])
f <- factor(c(1,1,2,2,3,3,4,4,5,5,6),levels=1:6,labels=LETTERS[1:6])
f

g <- factor(c(1,1,1,1,2,2,2,2,3,3,3),levels=1:3,labels=LETTERS[7:9])
g


f %is_finer_than% g
g %is_finer_than% g



tt <- table(f,g)
## f is (strictly) nested within g if (post droplevels()):
##
## (i) each f level occurs with one, and only one, g level; and
##
## (ii) each G level occurs with at least one F level; AND
##
## (iii) at least one G level occurs with more than one F level.
##
## Relaxing (iii) allows f and g to be equivalent - hence, condition
## (iii) can be re-posed as
##
## (iii) f and g are not equivalent.

rslt <- sakekejthaewijhqwelifhaewiufhwae

all(apply(tt,1,function(x) sum(x > 0))==1) &
 all(apply(tt,2,function(x) sum(x > 0))>=1) &
  !suppressWarnings(fct_equiv(f,g))


tt <- table(g,f)
all(apply(tt,1,function(x) sum(x > 0))==1) &
 all(apply(tt,2,function(x) sum(x > 0))>=1) &
  !suppressWarnings(CalculusOfFactors::fct_equiv(g,f))



CalculusOfFactors::fct_equiv(f,g)



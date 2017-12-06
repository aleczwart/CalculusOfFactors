
## TODO - I've change the functionality slightly, so check that the
## examples still reflect the functionality.  Also, ensure that
## examples are given of the issues around ordered equivalence and
## unused levels...

## Function fct_equiv
##
##' Compare factors \code{f} and \code{g} for 'equivalence', meaning
##' that the two factors define the same partition/grouping,
##' regardless of differences in level labels between the factors.
##'
##' Function \code{fct_equiv} (along with its binary operator aliases
##' \code{\link{\%is_equiv_to\%}} and
##' \code{\link{\%is_ordered_equiv_to\%}} compares two factors for
##' equivalence, meaning that the two factors define the same
##' partition/grouping, regardless of differences in level labels
##' between the two factors.
##'
##' The weaker condition of \emph{unordered} equivalence
##' (\code{ordered=FALSE}, the default) implies that the factor level
##' order encodings need not be identical between the two factors.
##'
##' The stronger condition of \emph{ordered} equivalence
##' (\code{ordered=TRUE}) requires that the level order
##' \emph{encodings} be identical between the two factors, and hence
##' that \code{as.integer(f)} is identical to \code{as.integer(g)}.
##'
##' Regarding unused levels: If one or other of the factors has unused
##' levels, these will be \emph{ignored} in testing for unordered
##' equivalence - factors are compared on the basis of the levels that
##' are used, only.  In checking for ordered equivalence, unused
##' levels may effect the level encoding of a factor, depending upon
##' where the unused levels occur in the level order.  The key thing
##' to remember is that two factors \code{f} and \code{g} are regarded
##' as 'ordered equivalent' if and only if
##' \code{as.integer(f)==as.integer(g)} is \code{TRUE}.  See the
##' examples below for an illustration of the ways in which unused
##' levels may, or may not affect ordered equivalence status.
##'
##' Note that, consistent with R's general philosophy, factors
##' containing \code{NA} values will return an \code{NA} result from
##' \code{fct_equiv}.
##'
##' @title Equivalence of factors
##' @param f factor
##' @param g factor, of length equal to \code{f}.
##' @param ordered logical: Should \code{\link{fct_equiv}} also
##' require identical level order encoding ('ordered equivalence')?
##' Default is \code{ordered=FALSE}.
##' @return Logical \code{TRUE} or \code{FALSE} value.
##' @author Alexander Zwart <alec.zwart at csiro.au>
##' @export
##'
##' @examples
##' f1 <- factor(c("a", "a", "b", "b", "c", "c", "a", "a"))
##'
##' ## Equivalent to f1 - same grouping, different assignment of labels to
##' ## levels:
##' f2 <- factor(c("c", "c", "b", "b", "a", "a", "c", "c"))
##' fct_equiv(f1,f2)
##' ## However, level order encoding is different, so factors are not
##' ## order equivalent:
##' fct_equiv(f1,f2,ordered=TRUE)
##'
##' ## Equivalent to f1 - same grouping, different labels:
##' f3 <- factor(c("x", "x", "y", "y", "z", "z", "x", "x"))
##' fct_equiv(f1,f3)
##' ## Level order encoding is identical, so factors are also order
##' ## equivalent:
##' fct_equiv(f1,f3,ordered=TRUE)
##'
##' ## Equivalent to f1 - same grouping, different level order:
##' f4 <- factor(c("a", "a", "b", "b", "c", "c", "a", "a"),
##'              levels=c("b", "c", "a"))
##' fct_equiv(f1,f4)
##' ## With a different level order, the factors are not order equivalent:
##' fct_equiv(f1,f4,ordered=TRUE)
##'
##' ## Not equivalent to f1 - different grouping:
##' f5 <- factor(c("a", "a", "a", "b", "c", "c", "a", "a"))
##' fct_equiv(f1,f5)
##' ## Unordered non-equivalence implies ordered non-equivalence:
##' fct_equiv(f1,f5,ordered=TRUE)
##'
##' ## Not equivalent to F1 - different grouping (with fewer levels):
##' f6 <- factor(c("a", "a", "a", "b", "b", "b", "b", "b"))
##' fct_equiv(f1,f6)
##'
fct_equiv <- function(f,g,ordered=FALSE)
{
  fnm <- deparse(substitute(f))
  gnm <- deparse(substitute(g))
  stopifnot(is.factor(f))
  stopifnot(is.factor(g))
  stopifnot(length(f)==length(g))
  ##
  ## As per R convention, an operation involving an NA produces NA:
  if (any(is.na(f))|any(is.na(g))) return(NA)
  ##
  ## Unused levels are not dropped from the factors, merely ignored in
  ## the comparisons.  However, we DO warn the user if there are a
  ## different number of levels:
  if (length(levels(f))!=length(levels(g)))
    warning(paste0("Factors ",fnm," and ",gnm,
                   " have different numbers of levels"))
  ## Use of droplevels() ensures that unused levels are not included
  ## in the tabulation (and hence are ignored in deciding unordered
  ## equivalence):
  rslt <- all(apply(table(droplevels(f),droplevels(g)),1,
                    function(x) sum(x > 0))==1)
  ## Function droplevels() is NOT used here - we don't want to
  ## re-encode the factor levels prior to testing for _order_
  ## equivalence - the key determination of whether order equivalence
  ## exists is whether f and g produce equal vectors upon conversion
  ## to integer...
  if (ordered) rslt <- rslt & all(as.integer(f)==as.integer(g))
  ##
  return(rslt)
}


## Function `%is_equiv_to%`
##
##' Compare factors \code{f} and \code{g} for 'unordered equivalence'
##' meaning that the two factors define the same partition/grouping,
##' regardless of differences in level labels between the
##' factors. \code{\%is_equiv_to\%} is a binary operator version of
##' \code{\link{fct_equiv}} with specified argument
##' \code{ordered=FALSE}.
##'
##' \code{\%is_equiv_to\%} provides a binary operator version of the
##' function \code{\link{fct_equiv}} to test for 'unordered equivalence'
##' between two factors, meaning that the two factors define the same
##' partition/grouping, regardless of differences in level labels
##' between the two factors.
##'
##' \code{\%is_equiv_to\%} specifically tests the weaker condition of
##' \emph{unordered} equivalence (\code{ordered=FALSE}, the default)
##' which does \emph{not} require that the factor level order
##' encodings be identical for the two factors to be regarded as
##' equivalent.
##'
##' Regarding unused levels: If one or other of the factors has unused
##' levels, these will be \emph{ignored} in testing for unordered
##' equivalence - factors are compared on the basis of the levels that
##' are used, only.
##'
##' Note that, consistent with R's general philosophy, factors
##' containing \code{NA} values will return an \code{NA} result from
##' \code{fct_equiv}.
##'
##' @title Equivalence of factors
##' @usage f \%is_equiv_to\% g
##' @param f factor
##' @param g factor, of length equal to \code{f}.
##' @return Logical \code{TRUE} or \code{FALSE} value.
##' @author Alexander Zwart <alec.zwart at csiro.au>
##' @export
##'
##' @examples
##'
`%is_equiv_to%` <- function(f,g) fct_equiv(f,g)


## Function `%is_ordered_equiv_to%`
##
##' Compare factors \code{f} and \code{g} for 'ordered equivalence',
##' meaning that the two factors define the same partition/grouping
##' with the same level order encoding, regardless of differences in
##' level labels between the factors. \code{\%is_ordered_equiv_to\%} is
##' a binary operator version of \code{\link{fct_equiv}} with
##' specified argument \code{ordered=TRUE}.
##'
##' \code{\%is_ordered_equiv_to\%} provides a binary operator version of
##' the function \code{\link{fct_equiv}} with specified argument
##' \code{ordered=TRUE}, to test for 'ordered equivalence' between two
##' factors.  Ordered equivalence implies that the two factors define
##' the same partition/grouping with the same level order encoding,
##' regardless of differences in level labels between the two factors.
##'
##' Regarding unused levels: If one or other of the factors has unused
##' levels, these may effect the level encoding of a factor, depending
##' upon where the unused levels occur in the level order.  The key
##' thing to remember is that two factors \code{f} and \code{g} are
##' regarded as 'ordered equivalent' if and only if
##' \code{as.integer(f)==as.integer(g)} is \code{TRUE}.  See the
##' examples below for an illustration of the ways in which unused
##' levels may, or may not affect ordered equivalence status.
##'
##' Note that, consistent with R's general philosophy, factors
##' containing \code{NA} values will return an \code{NA} result from
##' \code{fct_equiv}.
##'
##' @title Equivalence of factors
##' @usage f \%is_ordered_equiv_to\% g
##' @param f factor
##' @param g factor, of length equal to \code{f}.
##' @return Logical \code{TRUE} or \code{FALSE} value.
##' @author Alexander Zwart <alec.zwart at csiro.au>
##' @export
##'
##' @examples
##'
`%is_ordered_equiv_to%` <- function(f,g) fct_equiv(f,g,ordered=TRUE)


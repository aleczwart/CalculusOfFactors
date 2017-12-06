
## TODO - I've change the functionality slightly, so check that the
## examples still reflect the functionality.  Also, ensure that
## examples are given of the issues around ordered equivalence and
## unused levels...

## Function fct_equiv
##
##' Compare factors \code{f} and \code{g} for 'equivalence' (meaning
##' that the two factors define the same partition/grouping,
##' regardless of differences in level labels between the factors).
##'
##' Function \code{fct_equiv} (along with its binary operator aliases
##' \code{%is_equiv_to%} and \code{%is_ordered_equiv_to%} compare two
##' factors for equivalence, meaning that the two factors define the
##' same partition/grouping, regardless of differences in level
##' labels between the two factors.
##'
##' The weaker condition of \emph{unordered} equivalence
##' (\code{ordered=FALSE}, the default) implies that the factor level
##' order encodings need not be identical between the two factors.
##'
##' The stronger condition of \emph{ordered} equivalence
##' (\code{ordered=TRUE}) requires that the level order
##' \emph(encodings) be identical between the two factors, and hence
##' that \code{as.integer(f)} is identical to \code{as.integer(g)}.
##'
##' Regarding unused levels: If one or other of the factors has unused
##' levels, these will be \emph{ignored} in testing for unordered
##' equivalence - factors are compared on the basis of the levels that
##' are used, only.  In checking for ordered equivalence, unused
##' levels may effect the level encoding of a factor, depending upon
##' where the unused levels occur in the level order.  The key thing
##' to remember is that two factors \code{f} and \code{g} are regarded
##' as 'ordered equivalent' if \code{as.integer(f)==as.integer(g)} is
##' \code{TRUE}.  See the examples below for an illustration of the
##' ways in which unused levels may, or may not affect ordered
##' equivalence status.
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
  stopifnot(is.factor(f))
  stopifnot(is.factor(g))
  stopifnot(length(f)==length(g))
  ##
  ## As per R convention, an operation involving an NA produces NA:
  if (any(is.na(f))|any(is.na(g))) return(NA)
  ##
  if (length(levels(f))!=length(levels(g))) {
    rslt <- FALSE
  } else {
    rslt <- all(apply(table(f,g),1,function(x) sum(x > 0))==1)
    if (ordered) rslt <- rslt & all(as.integer(f)==as.integer(g))
  }
  return(rslt)
}


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

require(testthat)


f <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3))

## Identical factors are equivalent and ordered equivalent:
g <- f

expect_that(fct_equiv(f,g),is_true())
expect_that(fct_equiv(g,f),is_true())  ## Commutative

expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative

## g has different level labels, but same level order:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:3,labels=3:1)

expect_that(fct_equiv(f,g),is_true())
expect_that(fct_equiv(g,f),is_true())  ## Commutative

expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative

## g has same level labels, but different level order
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 3:1)

expect_that(fct_equiv(f,g),is_true())
expect_that(fct_equiv(g,f),is_true())  ## Commutative

expect_that(fct_equiv(f,g,ordered=TRUE),is_false())
expect_that(fct_equiv(g,f,ordered=TRUE),is_false()) ## Commutative

######################################################################

## g has extra levels, same level order:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),levels=1:6)

expect_that(fct_equiv(f,g),is_true())
expect_that(fct_equiv(g,f),is_true())  ## Commutative

expect_that(fct_equiv(f,g),
            gives_warning("Factors f and g have different numbers of levels"))
expect_that(fct_equiv(g,f),
            gives_warning("Factors g and f have different numbers of levels"))

expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative

expect_that(fct_equiv(f,g,ordered=TRUE),
            gives_warning("Factors f and g have different numbers of levels"))
expect_that(fct_equiv(g,f,ordered=TRUE),
            gives_warning("Factors g and f have different numbers of levels"))

## g has different level labels and more levels, but same level order:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:6,labels=c(1:3,4:6))

expect_that(fct_equiv(f,g),is_true())
expect_that(fct_equiv(g,f),is_true())  ## Commutative

expect_that(fct_equiv(f,g),
            gives_warning("Factors f and g have different numbers of levels"))
expect_that(fct_equiv(g,f),
            gives_warning("Factors g and f have different numbers of levels"))

expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative

expect_that(fct_equiv(f,g,ordered=TRUE),
            gives_warning("Factors f and g have different numbers of levels"))
expect_that(fct_equiv(g,f,ordered=TRUE),
            gives_warning("Factors g and f have different numbers of levels"))

## g has more level labels, the same ones _used_, and a different
## level order:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 6:1)

expect_that(fct_equiv(f,g),is_true())
expect_that(fct_equiv(g,f),is_true())  ## Commutative

expect_that(fct_equiv(f,g),
            gives_warning("Factors f and g have different numbers of levels"))
expect_that(fct_equiv(g,f),
            gives_warning("Factors g and f have different numbers of levels"))

expect_that(fct_equiv(f,g,ordered=TRUE),is_false())
expect_that(fct_equiv(g,f,ordered=TRUE),is_false())  ## Commutative

expect_that(fct_equiv(f,g,ordered=TRUE),
            gives_warning("Factors f and g have different numbers of levels"))
expect_that(fct_equiv(g,f,ordered=TRUE),
            gives_warning("Factors g and f have different numbers of levels"))

            ## One or both factors contain NAs:
g <- factor(c(1,1,1,3,3,3,2,NA,2,3,3,3,3))

expect_that(is.na(fct_equiv(f,g)),is_true())
expect_that(is.na(fct_equiv(g,f)),is_true())  ## Commutative
expect_that(is.na(fct_equiv(g,g)),is_true())  ## Both factors contain NA's

expect_that(is.na(fct_equiv(f,g,ordered=TRUE)),is_true())
expect_that(is.na(fct_equiv(g,f,ordered=TRUE)),is_true())  ## Commutative




%is_equiv_to% <- function(f,g) fct_equiv(f,g)
%is_ordered_equiv_to% <- function(f,g) fct_equiv(f,g,ordered=TRUE)

## NA values in the factor?  I am returning NA.  Consistent with the
## usual R logic.

## Unused levels?  these can ignored, I think.  Probably important to
## ignore unused levels, since I

## Think about the possiilties.  Also, what to do about NA values or
## levels in the factor?  What to do when there are a different number
## of levels in the factor? (I don't think my levels length test above
## is the right thing to do - if there are _unused_ levels, the
## factors can still, stricktly speaking, be equivalent.  Perhaps
## produce a warning if there are unused levels but the factors are
## equivalent...)


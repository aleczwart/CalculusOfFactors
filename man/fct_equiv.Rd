% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_equiv.R
\name{fct_equiv}
\alias{fct_equiv}
\title{Equivalence of factors}
\usage{
fct_equiv(f, g, ordered = FALSE)
}
\arguments{
\item{f}{factor}

\item{g}{factor, of length equal to \code{f}.}

\item{ordered}{logical: Should \code{\link{fct_equiv}} also
require identical level order encoding ('ordered equivalence')?
Default is \code{ordered=FALSE}.}
}
\value{
Logical \code{TRUE} or \code{FALSE} value.
}
\description{
Compare factors \code{f} and \code{g} for 'equivalence', meaning
that the two factors define the same partition/grouping,
regardless of differences in level labels between the factors.
}
\details{
Function \code{fct_equiv} (along with its binary operator aliases
\code{\link{\%is_equiv_to\%}} and
\code{\link{\%is_ordered_equiv_to\%}} compares two factors for
equivalence, meaning that the two factors define the same
partition/grouping, regardless of differences in level labels
between the two factors.

The weaker condition of \emph{unordered} equivalence
(\code{ordered=FALSE}, the default) implies that the factor level
order encodings need not be identical between the two factors.

The stronger condition of \emph{ordered} equivalence
(\code{ordered=TRUE}) requires that the level order encodings be
identical between the two factors, and hence that
\code{as.integer(f)} is identical to \code{as.integer(g)}.

Regarding unused levels: If one or other of the factors has unused
levels, these will be \emph{ignored} in testing for unordered
equivalence - factors are compared only on the basis of levels
that are \emph{used}.  In checking for ordered equivalence, unused
levels may effect the level encoding of a factor, depending upon
where unused levels occur in the level order.  The key thing to
remember is that two factors \code{f} and \code{g} are regarded as
'ordered equivalent' if and only if
\code{as.integer(f)==as.integer(g)} is \code{TRUE}.  See the
examples below for an illustration of the ways in which unused
levels may, or may not, affect ordered equivalence status.  In
general, some care is needed when dealing with factors containing
unused levels. Function \code{\link{droplevels}} can be applied to
a factor to remove any unused levels (and this author generally
prefers to work with factors that have been stripped of any unused
levels), but do bear in mind that use of \code{\link{droplevels}}
may change the level order encoding of a factor...

\code{fct_equiv} will produce warning messages when one or both
factors contain unused levels. You can wrap the call to
\code{fct_equiv} in use \code{\link{suppressWarnings}} if you do
not wish to see these warnings.

Note that, consistent with R's general philosophy, factors
containing \code{NA} values will return an \code{NA} result from
\code{fct_equiv}.
}
\examples{
## Define a factor f:
f <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
           levels=1:3,
           labels=LETTERS[1:3])
f

## Factor g has different labels to f:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:3,
            labels=LETTERS[5:7])
g

## ...but represents the same partition, hence is equivalent:
fct_equiv(f,g)

## f and g also have the same level order encoding:
as.integer(f)
as.integer(g)

## ...hence are also ordered equivalent:
fct_equiv(f,g,ordered=TRUE)

## Now, let g represent the same partition, as f, with the same level
## labels, but with a different level order encoding:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 3:1,
            labels=LETTERS[3:1])
g

## ...hence, f and g are (unordered) equivalent:
fct_equiv(f,g)

## ...but not ordered equivalent:
fct_equiv(f,g,ordered=TRUE)

## Now let g represent the same partition as f, but with extra,
## unused levels:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:5,
            labels=LETTERS[5:9])
g

## When testing (unordered) equivalence, the unused levels in g
## are ignored, but a warning is given:
fct_equiv(f,g)

## Use suppressWarnings() if you don't want to see the warnings
suppressWarnings(fct_equiv(f,g))

## The above definition of g is also ordered equivalent, since the
## unused levels in g defined above do not affect the level order
## encoding:
as.integer(f)
as.integer(g)
fct_equiv(f,g,ordered=TRUE)

## However, consider g defined as follows:
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 0:4,
            labels=LETTERS[5:9])
g

## g is still (unordered) equivalent to f:
fct_equiv(f,g)

## ...but g is not ordered equivalent to f:
as.integer(f)
as.integer(g)
fct_equiv(f,g,ordered=TRUE)
## ...since as.integer(f) and as.integer(g) are not identical.

## Note therefore, that the definition of ordered equivalence used by
## fct_equiv() is stronger than one might think is required, since the
## integer encodings of f and g, though different, do imply the same
## ordering of the levels (for f and g defined above,
## \\code{as.integer(g) == as.integer(f) + 1}).  The definition of
## ordered equivalence used in fct_equiv() has been chosen based on
## what the author feels is likely to be more useful in practice (and
## I admit there's a possibility that I could be wrong...)

}
\author{
Alexander Zwart <alec.zwart at csiro.au>
}
\seealso{
Binary operator versions \code{\link{\%is_equiv_to\%}}
and \code{\link{\%is_ordered_equiv_to\%}}.
}


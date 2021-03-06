% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_equiv.R
\name{\%is_equiv_to\%}
\alias{\%is_equiv_to\%}
\title{Equivalence of factors}
\usage{
f \%is_equiv_to\% g
}
\arguments{
\item{f}{factor}

\item{g}{factor, of length equal to \code{f}.}
}
\value{
Logical \code{TRUE} or \code{FALSE} value.
}
\description{
Compare two factors and for 'unordered equivalence' meaning that
the two factors define the same partition/grouping, regardless of
differences in level labels between the
factors. \code{\%is_equiv_to\%} is a binary operator version of
\code{\link{fct_equiv}} with specified argument
\code{ordered=FALSE}.
}
\details{
\code{\%is_equiv_to\%} provides a binary operator version of the
function \code{\link{fct_equiv}} with argument
\code{ordered=FALSE}, to test for 'unordered equivalence' between
two factors, meaning that the two factors define the same
partition/grouping, regardless of differences in level labels
between the two factors.  Unordered equivalence does not require
that the factor levels order encodings be identical.

Regarding unused levels: If one or other of the factors has unused
levels, these will be \emph{ignored} in testing for unordered
equivalence - factors are compared only on the basis of the levels
that are \emph{used}.  \code{\%is_equiv_to\%} will give warning
messages if one or other of the factors has unused levels.

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
f \%is_equiv_to\% g

## See the help for function fct_equiv() for further examples and
## discussion.

}
\author{
Alexander Zwart <alec.zwart at csiro.au>
}
\seealso{
Binary operator \code{\link{\%is_ordered_equiv_to\%}} for
testing ordered equivalence, and function \code{\link{fct_equiv}}.
}


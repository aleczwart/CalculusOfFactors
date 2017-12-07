

## TODO are there any issues around unused levels when assessing
## finer/coarser?  I don't think so?


fct_finer <- function(f,g)
  {
    stopifnot(is.factor(f))
    stopifnot(is.factor(g))
    stopifnot(length(f)==length(g))
    ## R convention - if an operation producing a scalar results
    ## involves missings, return NA.
    if (any(is.na(f)) | any(is.na(g))) return(NA)
    ## Can't think of any issues re unused levels when assessing
    ## nesting, so let's just drop any unused levels:
    f <- droplevels(f)
    g <- droplevels(g)
    ##

  }


`%is_finer_than%` <- function(f,g) fct_finer(f,g)


##f <- factor(c(1,1,2,2,1,1,2,2,1,1,2),levels=1:2,labels=LETTERS[1:2])
f <- factor(c(1,1,2,2,3,3,4,4,5,5,6),levels=1:6,labels=LETTERS[1:6])
f

g <- factor(c(1,1,1,1,2,2,2,2,3,3,3),levels=1:3,labels=LETTERS[7:9])
g

table(f,g)

CalculusOfFactors::fct_equiv(f,g)


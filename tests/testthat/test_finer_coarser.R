
library(CalculusOfFactors)
context("Finer, coarser relationships")

## TODO double check all of these tests!

######################################################################
## Tests:
######################################################################

test_that("Finer and coarser functions & operators",
          {
            f <- factor(c(1,1,2,2,3,3,4,4,5,5,6),
                        levels=1:6,
                        labels=LETTERS[1:6])
            g <- factor(c(1,1,1,1,2,2,2,2,3,3,3),
                        levels=1:3,
                        labels=LETTERS[7:9])
            ## Test functions:
            expect_that(fct_finer(f,g),is_true())
            expect_that(fct_finer(g,g),is_false())
            expect_that(fct_finer(g,f),is_false())
            expect_that(fct_coarser(g,f),is_true())
            expect_that(fct_coarser(f,f),is_false())
            ## Test operators:
            expect_that(f %is_finer_than% g,is_true())
            expect_that(g %is_finer_than% g,is_false())
            expect_that(g %is_finer_than% f,is_false())
            expect_that(g %is_coarser_than% f,is_true())
            ## Only accepts factors:
            expect_that(fct_finer(as.integer(f),g),throws_error())
            ## Factor lengths must be equal:
            expect_that(fct_finer(f[1:9],g),throws_error())
          })


## fct_finer & fct_coarser are defined with arguments f and g.  I've
## refactored the warning generator code to ensure that warnings are
## given in terms of the names of factors passed to the function, not
## simply 'f' or 'g'. Previously, warnings were always given in terms
## of 'f' or 'g' when the warning was generated in a call to,
## e.g. fct_equiv() from within fct_finer().
test_that("Finer and coarser warnings",
          {
            x <- factor(c(1,1,2,2,3,3,4,4),levels=1:6)
            y <- factor(c(1,1,1,1,2,3,2,2))
            ##
            expect_that(fct_finer(x,y),
                        gives_warning("Factor x contains unused levels"))
            expect_that(fct_coarser(x,y),
                        gives_warning("Factor x contains unused levels"))
            expect_that(x %is_finer_than% y,
                        gives_warning("Factor x contains unused levels"))
            expect_that(x %is_coarser_than% y,
                        gives_warning("Factor x contains unused levels"))
          })


######################################################################
## Clean up
######################################################################

## suppressWarnings(rm(f,g))

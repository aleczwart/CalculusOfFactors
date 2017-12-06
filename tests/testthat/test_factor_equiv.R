library(CalculusOfFactors)
context("Factor equivalence")

## TODO double check all of these tests!

######################################################################
## Setup:
######################################################################

f <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels=1:3,
            labels=LETTERS[1:3])

######################################################################
## Tests
######################################################################

g <- f
##
test_that("Equivalence of identical factors",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
          })


g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:3,
            labels=LETTERS[5:7])
##
test_that("Different labels, same level encoding, version 1",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
          })


## As before, but checking that fct_equiv is not confused by the
## _same_ level labels being mapped in a different order!
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:3,
            labels=LETTERS[3:1])
##
test_that("Different labels, same level encoding, version 2",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
          })


g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 3:1,
            labels=LETTERS[3:1])
##
test_that("Same labels, different level encoding",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_false())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_false()) ## Commutative
          })


g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels=1:6,
            labels=LETTERS[1:6])
##
test_that("Unused levels, same level encoding",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(fct_equiv(g,f),
                        gives_warning("Factors g and f have different numbers of levels"))
            ##
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factors g and f have different numbers of levels"))
          })

g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:6,labels=LETTERS[c(7:12)])
##
test_that("Unused labels, different level labels, same level encoding",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(fct_equiv(g,f),
                        gives_warning("Factors g and f have different numbers of levels"))
            ##
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factors g and f have different numbers of levels"))
          })


g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 6:1,labels=LETTERS[6:1])
##
test_that("Unused labels, same ones _used_, different level encoding",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(fct_equiv(g,f),
                        gives_warning("Factors g and f have different numbers of levels"))
            ##
            expect_that(fct_equiv(f,g,ordered=TRUE),is_false())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_false())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factors g and f have different numbers of levels"))
          })


g <- factor(c(1,1,1,3,3,3,2,NA,2,3,3,3,3))
##
test_that("One or both factors contain NAs",
          {
            expect_that(is.na(fct_equiv(f,g)),is_true())
            expect_that(is.na(fct_equiv(g,f)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g)),is_true())  ## Both factors contain NA's
            expect_that(is.na(fct_equiv(f,g,ordered=TRUE)),is_true())
            expect_that(is.na(fct_equiv(g,f,ordered=TRUE)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g,ordered=TRUE)),is_true())  ## Both factors contain NA's
          })


g <- factor(rep(NA,length(f)),levels=1:6)
##
test_that("One or both factors contain only NAs",
          {
            expect_that(is.na(fct_equiv(f,g)),is_true())
            expect_that(is.na(fct_equiv(g,f)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g)),is_true())  ## Both factors contain NA's
            expect_that(is.na(fct_equiv(f,g,ordered=TRUE)),is_true())
            expect_that(is.na(fct_equiv(g,f,ordered=TRUE)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g,ordered=TRUE)),is_true())  ## Both factors contain NA's

          })


######################################################################
## Clean up
######################################################################

suppressWarnings(rm(f,g))

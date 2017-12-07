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


## Defining tt1, etc in this way should prevent testthat from reporting
## warnings (no need, since the warnings are explicitly tested for).
g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels=1:6,
            labels=LETTERS[1:6])
##
test_that("Unused levels, same level encoding",
          {
            expect_that(tt1 <- fct_equiv(f,g),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(tt2 <- fct_equiv(g,f),
                        gives_warning("Factors g and f have different numbers of levels"))
            expect_that(tt1,is_true())
            expect_that(tt2,is_true())  ## Commutative
            ##
            expect_that(tt3 <- fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(tt4 <- fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factors g and f have different numbers of levels"))
            expect_that(tt3,is_true())
            expect_that(tt4,is_true())  ## Commutative
          })

g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 1:6,labels=LETTERS[c(7:12)])
##
test_that("Unused labels, different level labels, same level encoding",
          {
            expect_that(tt1 <- fct_equiv(f,g),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(tt2 <- fct_equiv(g,f),
                        gives_warning("Factors g and f have different numbers of levels"))
            expect_that(tt1,is_true())
            expect_that(tt2,is_true())  ## Commutative
            ##
            expect_that(tt3 <- fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(tt4 <- fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factors g and f have different numbers of levels"))
            expect_that(tt3,is_true())
            expect_that(tt4,is_true())  ## Commutative
          })


g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 6:1,labels=LETTERS[6:1])
##
test_that("Unused labels, same ones _used_, different level encoding",
          {
            expect_that(tt1 <- fct_equiv(f,g),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(tt2 <- fct_equiv(g,f),
                        gives_warning("Factors g and f have different numbers of levels"))
            expect_that(tt1,is_true())
            expect_that(tt2,is_true())  ## Commutative
            ##
            expect_that(tt3 <- fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factors f and g have different numbers of levels"))
            expect_that(tt4 <- fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factors g and f have different numbers of levels"))
            expect_that(tt3,is_false())
            expect_that(tt4,is_false())  ## Commutative
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
test_that("One or both factors contain ONLY NAs",
          {
            expect_that(is.na(fct_equiv(f,g)),is_true())
            expect_that(is.na(fct_equiv(g,f)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g)),is_true())  ## Both factors contain NA's
            expect_that(is.na(fct_equiv(f,g,ordered=TRUE)),is_true())
            expect_that(is.na(fct_equiv(g,f,ordered=TRUE)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g,ordered=TRUE)),is_true())  ## Both factors contain NA's

          })


## Bugfix - nested factor relationships were erroneously regarded as
## cases of equivalent factors. Here's a check, to make sure that
## doesn't happen again:
f <- factor(c(1,1,2,2,3,3,4,4,5,5,6),levels=1:6,labels=LETTERS[1:6])
g <- factor(c(1,1,1,1,2,2,2,2,3,3,3),levels=1:3,labels=LETTERS[7:9])
test_that("Nested relationships not confused for equivalence",
          {
            expect_that(is.na(suppressWarnings(fct_equiv(f,g))),
                        is_false())
            expect_that(is.na(suppressWarnings(fct_equiv(g,f))),
                        is_false())
            expect_that(is.na(suppressWarnings(fct_equiv(f,g,ordered=TRUE))),
                        is_false())
            expect_that(is.na(suppressWarnings(fct_equiv(g,f,ordered=TRUE))),
                        is_false())
          })



######################################################################
## Clean up
######################################################################

suppressWarnings(rm(f,g))

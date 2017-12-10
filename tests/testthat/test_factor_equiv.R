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

test_that("Equivalence of identical factors",
          {
            g <- f
            ##
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
          })


test_that("Different labels, same level encoding, version 1",
          {
            g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
                        levels = 1:3,
                        labels=LETTERS[5:7])
            ##
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
          })


## As before, but checking that fct_equiv is not confused by the
## _same_ level labels being mapped in a different order!
test_that("Different labels, same level encoding, version 2",
          {
            g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
                        levels = 1:3,
                        labels=LETTERS[3:1])
            ##
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
          })


test_that("Same labels, different level encoding",
          {
            g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
                        levels = 3:1,
                        labels=LETTERS[3:1])
            ##
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_false())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_false()) ## Commutative
          })


## Defining tt1, etc in this way ensures that testthat _tests_ for all
## warnings, instead of reporting them as issues.
test_that("Unused levels, same level encoding",
          {
            g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
                        levels=1:6,
                        labels=LETTERS[1:6])
            ##
            expect_that(tt1 <- fct_equiv(f,g),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt2 <- fct_equiv(g,f),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt1,is_true())
            expect_that(tt2,is_true())  ## Commutative
            ##
            expect_that(tt3 <- fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt4 <- fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt3,is_true())
            expect_that(tt4,is_true())  ## Commutative
          })

test_that("Unused labels, different level labels, same level encoding",
          {
            g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
                        levels = 1:6,labels=LETTERS[c(7:12)])
            ##
            expect_that(tt1 <- fct_equiv(f,g),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt2 <- fct_equiv(g,f),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt1,is_true())
            expect_that(tt2,is_true())  ## Commutative
            ##
            expect_that(tt3 <- fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt4 <- fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt3,is_true())
            expect_that(tt4,is_true())  ## Commutative
          })


test_that("Unused labels, same ones _used_, different level encoding",
          {
            g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
                        levels = 6:1,labels=LETTERS[6:1])
            ##
            expect_that(tt1 <- fct_equiv(f,g),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt2 <- fct_equiv(g,f),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt1,is_true())
            expect_that(tt2,is_true())  ## Commutative
            ##
            expect_that(tt3 <- fct_equiv(f,g,ordered=TRUE),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt4 <- fct_equiv(g,f,ordered=TRUE),
                        gives_warning("Factor g contains unused levels"))
            expect_that(tt3,is_false())
            expect_that(tt4,is_false())  ## Commutative
          })


test_that("One or both factors contain NAs",
          {
            g <- factor(c(1,1,1,3,3,3,2,NA,2,3,3,3,3))
            ##
            expect_that(is.na(fct_equiv(f,g)),is_true())
            expect_that(is.na(fct_equiv(g,f)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g)),is_true())  ## Both factors contain NA's
            expect_that(is.na(fct_equiv(f,g,ordered=TRUE)),is_true())
            expect_that(is.na(fct_equiv(g,f,ordered=TRUE)),is_true())  ## Commutative
            expect_that(is.na(fct_equiv(g,g,ordered=TRUE)),is_true())  ## Both factors contain NA's
          })


test_that("One or both factors contain ONLY NAs",
          {
            g <- factor(rep(NA,length(f)),levels=1:6)
            ##
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
test_that("Nested relationships not confused for equivalence",
          {
            f <- factor(c(1,1,2,2,3,3,4,4,5,5,6),levels=1:6,labels=LETTERS[1:6])
            g <- factor(c(1,1,1,1,2,2,2,2,3,3,3),levels=1:3,labels=LETTERS[7:9])
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

suppressWarnings(rm(f))

library(CalculusOfFactors)
context("Factor equivalence")

## TODO double check all of these tests!

######################################################################
## Setup:
######################################################################

f <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3))

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
            levels = 1:3,labels=3:1)
##
test_that("Different level labels, same order",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_true())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_true())  ## Commutative
          })

g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),
            levels = 3:1)
##
test_that("Same level labels, different order",
          {
            expect_that(fct_equiv(f,g),is_true())
            expect_that(fct_equiv(g,f),is_true())  ## Commutative
            expect_that(fct_equiv(f,g,ordered=TRUE),is_false())
            expect_that(fct_equiv(g,f,ordered=TRUE),is_false()) ## Commutative
          })

g <- factor(c(1,1,1,3,3,3,2,2,2,3,3,3,3),levels=1:6)
##
test_that("Extra levels, same level order",
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
            levels = 1:6,labels=c(1:3,4:6))
##
test_that("Different level labels, more levels, but same level order",
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
            levels = 6:1)
##
test_that("More level labels, same ones _used_, different level order",
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
          })


######################################################################
## Clean up
######################################################################

suppressWarnings(rm(f,g))

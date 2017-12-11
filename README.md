# CalculusOfFactors
Calculus of Factors is an R package that implements some of R. A. Bailey's 'calculus of factors' concepts, plus other factor 
related utility functions

Currently, the package implements:
1. Factor equivalence - two factors are equivalent if they define the same partition, regardless of differences in factor level labels.
2. 'finer' and 'coarser' comparisons.  A factor f is 'finer' than another factor g if each level of f is entirely contained within a 
level of g.  Factor g is then described as 'coarser' than f.  Note that if f is finer than g, then f is equivalent to the interaction 
between f and g.
3. Utility function has_unused_levels, to test whether a factor has unused levels.



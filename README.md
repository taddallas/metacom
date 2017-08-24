### *metacom*: an R package for the analysis of metacommunity structure

[![Build Status](https://travis-ci.org/taddallas/metacom.png?branch=master)](https://travis-ci.org/taddallas/metacom)
[![Downloads](http://cranlogs.r-pkg.org/badges/metacom)](http://cranlogs.r-pkg.org/badges/metacom)
[![codecov](https://codecov.io/gh/taddallas/metacom/branch/master/graph/badge.svg)](https://codecov.io/gh/taddallas/metacom)
[![Build status](https://ci.appveyor.com/api/projects/status/hoe5fjj5c2i1mh6n?svg=true)](https://ci.appveyor.com/project/taddallas/metacom)


### Install
Either install from [CRAN]( https://cran.r-project.org/package=metacom), or directly from GitHub using the code below. Note that GitHub is the development version of the package, which may or may not differ from what's on CRAN.


```r
# From CRAN
install.packages("metacom")
```

```r
# From GitHub
# install.packages("devtools")
devtools::install_github("taddallas/metacom")
library("metacom")
```

Description:

     'metacom' provides functions for the analysis of the elements of
     metacommunity structure (coherence, boundary clumping, &
     turnover), following the pattern-based metacommunity framework of
     Leibold & Mikkelson 2002 and Presley et al. 2010. This package is
     designed to allow the user to distinguish between several
     idealized patterns of metacommunity structure (Presley et al.
     2010) utilizing any number of null model algorithms for the
     randomization procedure. However, these metrics can also be used
     in isolation, and without ordination via reciprocal averaging, and
     instead, ordering along some biological gradient.

Definition of a metacommunity:

    A metacommunity is a set of sites (e.g. plants in plant-pollinator
     networks) associated through interactions (e.g. insect species
     (columns) interact with plant species (rows) in plant-pollinator
     networks). The pattern-based metacommunity concept, proposed by
     Leibold & Mikkelson 2002 and expounded on by Presley et al. 2010,
     allows for the evaluation of metacommunity structure by using
     randomization techniques to discern between 10 patterns of
     metacommunity structure. This is performed by ordinating
     site-by-species interaction matrices and calculating three
     metrics; coherence, boundary clumping & turnover.

The Elements of Metacommunity Structure Framework:

     The metacom package calculates these three metrics; coherence is
     calculated using the function Coherence(), boundary clumping with
     BoundaryClump(), and turnover (from either species or range
     perspective) using the Turnover() function. These functions are
     consolidated in the metacommunity() function, which can be used to
     calculate all three metrics. In order to interpret the output of
     these functions, it will be helpful to read Leibold & Mikkelson
     2002 and Presley et al. 2010, but to also read Ulrich and Gotelli
     2013, as this paper outlines the difficulty seemingly inherent
     with investigating community structure. Also, these functions do
     not have to be used strictly in the Leibold and Mikkelson 2002
     framework.

A cautionary note:

    I caution the user to be aware that the creation of null matrices
     can be performed to allow (or not allow) sites to be empty, or
     species to not exist at any site (i.e. column sums and/or row sums
     are allowed to be zero). This is controlled by the logical
     argument 'allow.empty' in the Metacommunity(), NullMaker(),
     Coherence(), and Turnover() functions. Restricting nulls to not
     allow empty rows or columns may be biologically realistic, but it
     also reduces the number of unique null matrices that can be built,
     which will impact computation time, making it infeasible or
     impossible in some situations. These situations occur when you
     have a very sparse interaction matrix, and is also influenced by
     null model algorithm ('method') that you choose.


The 'metacom' package is partially adapted from previous Matlab
code written by Christopher Higgins (available [here](http://faculty.tarleton.edu/higgins/metacommunity-structure.html)) and relies on many functions in the 'vegan' package (Oksanen et al. 2012).

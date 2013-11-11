\name{BoundaryClump}
\alias{BoundaryClump}


\title{
Determines boundary clumping 
}


\description{
'BoundaryClump' calculates the Morisita's Index (Morisita 1962) for presence-absence interaction matrices, using a chi-squared test to assess significance.

This statistic is not based on randomization methods, so the function only requires a presence-absence interaction matrix and two arguments regarding the ordination of the empirical matrix.

The default is the range perspective, meaning that the analyses of boundary clumping and species turnover compare the distribution of species among sites. If the 'community' perspective is desired, transpose the matrix before analysis using the transpose function ('t()'). However, the author cautions against misinterpretation of the community perspective, as the biological meaning of turnover and boundary clumping differ between perspectives. 

}



\usage{
BoundaryClump(comm, order=TRUE, scores=1)
}


\arguments{
  \item{comm}{
		community data in the form of a presence absence matrix
}
		\item{order}{
logical argument indicating whether to ordinate the interaction matrix or not. See details.
}

  \item{scores}{
axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores
}
}


\details{Boundary clumping, quantified by the Morisita's index, is a measure of the degree to which species range boundaries overlap. This measure, and species turnover, cannot be interpreted unless the network is significantly coherent (see 'Coherence()'). 

If 'order' is FALSE, the interaction matrix is not ordinated, allowing the user to order the matrix based on site characteristics or other biologically relevant characteristics.
}


\value{
'BoundaryClump' returns a data frame containing the calculated Morisita's index ('index'), the corresponding p-value ('P'), and the degrees of freedom ('df'). 

The p-value is based on a chi-squared test comparing the Morisita's index to a value of 1. If the Morisita's index is less than 1, a left-tailed test is performed (less clumping than expected by chance).
 
If the Morisita's index is greater than 1, a right-tailed test is performed (more clumping han expected by chance)
}

\references{
Morisita, M. 1962. Id-index, a measure of dispersion of individuals. Res. Popul. Ecol. 4, 1-7.
}

\author{
Tad Dallas
}



\examples{

## define an interaction matrix
data(TestMatrices)
intmat=TestMatrices[[1]]

## analysis of boundary clumping
bound.test=BoundaryClump(intmat)

## prints a single row data.frame containing the Morisita's index, p-value, and degrees of freedom
bound.test

}

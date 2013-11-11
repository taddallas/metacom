\name{Coherence}
\alias{Coherence}

\title{
Determines coherence
}
\description{
This function determines the number of embedded absences in an interaction matrix, and compares this value against null simulated matrices. Species ranges should be coherent along the ordination axis, as this axis represents a latent environmental gradient. A negative value of coherence (empirical matrix has more embedded absences than null matrices) indicates a 'checkerboard' pattern, whereas nonsignificance indicates a 'random' pattern (Leibold & Mikkelson 2002). 
}

\usage{
Coherence(comm, method = "r1", sims = 1000, scores = 1, 
order=TRUE, allow.empty=FALSE)
}

\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
  \item{method}{
null model randomization method used by 'nullmaker'. See details below (and the help file of fucntion 'nullmaker') for more information. 
}
  \item{sims}{
number of simulated null matrices to use in analysis
}
  \item{scores}{
axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores
}

		\item{order}{
logical argument indicating whether to ordinate the interaction matrix or not. See details.
}

		\item{allow.empty}{
logical argument indicating whether to allow null matrices to have empty rows or columns
}
}


\details{
'method' is an argument handed to functions in the 'vegan' package. Leibold & Mikkelson advocated the use of equiprobable rows and columns (provided that rows and columns had at least one entry). This method is called 'r00'. Methods maintaining row (site) frequencies include 'r0','r1' & 'r2'. The default method argument is 'r1', which maintains the species richness of a site (row totals) and fills species ranges (columns) based on their marginal probabilities. Arguably the most conservative null algorithm is the fixed row - fixed column total null, which is implemented as 'fixedfixed'. See the help file for 'commsimulator' or Wright et al. 1998 for more information. 

If 'order' is FALSE, the interaction matrix is not ordinated, allowing the user to order the matrix based on site characteristics or other biologically relevant characteristics.

This function can either be used as a standalone, or can be used through the 'metacommunity()' function, which determines all 3 elements of metacommunity structure (coherence, boundary clumping, & turnover) (Leibold & Mikkelson 2002)
}


\value{
\item{Coherence}{the number of embedded absences within the interaction matrix}
  
\item{z}{the z statistic comparing observed turnover against species turnover of null matrices}

\item{pval}{p-value from a z-test to assess significance of species turnover relative to null matrices}

\item{simulated mean}{mean number of replacements in simulated null matrices}

\item{simulated variance}{variance in the number of replacements of simulated null matrices}

\item{method}{randomization method to create null matrices}
}


\references{
Leibold, M.A. and G.M. Mikkelson. 2002. Coherence, species turnover, and boundary clumping: elements of meta-community structure. Oikos 97: 237 - 250.

Wright, D.H., Patterson, B.D., Mikkelson, G.M., Cutler, A. & Atmar, W. (1998). A comparative analysis of nested subset patterns of species composition. Oecologia 113, 1-20.
}


\author{
Tad Dallas
}

\examples{
#define an interaction matrix
data(testmatrices)
intmat=testmatrices[[2]]

#determine coherence of interaction matrix
coh.intmat=Coherence(intmat, method='r1', sims=1000, scores=1, order=TRUE)

#return results 
coh.intmat
}






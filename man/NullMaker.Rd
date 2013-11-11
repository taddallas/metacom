\name{NullMaker}
\alias{NullMaker}
\title{
Null matrix creator
}

\description{
Creates null matrices based on the constraints of the null model algorithm ('method'). Also allows for null matrices with a species that occurs at no sites, or a site without any species to be removed from the suite of simulated null matrices. This function borrows heavily from the commsimulator() function in the 'vegan' package, but also allows for the fixed-fixed null model. 
}

\usage{
NullMaker(comm, sims = 1000, method = "r1", allow.empty=FALSE)
}

\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
  \item{sims}{
number of simulated null matrices to use in analysis
}
  \item{method}{
null model randomization method. See details below.
}
		\item{allow.empty}{
logical argument indicating whether to allow null matrices to have empty rows or columns
}

}

\details{
'method' is the null model algorithm used to create the null matrices. The choice of a null algorithm is nontrivial. Leibold & Mikkelson advocated the use of equiprobable rows and columns (provided that rows and columns had at least one entry). This method is called 'r00'. Methods maintaining row (site) frequencies include 'r0','r1' & 'r2', whereas species (column) occurrences are preserved with fixed column methods such as 'c0'. The default method argument is 'r1', which maintains the species richness of a site (row totals) and fills species ranges (columns) based on their marginal probabilities. Arguably the most conservative null algorithm is the fixed row - fixed column total null, which is implemented as 'fixedfixed'. There are also sequential swap methods 'tswap', 'swap', and non-sequential 'quasiswap' and 'backtracking' that can be implemented as well. See the help file for 'commsimulator' or Wright et al. 1998 for more information. 

}

\value{
The output is a list of length 'sims' consisting of generated null matrices
}


\references{
J. Oksanen, F.G. Blanchet, R. Kindt, P. Legendre, P.R. Minchin, R.B. O'Hara, G.L. Simpson, P. Solymos, M.H.H. Stevens and H. Wagner (2012). vegan: Community Ecology Package. R package version 2.0-4. http://CRAN.R-project.org/package=vegan
}

\author{
Tad Dallas
}

\seealso{
commsimulator()
}

\examples{
#define an interaction matrix
data(TestMatrices)
intmat=TestMatrices[[4]]

#creation of the null matrices
nulls=NullMaker(intmat, sims=1000,method='r1')

#output is a list of length sims with each list element corresponding to a null matrix
str(nulls)
}



\keyword{ordination}

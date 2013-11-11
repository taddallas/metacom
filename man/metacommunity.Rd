\name{metacommunity}
\alias{metacommunity}


\title{
Analysis of the Elements of Metacommunity Structure
}

\description{
'metacommunity' is a wrapper for the analysis of the three elements of metacommunity structure (coherence, boundary clumping, & turnover) following the framework of Leibold & Mikkelson 2002. It is important to note here that the results of boundary clumping and turnover are only relevant if the matrix is significantly positively coherent (i.e. empirical matrix has fewer embedded absences than null matrices).

}

\usage{
metacommunity(comm, scores=1, method = "r1", sims = 1000, 
order=TRUE, allow.empty=FALSE)
}



\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
 
  \item{scores}{
Axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores. See Details.
}

 \item{method}{
null model randomization method used by 'nullmaker'. See details. 
}
  \item{sims}{
number of simulated null matrices to use in analysis
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

}

\note{This function may take awhile to finish as a result of the creation of null matrices. If you are running multiple interaction matrices, the code can be parallelized using the 'snow' package.}


\value{
\item{Comm}{ordinated matrix used to calculate coherence, boundary clumping & turnover}

\item{Coherence}{output of the Coherence() function, giving information on the number of embedded absences and the significance relative to simulated null matrices}

\item{Turnover}{output of the Turnover() function, testing the number of species replacements relative to simulated null matrices}

\item{Boundary}{output of the BoundaryClump() function, which calculates the Morisita's index, assessing significance using a chi-squared test}

}



\references{
Leibold, M.A. and G.M. Mikkelson. 2002. Coherence, species turnover, and boundary clumping: elements of meta-community structure. Oikos 97: 237 - 250.

Presley, S. J., C. L. Higgins, and M. R. Willig. 2010. A comprehensive framework for the evaluation of metacommunity structure. Oikos 119:908-917

Wright, D.H., Patterson, B.D., Mikkelson, G.M., Cutler, A. & Atmar, W. (1998). A comparative analysis of nested subset patterns of species composition. Oecologia 113, 1-20.
}


\author{
Tad Dallas
}


\examples{
#define an interaction matrix
data(testmatrices)
lei2b=testmatrices[[4]]

#determines the elements of metacommunity structure 
ems.intmat=metacommunity(lei2b ,method='r1' ,sims=1000, scores=1)

#return results in list form
ems.intmat
}



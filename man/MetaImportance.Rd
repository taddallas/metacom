\name{MetaImportance}
\alias{MetaImportance}


\title{
Determine Importance of Each Node on Metacommunity Structure
}

\description{
Permutes the interactions of each row in an interaction matrix to determine the relative effect of that single row on the three metacommunity statistics. Negative z-values for coherence mean that the species contributes negatively to coherence, as null matrices with interactions of that species or site randomized result in fewer embedded absences than the empirical matrix. 

}

\usage{
MetaImportance(comm, margin=1, scores = 1, method = "r00",
 sims = 1000, order = TRUE, allow.empty=TRUE, binary = TRUE, 
 verbose = TRUE, modularity=FALSE, c = sum(dim(comm)), nstarts= 100)
}


\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
 
  \item{margin}{
would you like to obtain importance values from the randomizations of rows (margin=1) or columns (margin=2) in your matrix?
}

  \item{scores}{
axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores. See Details.
}

 \item{method}{
null model randomization method used by \code{NullMaker}. See details. 
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

 \item{binary}{
logical argument indicating whether to ordinate the community matrix 
based on abundance or binary (default) data.
}


 \item{verbose}{
logical. Prints a graphical progress bar that tracks the creation of null matrices. Useful for conservative null models on large and/or sparse data.
}

 \item{modularity}{
logical. Use Barber's modularity (`Modularity` function) as a measure of boundary clumping, or use Morisita's index (`BoundaryClump` function)?
}

	\item{c}{starting guess for the number of modules present. Defaults to the maximum number of modules possible.}
	
	\item{nstarts}{number of starts. Default is 100. More will both slow the function down, and increase the likelihood of converging on the true modularity value.}



}


\details{
\code{method} is an argument handed to functions in the \code{vegan} package. Leibold & Mikkelson advocated the use of equiprobable rows and columns (provided that rows and columns had at least one entry). This method is called \code{r00}. Methods maintaining row (site) frequencies include \code{r0},\code{r1}, and \code{r2}. The default method argument is \code{r1}, which maintains the species richness of a site (row totals) and fills species ranges (columns) based on their marginal probabilities. Arguably the most conservative null algorithm is the fixed row - fixed column total null, which can be attained using many of swap algorithms described in the vegan package (sequential methods like \code{tswap}, \code{swap}, and non-sequential \code{quasiswap} and \code{backtracking}). See the help file for \code{commsim} or Wright et al. 1998 for more information. 


If \code{order} is FALSE, the interaction matrix is not ordinated, allowing the user to order the matrix based on site characteristics or other biologically relevant characteristics.

}

\note{This function may take awhile to finish as a result of the creation of null matrices. If you are running multiple interaction matrices, the code can be parallelized using the \code{snow} package.}


\value{
\item{data.frame}{Data frame containing the following columns: the row identity (`row`), changes to coherence (`coherence.Z`), changes to turnover (`turnover.Z`), and changes to Morisita's index (`bclump.I`) }

}



\references{
Dallas,T. 2014. metacom: an R package for the analysis of metacommunity structure. Ecography. DOI:10.1111/j.1600-0587.2013.00695.x

Leibold, M.A. and G.M. Mikkelson. 2002. Coherence, species turnover, and boundary clumping: elements of meta-community structure. Oikos 97: 237 - 250.

Wright, D.H., Patterson, B.D., Mikkelson, G.M., Cutler, A. & Atmar, W. (1998). A comparative analysis of nested subset patterns of species composition. Oecologia 113, 1-20.
}


\author{
Tad Dallas
}

\examples{
#define an interaction matrix
data(TestMatrices)
intMat=TestMatrices[[5]]

#determines row importance to coherence, turnover, boundary clumping
ems.intmat=MetaImportance(intMat, method='r00', sims=10, scores=1, verbose=TRUE, modularity=FALSE)

ems.intmat

}


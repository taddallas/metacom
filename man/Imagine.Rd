\name{Imagine}
\alias{Imagine}

\title{
Plotting of site-by-species interaction matrices
}
\description{
'Imagine' produces an image plot, grid of small rectangles representing species occurrences in sites, of a given interaction matrix.
}



\usage{
Imagine(comm, col=c(0,1), ordinate=TRUE, fill=TRUE, xlab='Species', 
ylab='Site', sitenames=rownames(comm), speciesnames=colnames(comm))}




\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
  \item{col}{
colors used to plot interactions. First value is the background color (no interaction) and the second color indicates an interaction. 
}
	\item{ordinate}{
logical. Should the interaction matrix be ordinated before plotting?
}

	\item{fill}{
logical. Should species ranges be made coherent before plotting?
}

	\item{xlab}{
name of the x axis
}

	\item{ylab}{
name of the y axis
}
	\item{sitenames}{
names for each row in the interaction matrix. Default is to not plot names.
}

	\item{speciesnames}{
names for each site in the interaction matrix. Default is to not plot names.
}

}


\value{
Produces an image plot of the interaction matrix. The code is very simple, and may need to be modified if you have long site or species names, or wish to make it prettier than I have the ability to. 
}




\author{
Tad Dallas
}




\examples{
#define an interaction matrix
data(TestMatrices)
pres3c=TestMatrices[[6]]

#plot interaction matrix
Imagine(pres3c, col=c('white','blue'), ordinate=TRUE, fill=FALSE)

}



\keyword{ordination}

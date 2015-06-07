\name{Imagine}
\alias{Imagine}

\title{
Plotting of site-by-species interaction matrices
}
\description{
'Imagine' produces an image plot, grid of small rectangles representing species occurrences in sites, of a given interaction matrix.
}



\usage{
Imagine(comm, col=c(0,1), ordinate=TRUE, scores=1, fill=TRUE, xlab='Species', 
ylab='Site', yline=2, xline=2, sitenames=rownames(comm), 
speciesnames=colnames(comm), binary=TRUE)}




\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
  \item{col}{
colors used to plot interactions. First value is the background color (no interaction) and the second color indicates an interaction. 
}
	\item{scores}{
	axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores
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

	\item{yline}{
line that the y-axis label is plotted on. 
}
	\item{xline}{
line that the x-axis label is plotted on. 
}

	\item{sitenames}{
names for each row in the interaction matrix. Default is to not plot names.
}

	\item{speciesnames}{
names for each site in the interaction matrix. Default is to not plot names.
}
 \item{binary}{
logical argument indicating whether to ordinate the community matrix based on abundance or binary (default) data.
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

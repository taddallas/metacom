\name{OrderMatrix}
\alias{OrderMatrix}

\title{
Ordinates interaction matrix
}
\description{
'OrderMatrix' ordinates an interaction matrix scores derived from reciprocal averaging (Gauch et al. 1977). These scores represent a latent environmental gradient along which species distributions are structured.   
}



\usage{
OrderMatrix(comm, scores = 1, output.scores=FALSE)
}




\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
  \item{scores}{
axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores
}
	\item{output.scores}{
logical. Default is to return the ordinated matrix. If 'output.scores' is TRUE, the function returns the site and species scores.
}
}



\value{
'OrderMatrix' returns either an ordinated matrix (output.scores = FALSE) or the site and species scores (output.scores = TRUE) obtained from reciprocal averaging. This function is already contained within functions calculating coherence, species turnover & boundary clumping, but may be useful for visualizations or for hypothesis testing concerning the important variables associated with the site or species scores.  
}



\references{
Gauch, H.G., R.H. Whittaker, and T.R. Wentworth. 1977. A comparative study of reciprocal averaging and other ordination techniques. Journal of Ecology 65:157-174.

Leibold, M.A. and G.M. Mikkelson. 2002. Coherence, species turnover, and boundary clumping: elements of meta-community structure. Oikos 97: 237 - 250.

Oksanen,J., F.G. Blanchet, R. Kindt, P. Legendre, P.R. Minchin, R.B. O'Hara, G.L. Simpson, P. Solymos, M.H.H. Stevens and H. Wagner (2012). vegan: Community Ecology Package. R package version 2.0-4. http://CRAN.R-project.org/package=vegan

}

\author{
Tad Dallas
}

\note{
'OrderMatrix', like many of these functions, relies heavily on the 'vegan' package.
}



\examples{
#define an interaction matrix
data(TestMatrices)
pres3c=TestMatrices[[6]]

#obtain an ordinated interaction matrix
OrderMatrix(pres3c, scores = 1, output.scores = FALSE)

#obtain site and species scores from reciprocal averaging
OrderMatrix(pres3c, scores = 1, output.scores = TRUE)

}



\keyword{ordination}

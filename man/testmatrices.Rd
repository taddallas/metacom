\name{testmatrices}
\alias{testmatrices}
\docType{data}
\title{
Test matrices used to evaluate metacommunity functions
}


\description{
A list of 7 test matrices from two of the foundational papers on the elements of metacommunity structure (Leibold & Mikkelson 2002 and Presley et al. 2010)
}


\usage{data(testmatrices)}


\format{
A list containing interaction matrices from Leibold & Mikkelson 2002 and Presley et al. 2010:

	[[1]]  dim=(10 x 10) Leibold & Mikkelson 2002 Figure 1a

	[[2]]  dim=(10 x 10) Leibold & Mikkelson 2002 Figure 1b

	[[3]]  dim=(10 x 10) Leibold & Mikkelson 2002 Figure 2a

	[[4]]  dim=(10 x 10) Leibold & Mikkelson 2002 Figure 2b

	[[5]]  dim=(15 x 10) Leibold & Mikkelson 2002 Figure 3c

	[[6]]  dim=(20 x 20) Presley et al. Figure 3c

	[[7]]  dim(20 x 20) Presley et al. Figure 4a
}



\source{
Leibold, M. A., & Mikkelson, G. M. (2002). Coherence, species turnover, and boundary clumping: elements of metacommunity structure. Oikos, 97(2), 237-250.

Presley, S. J., C. L. Higgins, and M. R. Willig. 2010. A comprehensive framework for the evaluation of metacommunity structure. Oikos 119:908-917
}



\examples{
#load list containing interaction matrices
data(testmatrices)

length(testmatrices)
names(testmatrices)

#image plot of interaction matrix, rotated for clarity
lei1a=testmatrices[[1]]
image(apply(lei1a,2,rev),col=c(0,'dodgerblue3'), axes=FALSE, xlab='species', ylab='sites', cex=1.4)
 box(lwd=2,col=1)
}


\keyword{datasets}

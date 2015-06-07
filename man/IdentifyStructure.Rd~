\name{IdentifyStructure}
\alias{IdentifyStructure}

\title{
Classifies metacommunities
}

\description{
Identifies structure (or quasi-structure) and outputs a classification.   
}



\usage{
IdentifyStructure(metacom.obj)
}




\arguments{
 
 \item{metacom.obj}{
The result of the `Metacommunity` function, containing a list of 4 elements; the empirical matrix being tested, and results for coherence, turnover, and boundary clumping.
}

}



\value{
Ouputs a classification of the metacommunity. 
}


\author{
Jon Lefcheck
}

\note{
Quasi structures, as well as 'random' and 'Gleasonian' structures, may not strictly be discernable through the EMS approach, as they rely on inferring a result from a non-significant test ('accepting the null'), which is typically a bad idea.
}



\examples{
# Define interaction matrices
data(TestMatrices)

# Analyze metacommunity & Identify the structure
metacom.list = lapply(TestMatrices[-1], function(i) Metacommunity(i))
lapply(metacom.list, IdentifyStructure)

}





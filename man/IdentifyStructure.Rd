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
John Lefcheck
}

\note{
Quasi structures, as well as 'random' and 'Gleasonian' structures, may not strictly be discernable through the EMS approach, as they rely on inferring a result from a non-significant test ('accepting the null'), which is typically a bad idea.
}



\examples{
# Define an interaction matrix
data(TestMatrices)
pres3c=TestMatrices[[6]]

# Analyze metacommunity
pres3c.metacom=Metacommunity(pres3c, sims=10, method='r1')

# Identify the structure
IdentifyStructure(pres3c.metacom)

}





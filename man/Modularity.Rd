\name{Modularity}
\alias{Modularity}

\title{
Detects module formation using Barber's Q statistic
}
\description{
Modularity measures the tendency of communities to form small groups, with dense interactions among group members, but sparse interactions with other groups. This is analagous in many ways to a Clementsian metacommunity. While this is separate from the Elements of Metacommunity Structure analysis, it is another tool to detect network structure. 
}

\usage{
Modularity(Atilde, c=sum(dim(Atilde)), nstarts=100, output='both')
}

\arguments{
  \item{Atilde}{
community data in the form of a presence absence matrix
}
  \item{c}{

}
  \item{nstarts}{

}
  \item{output}{

}

	
}


\details{

}


\value{
\item{}{}
 
}


\references{

}


\author{
Tad Dallas
}

\examples{
#define an interaction matrix
data(TestMatrices)
intmat=TestMatrices[[2]]


}






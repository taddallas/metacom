Modularity <- function(comm, method='tswap', sims=1000, scores=1,  order=TRUE, c=sum(dim(comm)), nstarts=100){

 
#suport functions
reduce <- function(v){
  n <- length(unique(v))
  while(any(sort(unique(v)) != 1:n)){
    su <- sort(unique(v))
    missing <- which(su != 1:n)[1]
    high <- which( v > missing)
    v[high] <- v[high] - 1
  }

  ret <- rep(0, n)
  for(i in 1:n){
    swap <- which(unique(v)==i)[1]
    ret[which(v==i)] <- swap
  }

  return(ret)
}

BarberQ <- function(comm, modules){
  modules <- reduce(modules)
  p <- nrow(comm)	# Number of red nodes
  q <- ncol(comm)	# Number of blue nodes
  n <- p+q		# Number of nodes
  if(n != length(modules)){
    warning("Please ensure that every node is assigned to exactly one module.")
  }
  m <- sum(comm != 0)	# Number of links
  c <- max(modules)	# Number of modules

  # Adjacency Matrix
  A <- rbind(cbind(matrix(0, p, p), comm), cbind(t(comm), matrix(0, q, q)))
  # Probabilities of potential links
  Ptilde <- (rowSums(comm) %*% t(colSums(comm)))/m
  # Probabilities of all links
  P <- rbind(cbind(matrix(0, p, p), Ptilde), cbind(t(Ptilde), matrix(0,q,q)))
  # Modularity Matrix
  B <- A - P
  # Index matrix
  S <- matrix(0, n, c)
  for(i in 1:n){
    S[i,modules[i]] <- 1
  }
  # Barber's Bipartite Modularity
  Q <- sum(diag(t(S) %*% B %*% S))/(2*m)
  return(Q)
}


maximizeQ <- function(comm, modules, c=sum(dim(comm))){
  modules <- reduce(modules)
  p <- nrow(comm)
  q <- ncol(comm)
  n <- p+q
  m <- sum(comm != 0)

  Ptilde <- (rowSums(comm) %*% t(colSums(comm)))/m
  Btilde <- comm - Ptilde

  S <- matrix(0, n, c)
  for(i in 1:n){
    S[i,modules[i]] <- 1
  }

  R <- S[1:p,]
  T <- S[(p+1):n,]

  lastMods <- rep(0, n)

  while(!all(lastMods == modules)){
    lastMods <- modules
    Ttilde <- Btilde %*% T
    redMods <- apply(Ttilde, 1, which.max)
    for(i in 1:p){
      tmp <- rep(0, c)
      tmp[redMods[i]] <- 1
      R[i,] <- tmp
    }
    Rtilde <- t(Btilde) %*% R
    blueMods <- apply(Rtilde, 1, which.max)
    for(i in 1:q){
      tmp <- rep(0, c)
      tmp[blueMods[i]] <- 1
      T[i,] <- tmp
    }
    modules <- c(redMods, blueMods)
  }

  modules <- reduce(modules)
  return(modules)
}


#actual modularity calculation
getQ=function(comm){
  n <- sum(dim(comm))
  best.modules <- rep(1,n)
  best.modules <- maximizeQ(comm, best.modules, c)
  best.Q <- BarberQ(comm, best.modules)

  for(i in 1:nstarts){
    modules <- reduce(sample(c, n, replace=TRUE))
    modules <- maximizeQ(comm, modules, c)
    Q <- BarberQ(comm, modules)
    if(Q > best.Q){
      best.modules <- modules
      best.Q <- Q
    }
  }
  ret <- list(Q=best.Q)
  return(ret)
}

	nulls=NullMaker(comm=comm, sims=sims, method=method, scores=scores, ordinate=order)
  Qemp=getQ(comm)$Q
	Qnull=as.numeric(unlist(lapply(nulls, getQ)))
  varstat = sd(Qnull)
  z = (mean(Qnull) - Qemp) / (varstat)
  pval = 2 * pnorm(-abs(z))
 return(data.frame(Q = Qemp, z = z, pval = pval, SimulatedMean = mean(Qnull))) 
}




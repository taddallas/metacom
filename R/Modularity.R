Modularity <- function(Atilde, c=sum(dim(Atilde)), nstarts=100, output='both'){
  
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

BarberQ <- function(Atilde, modules){
  modules <- reduce(modules)
  p <- nrow(Atilde)  # Number of red nodes
  q <- ncol(Atilde)	# Number of blue nodes
  n <- p+q		# Number of nodes
  if(n != length(modules)){
    warning("Please ensure that every node is assigned to exactly one module.")
  }
  m <- sum(Atilde != 0)	# Number of links
  c <- max(modules)	# Number of modules

  # Adjacency Matrix
  A <- rbind(cbind(matrix(0, p, p), Atilde), cbind(t(Atilde), matrix(0, q, q)))
  # Probabilities of potential links
  Ptilde <- (rowSums(Atilde) %*% t(colSums(Atilde)))/m
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



maximizeQ <- function(Atilde, modules, c=sum(dim(Atilde))){
  modules <- reduce(modules)
  p <- nrow(Atilde)
  q <- ncol(Atilde)
  n <- p+q
  m <- sum(Atilde != 0)

  Ptilde <- (rowSums(Atilde) %*% t(colSums(Atilde)))/m
  Btilde <- Atilde - Ptilde

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





	n <- sum(dim(Atilde))
  best.modules <- rep(1,n)
  best.modules <- maximizeQ(Atilde, best.modules, c)
  best.Q <- BarberQ(Atilde, best.modules)

  for(i in 1:nstarts){
    modules <- reduce(sample(c, n, replace=TRUE))
    modules <- maximizeQ(Atilde, modules, c)
    Q <- BarberQ(Atilde, modules)
    if(Q > best.Q){
      best.modules <- modules
      best.Q <- Q
    }
  }
if(output=='both'){return(list(Q=best.Q, modules=best.modules))}
if(output=='Q'){return(Q=best.Q)}
if(output=='modules'){return(best.modules)}
}

	






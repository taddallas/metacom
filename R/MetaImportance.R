MetaImportance=function(comm, margin=1, scores = 1, method = "r00", sims = 1000, order=TRUE, allow.empty=TRUE, binary = TRUE, verbose=TRUE, modularity=FALSE, c=sum(dim(comm)), nstarts=100){

 #To order or not to order
 if(order == TRUE){mat = OrderMatrix(comm, scores = scores, binary = binary)
  }else{mat = comm}

 #Coherence support function
  coherence <- function(web) {
    zeros = which(web == 0, arr.ind = TRUE)
    ret = matrix(0, ncol = 2)
    uncols = which(colSums(web) > 1)
    for (i in 1:length(uncols)) {
      temp = zeros[which(zeros[, 2] == uncols[i]), ]
      tempmin = min(which(web[, uncols[i]] == 1))
      tempmax = max(which(web[, uncols[i]] == 1))
      if (length(temp) < 3) {
        if (temp[1] %in% tempmin:tempmax) {
          ret = rbind(ret, as.vector(temp))
        }
      } else {
        temp = temp[which(temp[, 1] %in% tempmin:tempmax), 
                    ]
        ret = rbind(ret, temp)
      }
    }
    unrows = which(rowSums(web) > 1)
    for (j in 1:length(unrows)) {
      temp = zeros[which(zeros[, 1] == unrows[j]), ]
      tempmin = min(which(web[unrows[j], ] == 1))
      tempmax = max(which(web[unrows[j], ] == 1))
      if (length(temp) < 3) {
        if (temp[1] %in% tempmin:tempmax) {
          ret = rbind(ret, as.vector(temp))
        }
      } else {
        temp = temp[which(temp[, 2] %in% tempmin:tempmax), 
                    ]
        ret = rbind(ret, temp)
      }
    }
    ret = ret[-1, ]
    ret = unique(ret)
    return(dim(ret)[1])
  }
 

 #Turnover support function
  turnover = function(web) {
    for (i in 1:dim(web)[1]) {
      temp = web[i, ]
      if (sum(temp) < 2) {
        break
      }  else {
        web[i, min(which(temp == 1)):max(which(temp == 
                                                 1))] <- 1
      }
    }
    for (j in 1:dim(web)[2]) {
      temp = web[, j]
      if (sum(temp) < 2) {
        web[, j] = temp
      } else {
        first = min(which(temp == 1))
        last = max(which(temp == 1))
        web[first:last, j] <- 1
      }
    }
    D <- designdist(web, method = "(A-J)*(B-J)", terms = "minimum")
    return(sum(D))
  }


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



BarberQ=function(comm, modules){
  modules <- reduce(modules)
  p <- nrow(comm)  # Number of red nodes
  q <- ncol(comm)# Number of blue nodes
  n <- p+q# Number of nodes
  if(n != length(modules)){
    warning("Please ensure that every node is assigned to exactly one module.")
  }
  m <- sum(comm != 0)# Number of links
  c <- max(modules)# Number of modules
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

BarberModularity=function(comm, c=sum(dim(comm)), nstarts=nstarts){
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
return(best.Q)
}


COH = coherence(mat)
TUR = turnover(mat)

if(modularity==FALSE){BND = BoundaryClump(mat, scores = scores, order = F)}
if(modularity==TRUE){BND = BarberModularity(mat, c=c, nstarts=nstarts)}

cohZ=vector(); turZ=vector(); bcI=vector();

# Don't order nulls, because row indexing will get messed up. 
 nullMats = NullMaker(mat, sims = sims, method = method, allow.empty = allow.empty, ordinate=FALSE, scores=scores, verbose=verbose)

 if(margin == 1){dims = nrow(mat)}; if(margin==2){dims=ncol(mat)};
 if(verbose){prog = txtProgressBar(min = 0, max = dims, style = 3)}


#for each row/column, do this
for(i in 1:dims){
 mats = rep(list(mat), sims)

 if(margin==1){
  #Grab row i out of the full null matrices created above
  nullVecs = lapply(nullMats, function(x){return(x[i,])})
  for(z in 1:length(nullVecs)){mats[[z]][i,] = nullVecs[[z]]}
 }
 
 if(margin==2){
  #Grab col i out of the full null matrices list created above
  nullVecs = lapply(nullMats, function(x){return(x[,i])})
  for(z in 1:length(nullVecs)){mats[[z]][,i] = nullVecs[[z]]}
 }

 #calculate metacommunity statistics
  #coherence
  simstat=unlist(lapply(mats, coherence))
	#if(length(simstat) < sims){simstat=c(simstat, rep(0, (sims-length(simstat))))}
  cohVar = sd(simstat)
  cohZ[i] = (mean(simstat) - COH)/(cohVar)
 
	#boundary clumping or modularity
	boundSims=vector()
	for(b in 1:length(mats)){
   if(modularity==FALSE){  
    boundSims[b] = BoundaryClump(mats[[b]], scores = scores, order = order)$index}
  
   if(modularity==TRUE){
	  boundSims[b] = unlist(BarberModularity(mats[[b]], c=c, nstarts=nstarts))}
  }

  boundSims=as.vector(boundSims)
  
  if(modularity==FALSE){bcI[i] = (mean(boundSims) - BND$index)/(sd(boundSims))}
  if(modularity==TRUE){bcI[i] = (mean(boundSims) - BND)/(sd(boundSims))}
	
  #turnover
  turSims=unlist(lapply(mats,turnover))
	turVar = sd(turSims)
  turZ[i] = (mean(turSims) - TUR)/ (turVar)
  if(verbose==TRUE){setTxtProgressBar(prog, i)}
}

 if(modularity==FALSE){
 ret = data.frame(row=seq(1,dims), coherenceZ=cohZ, turnoverZ=turZ, boundaryI = bcI)}
 if(modularity==TRUE){
 ret = data.frame(row=seq(1,dims), coherenceZ=cohZ, turnoverZ=turZ, modularityZ = bcI)}
  return(ret)
}








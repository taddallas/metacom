Turnover <-function(comm ,method="r1" ,sims=1000 ,scores=1, order=TRUE, allow.empty=FALSE, binary=TRUE, progressBar=FALSE){

 if(order == TRUE){comm = OrderMatrix(comm, scores = scores, binary = binary)}

 turnover = function(web) {
  for (j in 1:dim(web)[2]) {
   temp = web[, j]
   if (sum(temp) < 2) {web[, j] = temp
   }else{
    bnds=range(which(temp==1))
    web[(bnds[1]:bnds[2]), j] <- 1
   }
  }
  
  D <- designdist(web, method = "(A-J)*(B-J)", terms = "minimum")
  return(sum(D))
 }

 statistic = turnover(comm)
 nulls = NullMaker(comm = comm, sims = sims, method = method, 
        allow.empty = allow.empty, verbose = verbose, ordinate=order)
 simstat=as.numeric(lapply(nulls,turnover))
 varstat=sd(simstat)
 z = (mean(simstat)-statistic)/(varstat)
 pval=2*pnorm(-abs(z))
 return(list(Turnover=statistic, z=z,pval=pval,SimulatedMean=mean(simstat),SimulatedVariance=varstat,Method=method))
	
}

NullMaker=function (comm, sims = 1000, method = "r1", scores=1, allow.empty = FALSE, .progressBar = FALSE)
{
    ret = list()
    if(.progressBar == T) pb = txtProgressBar(min = 0, max = sims, style = 3)
		comm=0+(comm>0)
		if(method=='fixedfixed'){
			ret=permatfull(comm, fixedmar='both', mtype='prab', times=sims)$perm
			ret=lapply(ret, OrderMatrix, scores=scores)
		}

		if(method!='fixedfixed'){
    if(allow.empty == FALSE) {
	for(i in 1:sims) {
        	temp = commsimulator(comm, method = method)
        	if (any(rowSums(temp) < 1) || any(colSums(temp) < 1)) { 
          	next
        	} else { 
          		ret[[i]] = suppressWarnings(OrderMatrix(temp))
        	} 
        	if(.progressBar == T) setTxtProgressBar(pb, i)
            }
        }
    }
    if (allow.empty == TRUE) {
    	for(i in 1:sims) {
        	temp = commsimulator(comm, method = method)
        	if(.progressBar == T) setTxtProgressBar(pb, i)
        	ret[[i]] = suppressWarnings(OrderMatrix(temp)) }
    }
  if(.progressBar == T) close(pb)
  return(ret)
}

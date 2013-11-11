NullMaker=function (comm, sims = 1000, method = "r1", allow.empty = FALSE)
{
    ret = list()
		
		if(method=='fixedfixed'){
			ret=permatfull(comm, fixedmar='both', mtype='prab', times=sims)$perm
			ret=lapply(ret, OrderMatrix)
		}

		if(method!='fixedfixed'){
    if(allow.empty == FALSE) {
				i = 1        
				while (i < sims + 1) {
					  temp = commsimulator(comm, method = method)
            if (any(rowSums(temp) < 1) || any(colSums(temp) < 
                1)) {
            }
            else {
                ret[[i]] = OrderMatrix(temp)
                i = i + 1
            }
        }
    }
    if (allow.empty == TRUE) {
        i = 1
        while (i < sims + 1) {
            temp = commsimulator(comm, method = method)
            ret[[i]] = OrderMatrix(temp)
            i = i + 1
        }
    }
		}
		return(ret)
}



metacommunity <-function(comm, scores=1, method='r1', sims=1000, order=TRUE, allow.empty=FALSE){
	if(order==TRUE){mat=OrderMatrix(comm,scores=scores)
	}else{mat=comm}

#creates nulls with which to test the three EMS metrics
nulls=nullmaker(mat, sims=sims, method=method,allow.empty=allow.empty)

#calculates the number of embedded absences
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
            }
            else {
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
            }
            else {
                temp = temp[which(temp[, 2] %in% tempmin:tempmax), 
                  ]
                ret = rbind(ret, temp)
            }
        }
        ret = ret[-1, ]
        ret = unique(ret)
        return(dim(ret)[1])
    }

embabs = coherence(mat)
simstat = as.numeric(lapply(nulls, coherence))
varstat = sd(simstat)
z = (mean(simstat) - embabs)/(varstat)
pval = 2 * pnorm(-abs(z))

coh.out=data.frame(output=c(embabs,z,pval,mean(simstat),varstat, method))
rownames(coh.out)=c('embedded absences', 'z', 'pval', 'sim.mean', 'sim.sd', 'method')

#boundary clumping
boundmat=BoundaryClump(mat, scores=scores, order=order)

#turnover function
turnover = function(web) {
        for (i in 1:dim(web)[1]) {
            temp = web[i, ]
            if (sum(temp) < 2) {
                break
            }
            else {
                web[i, min(which(temp == 1)):max(which(temp == 
                  1))] <- 1
            }
        }
        for (j in 1:dim(web)[2]) {
            temp = web[, j]
            if (sum(temp) < 2) {
                web[, j] = temp
            }
            else {
                first = min(which(temp == 1))
                last = max(which(temp == 1))
                web[first:last, j] <- 1
            }
        }
        D <- designdist(web, method = "(A-J)*(B-J)", terms = "minimum")
        return(sum(D))
    }

turn=turnover(mat)
simstat.t = as.numeric(lapply(nulls, turnover))
varstat.t = sd(simstat.t)
z.t = (mean(simstat.t) - turn)/(varstat.t)
pval.t = 2 * pnorm(-abs(z.t))
tur=data.frame(output=c(turn,z.t,pval.t,mean(simstat.t),varstat.t, method))
rownames(tur)=c('replacements', 'z', 'pval', 'sim.mean', 'sim.sd', 'method')

ret=list(Comm=mat, Coherence=coh.out, Turnover=tur, Boundary=boundmat)
	return(ret)
}

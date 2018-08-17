#' Analysis of the Elements of Metacommunity Structure
#'
#' 'Metacommunity' is a wrapper for the analysis of the three elements of
#' metacommunity structure (coherence, boundary clumping, & turnover) following
#' the framework of Leibold & Mikkelson 2002. It is important to note here that
#' the results of boundary clumping and turnover are only relevant if the
#' matrix is significantly positively coherent (i.e. empirical matrix has fewer
#' embedded absences than null matrices).
#'
#' 'method' is an argument handed to functions in the 'vegan' package. Leibold
#' & Mikkelson advocated the use of equiprobable rows and columns (provided
#' that rows and columns had at least one entry). This method is called 'r00'.
#' Methods maintaining row (site) frequencies include 'r0','r1' & 'r2'. The
#' default method argument is 'r1', which maintains the species richness of a
#' site (row totals) and fills species ranges (columns) based on their marginal
#' probabilities. Arguably the most conservative null algorithm is the fixed
#' row - fixed column total null, which can be attained using many of swap
#' algorithms described in the vegan package (sequential methods like 'tswap',
#' 'swap', and non-sequential 'quasiswap' and 'backtracking'). See the help
#' file for 'commsim' or Wright et al. 1998 for more information.
#'
#' If 'order' is FALSE, the interaction matrix is not ordinated, allowing the
#' user to order the matrix based on site characteristics or other biologically
#' relevant characteristics.
#'
#' @param comm community data in the form of a presence absence matrix
#' @param scores Axis scores to ordinate matrix. 1: primary axis scores
#' (default) 2: secondary axis scores. See Details.
#' @param method null model randomization method used by 'nullmaker'. See
#' details.
#' @param	turnoverMethod (default='EMS') 'EMS' option generates null matrices 
#'  as in Leibold and Mikkelson's original framework. However, null models
#'  that randomize occurrences -- introducing embedded absences (bad), but in a
#'  far less constrained null space (good) -- can also be used
#' @param sims number of simulated null matrices to use in analysis
#' @param order logical argument indicating whether to ordinate the interaction
#' matrix or not. See details.
#' @param allowEmpty logical argument indicating whether to allow null
#' matrices to have empty rows or columns
#' @param binary logical argument indicating whether to ordinate the community
#' matrix based on abundance or binary (default) data.
#' @param verbose Logical. Prints a graphical progress bar that tracks the
#' creation of null matrices. Useful for conservative null models on large
#' and/or sparse data.
#' @param seed seed for simulating the null model. Null matrices should be repeatable.
#' @return A list of length 4, containing;
#' Comm -- ordinated matrix used to calculate coherence, boundary
#' clumping & turnover
#'
#' Coherence --output of the Coherence() function, giving information on
#' the number of embedded absences and the significance relative to simulated
#' null matrices
#'
#' Turnover -- output of the Turnover() function, testing the number of
#' species replacements relative to simulated null matrices
#'
#' Boundary -- output of the BoundaryClump() function, which calculates the
#' Morisita's index, assessing significance using a chi-squared test
#'
#' @note This function may take awhile to finish as a result of the creation of
#' null matrices. If you are running multiple interaction matrices, the code
#' can be parallelized using the 'snow' package.
#' @author Tad Dallas
#' @export
#' @references Dallas,T. 2014. metacom: an R package for the analysis of
#' metacommunity structure. Ecography. DOI:10.1111/j.1600-0587.2013.00695.x
#'
#' Leibold, M.A. and G.M. Mikkelson. 2002. Coherence, species turnover, and
#' boundary clumping: elements of meta-community structure. Oikos 97: 237 -
#' 250.
#'
#' Presley, S. J., C. L. Higgins, and M. R. Willig. 2010. A comprehensive
#' framework for the evaluation of metacommunity structure. Oikos 119:908-917
#'
#' Wright, D.H., Patterson, B.D., Mikkelson, G.M., Cutler, A. & Atmar, W.
#' (1998). A comparative analysis of nested subset patterns of species
#' composition. Oecologia 113, 1-20.
#' @examples
#' #define an interaction matrix
#' data(TestMatrices)
#' intmat <- TestMatrices[[7]]
#'
#' #determines the elements of metacommunity structure
#' ems.test <- Metacommunity(intmat, method='r1', sims=100, scores=1)
#'

Metacommunity = function (comm, scores = 1, method = "r1",
	turnoverMethod='EMS', sims = 1000, order = TRUE, 
	allowEmpty = FALSE, binary = TRUE, verbose = FALSE, seed=1){

    if(order){
      mat <- OrderMatrix(comm, scores = scores, binary = binary)
    }else{
      mat <- comm
    }
    nulls <- NullMaker(mat, sims = sims, method = method, allowEmpty = allowEmpty, 
        verbose = verbose, ordinate=FALSE)
    coherence <- function(web) {
        zeros <- which(web == 0, arr.ind = TRUE)
        ret <- matrix(0, ncol = 2)
        uncols <- which(colSums(web) > 1)
        for (i in seq_len(length(uncols))){
            temp <- zeros[which(zeros[, 2] == uncols[i]), ]
            tempmin <- min(which(web[, uncols[i]] == 1))
            tempmax <- max(which(web[, uncols[i]] == 1))
            if (length(temp) < 3) {
                if (temp[1] %in% tempmin:tempmax) {
                  ret <- rbind(ret, as.vector(temp))
                }
            }else{
            	temp <- temp[which(temp[, 1] %in% tempmin:tempmax), ]
              ret <- rbind(ret, temp)
            }
        }
        unrows <- which(rowSums(web) > 1)
        for (j in seq_len(length(unrows))){
        	temp <- zeros[which(zeros[, 1] == unrows[j]), ]
          tempmin <- min(which(web[unrows[j], ] == 1))
          tempmax <- max(which(web[unrows[j], ] == 1))
          if (length(temp) < 3) {
        	  if (temp[1] %in% tempmin:tempmax) {
        	    ret <- rbind(ret, as.vector(temp))
            }
          }else{
	          temp <- temp[which(temp[, 2] %in% tempmin:tempmax), ]
            ret <- rbind(ret, temp)
          }
        }
        ret <- ret[-1, ]
        ret <- unique(ret)
        return(dim(ret)[1])
    }

		#coherence
    embabs <- coherence(mat)
    simstat <- vapply(nulls, coherence, FUN.VALUE=numeric(1))
    varstat <- sd(simstat)
    z <- (mean(simstat) - embabs)/(varstat)
    pval <- 2 * pnorm(-abs(z))

		meth <- paste('method =', method)
    coh.out <- data.frame(name=c('embAbs', 'z', 'p', 
			'simMean', 'simVariance', meth), 
		stat=c(embabs, z, pval, mean(simstat), varstat, NA))

		#boundary clumping
    boundmat <- BoundaryClump(mat, scores = scores, order = FALSE, 
        binary = binary, fill=TRUE)

		#turnover
		tur <- Turnover(comm, method=turnoverMethod, scores=scores, 
			sims=sims, order=order, fill=TRUE, allowEmpty=allowEmpty, 
			binary=binary, seed=seed)

    ret <- list(Comm = mat, Coherence = coh.out, Turnover = tur, 
        Boundary = boundmat)
    return(ret)
}

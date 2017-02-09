# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# Stochastic Volatility Modeling 
# with Hidden Markov Models
#
#  --> Continuous Sequential Importance Sampling Algorithm
#
# (Authors) Davide Viviano | Robert Lange | Hans-Peter Höllwirth
# (Date)    02.2017

# Input 1: alpha_pr - predictive density
# Input 2: alpha_wt - normal pdf evaluated at y[t]
# Input 3: u        - sorted uniformly random sampled vector (rejection sampling)
# Output:  alpha_up - particle filtered (continuous version)

csir <- function(alpha_pr, alpha_wt, u) {
    P <- length(alpha_pr)

    alpha_up <- rep(0,P)
    
    # sorting and weighting slows down
    alpha_wt <- alpha_wt/sum(alpha_wt)
    alpha_sort <- cbind(seq(1,P,1),alpha_pr)
    alpha_pr_idx <- alpha_sort[order(alpha_sort[,2]),]
    alpha_pr <- alpha_pr_idx[,2]
    alpha_idx <- alpha_pr_idx[,1]
    alpha_wt <- alpha_wt[alpha_idx]
    alpha_cwt <- c(0, cumsum(alpha_wt))
    alpha_pr  <- c(alpha_pr[1], alpha_pr)

    j <- 1
    for (i in 1:P){
        while((alpha_cwt[i] < u[j]) & (u[j] <= alpha_cwt[i+1])){
            alpha_up[j] <- alpha_pr[i] + ((alpha_pr[i+1]-alpha_pr[i])/(alpha_cwt[i+1]-alpha_cwt[i])) * (u[j]-alpha_cwt[i])
            if (j < P){
                j <- j+1
            }
            else break
        }
    }
    return(alpha_up)
}

# test run
# u = sort(runif(P,0,1))
# csir(alpha_pr, alpha_wt, u)

# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# Stochastic Volatility Modeling 
# with Hidden Markov Models
#
#  --> Fit maximum likelihood estimator
#
# (Authors) Davide Viviano | Robert Lange | Hans-Peter Höllwirth
# (Date)    02.2017

library(nloptr)

sv_fit <- function(y, theta, P, estimate) {
    T <- length(y)
  
    alpha_up_0 <- rnorm(P, 0,1)
    alpha_wt_0 <- rep(1/P,P)
    eta_sim <- rnorm(P*T, 0,1)
    eta_sim <- matrix(eta_sim, nrow=P, ncol=T)
    u_sim <- runif(P*T, 0,1)
    u_sim <- matrix(u_sim, P, T)
  
    for (t in c(1:T)) {u_sim[,t] <- sort( u_sim[,t] )}
  
    if(estimate==1) {
        print('estimating...') 
        
        # set optimization parameters
        lb <- rep(0,length(theta)) + 0.001;
        ub <- rep(1,length(theta)) - 2*exp(-10);
        obj <- function(x){ return( sv_loglik(x, y, eta_sim, u_sim, alpha_up_0, alpha_wt_0)$loglik ) } 
        
        # run box-constrained optimization
        #param <- nlminb( theta, obj, lower=lb, upper=ub )
        param <- optim( theta, obj, method='L-BFGS-B', lower=lb, upper=ub, hessian=TRUE )
        theta_mle <- param$par
        theta_se <- diag(sqrt(solve(param$hessian)))
        print('... done!') 
    
    } else {
        theta_mle <- c()
        theta_se <- c()
    }
  
    # compute log-liklihood (quantiles) of MLE parameters
    ll <- sv_loglik(theta_mle, y, eta_sim, u_sim, alpha_up_0, alpha_wt_0)
    
    return(list(loglik      = - ll$loglik, 
                theta_mle   = theta_mle, 
                theta_se    = theta_se,
                alpha_up_pr = ll$alpha_up_pr))
}

# test run
#theta <- c(0.05, 0.98, 0.02)
#sim_df <- sv_sim(theta, 1000)
#y <- sim_df$y
#P <- 200
#start <- proc.time()
#values <- sv_fit(y,theta,P,1)
#diff <- proc.time() - start






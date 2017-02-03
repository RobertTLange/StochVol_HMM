#function [loglik theta_sml theta_se alpha_up_quant] = sv_fit(y,theta,P,estimate)

library(nloptr)


sv_fit <- function(y,theta,P,estimate){
  T = length(y)
  
  alpha_up_0 = rnorm(P, 0,1)
  alpha_wt_0 = rep(1/P,P)
  eta_sim = rnorm(P*T, 0,1)
  eta_sim = matrix(eta_sim, nrow=P, ncol=T)
  u_sim = runif(P*T, 0,1)
  u_sim      = matrix(u_sim, P, T)
  
  for (t in c(1:T)){u_sim[,t] = sort( u_sim[,t] )}
  
  if(estimate==1){
    
    ## Algorithm is the closest I foundd to active set in Matlab
    ## Not all options are included(but they are not necessary)
    opts <- list("algorithm"="NLOPT_LD_SLSQP",
                 "xtol_rel"=1.0e-12)
    #options = optimset('fmincon');
    #options = optimset(options , 'Display'     , 'iter');
    #options = optimset(options , 'Diagnostics' , 'off');
    #options = optimset(options , 'TolCon',1e-10);
    #options = optimset(options , 'Algorithm','active-set');
    #options = optimset(options , 'TolFun',1e-12);
    
    lb = rep(0, length(theta))+0.001;
    ub = rep(1, length(theta))-2*exp(-10);
    
    print('estimating...') 
    
    # Note: theta should be a vector
    optimized <- nloptr(theta, eval_f = function(x)(sv_loglik(x, y, eta_sim, u_sim, alpha_up_0, alpha_wt_0))[[1]], opts = opts, lb=lb, ub=ub, )
    
    ## !! To check how to compute the hessian
    #theta_se =diag(sqrt(inv(HESSIAN)));
    print('... done!') 
    
    
  }
  else{
    ## !! Unsure to be correct
    stop("no estimate required")
    
  }
  
  cc = sv_loglik(optimized$solution,y,eta_sim,u_sim,alpha_up_0,alpha_wt_0)
  
  loglik = cc[[1]]
  alpha <- cc[[2]]
  return(list(loglik, optimized$solution, alpha))#, theta_se)
  
  
}








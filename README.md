# Stochastic Volatility Modelling using Hidden Markov Model

Project carried out for Financial Econometrics class tought by Prof. Christian Brownlees.
We estimate a common stochastic volatility model which models the volatility process as a latent state.
We infer the dynamics in the time series by utilizing a Hidden Markov Model (HMM) which allows identification by Sequential Monte Carlo.

## Code Structure ##
The following repository contains these scripts:

* run.R - Main pipeline which executes all of the following scripts

* sv_sim.R - Simulates a time series that follows a specified stochastics volatility model

* sir.R - Simulation-based filter (sequential importance sampling)

* csir.R - Simulation-based filter (continuous sequential importance sampling) which is needed for the likelihood estimation

* sv_fit.R - Optimization function for the likelihood

* sv_loglik.R - Compute the likelihood function

* csir.c /csir.so - C version of simulation-based filer for faster performance (5 times faster)

## Dependencies ##
Required libraries:
* fanplot (tested version 3.4.1; used for heat-map plots)
* nloptr (tested version 1.0.4; used for maximum likelihood estimation) 

## Instructions ##
Execute script run.R. 
For faster execution of the maximum likelihood estimation run function sv_fit in mode 2 (estimation without standard errors).


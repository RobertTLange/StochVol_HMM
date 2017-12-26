# Stochastic Volatility Modelling using Hidden Markov Model

Project carried out for Financial Econometrics class taught by Prof. Christian Brownlees.
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

* csir.c /csir.so - C version of simulation-based filer for faster performance (default). The R function dyn.load loads the DLL. For few platforms it might require different arguments. If an error shows up, please consult the help page of dyn.load function. 

* notebook.ipybn - Notebook showing an example of SV parameters estimation using CSIR. 

## Dependencies ##
Required libraries:
* fanplot (tested version 3.4.1; used for heat-map plots)
* nloptr (tested version 1.0.4; used for maximum likelihood estimation) 

## Instructions ##
Set as working directory your-directory/StochVol_HMM/code. 
Execute script run.R (on Linux sudo Rscript run.R); open the notebook file.
For faster execution of the maximum likelihood estimation run function sv_fit in mode 2 (estimation without standard errors).


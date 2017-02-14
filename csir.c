// ----------------------------------------------------------------------
// Information
// ----------------------------------------------------------------------

// Stochastic Volatility Modeling 
// with Hidden Markov Models
//
//  --> Continuous Sequential Importance Sampling Algorithm
//
// (Authors) Davide Viviano | Robert Lange | Hans-Peter Höllwirth
// (Date)    02.2017

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double *array;

// compare function for index sorting
int cmp(const void *a, const void *b){
    int ia = *(int *)a;
    int ib = *(int *)b;
    return array[ia] < array[ib] ? -1 : array[ia] > array[ib];
}
 
// Output:  alpha_up - particle filtered (continuous version)
// Input 1: alpha_pr - predictive density
// Input 2: alpha_wt - normal pdf evaluated at y[t]
// Input 3: u        - sorted uniformly random sampled vector (rejection sampling)
void csir(double *alpha_up, double *alpha_pr, double *alpha_wt, double *u, int *len) {
    
    int P = len[0];
    int i; 

    // standardize alpha_wt
    double sum_alpha_wt = 0.0;    
    for (i = 0; i < P; i++)
        sum_alpha_wt += alpha_wt[i];
        
    for (i = 0; i < P; i++) 
        alpha_wt[i] = alpha_wt[i] / sum_alpha_wt;
    
    // sort alpha_pr index
    int alpha_idx[P];
    for (i = 0; i < P; i++) {
        alpha_idx[i] = i;
    } 
    array = alpha_pr;
    qsort(alpha_idx, P, sizeof(*alpha_idx), cmp);    
    
    // compute cumulated sum of alpha_wt
    double alpha_cwt[(P+1)];
    alpha_cwt[0] = 0.0;
    for (i = 1; i <= P; i++) {
        alpha_cwt[i] = alpha_cwt[i-1] + alpha_wt[alpha_idx[i-1]];       
    }  
    
    // duplicate first element of alpha_pr 
    double * alpha_pr_new = malloc(P * sizeof(alpha_pr[0]));
    memcpy(alpha_pr_new, alpha_pr, (P+1) * sizeof(alpha_pr[0]));
    alpha_pr_new[0] = alpha_pr[alpha_idx[0]];
    for (i = 0; i < P; i++)
        alpha_pr_new[i+1] = alpha_pr[alpha_idx[i]];    

    // compute alpha_up
    int j = 0;
    for (i = 0; i < P; i++) {
        while((alpha_cwt[i] < u[j]) && (u[j] <= alpha_cwt[i+1])){
            alpha_up[j] = alpha_pr_new[i] + ((alpha_pr_new[i+1]-alpha_pr_new[i])/(alpha_cwt[i+1]-alpha_cwt[i])) * (u[j]-alpha_cwt[i]);
            if (j < P) {
                j++;
            }
            else break;
        }
    }  
}

/* 
// test function csir
int main(void){
    int P[] = {5};
    double alpha_pr[] = {1.737899, 2.247636, 2.813836, 2.521015, 2.959267};
    double alpha_wt[] = {0.02438808, 0.04078347, 0.05066587, 0.04691185, 0.05149138};
    double u[]        = {0.01631192, 0.04410280, 0.05565297, 0.62613028, 0.79035955};

    double alpha_up[P[0]];    
    //double *alpha_up;
    csir (alpha_up, alpha_pr, alpha_wt, u, P);
    
    printf("alpha_up\n");
    for (int i=0; i<P[0]; i++) { 
        printf("%lf\n", alpha_up[i]);
    }    
    
    return 0;
}
*/





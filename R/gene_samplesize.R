# This is a function to compute the minimum sample size required for a significant predictor influence in a linear model for gene dataset
# Inputs:
#   R_aquare: the R^2 of the previous study.
#   power: the power of the F test
#   alpha: the significance level of the F test,
#   u: degrees of freesim for numerator (the number of parameters in the model, except for intercept)
#   Output:
#   n: the minimum sample size (rounded up to the closest integer)
gene_samplesize<-function(R_square,power,alpha,u){
  # compute the effect size
  ES = R_square/(1-R_square)
  # do the F test
  t<-pwr.f2.test(u=u,f2=ES, sig.level = alpha, power = power)
  # compute the minimal sample size
  n = ceiling(u+t[[2]]+1)
  n
}
# pacman::p_load(pwr)
# R_square = 0.1
# power = 0.9
# alpha = 0.05
# u = 5
# gene_samplesize(R_square,power,alpha,u)

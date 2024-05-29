gene_samplesize<-function(R_square,power,alpha,u){
  ES = R_square/(1-R_square)
  t<-pwr.f2.test(u=u,f2=ES, sig.level = alpha, power = power)
  n = ceiling(u+t[[2]]+1)
  n
}
#pacman::p_load(pwr)
#R_square = 0.1
#power = 0.9
#alpha = 0.05
#u = 5
#gene_samplesize(R_square,power,alpha,u)

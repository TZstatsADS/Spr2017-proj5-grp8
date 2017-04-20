H<-H+0.01
MultinomialEM <- function(H, K, tau=0.1){
  
  index<-sample(1:nrow(H),K)
  t_k<-H[index,]
  for (i in 1:nrow(t_k)){
    t_k[i,]<-(t_k[i,]-min(t_k[i,]))/(max(t_k[i,]-min(t_k[i,])))
  }
  
  t_k<-t_k+0.01 #dim(t_k) should be K by 16
  c_k<-rep(1,length=K)
  A<-matrix(1,nrow=nrow(H),ncol=K)
  delta<-norm(A,"o")
  
  while (delta>tau){
    A_old<-A
    phi<-exp(H%*%(t(log(t_k)))) #nrow(H) by K
    A<-t(c_k*t(phi))/apply(t(c_k*t(phi)),1,sum) #A should be nrow(H) by K
    c_k<-as.vector(apply(A,2,sum))/nrow(H) #vector of length K
    b_k<-t(A)%*%H #b_k is K by 16
    t_k<-b_k/apply(b_k,1,sum)
    delta<-norm((A-A_old),"o")
  }
  
  m<-apply(A,1,which.max)
  return(m)
}
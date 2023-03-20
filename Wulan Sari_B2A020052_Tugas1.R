# Wulan Sari / B2A020052
# Tugas 1 Teknik Simulasi

multiplicative_RNG<-function(a,z0,m,n) {
  xw<-matrix(NA,n,3)
  colnames(xw)<-c("aZ","XW","UW")
  for(w in 1:n)
  {
    xw[w,1]<-(a*z0)
    xw[w,2]<-xw[w,1]%%m
    xw[w,3]<-xw[w,2]/m
    z0<-xw[w,2]
  }
  hist(xw[,3])
  View(xw)
}

multiplicative_RNG(45, 21139, 417, 150)
  
Bernouli_2<-function(n,p){
  i<-n
  p<-p
  x<-runif(i)
  Y<-(x<=p)+0
  (tabel<-table(Y)/length(Y))
}
Bernouli_2(150, 0.83)

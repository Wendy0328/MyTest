
Option_Price<-function(s_now, strike_price, sigma, r, q, T, n, Call_or_Put){


dt<-T/n

p1=0.5
p2=0.5
u<-exp(sigma*sqrt(dt)+r*dt-q*dt)/(0.5*(exp(sigma*sqrt(dt))+exp(-sigma*sqrt(dt))))
d<-exp(-sigma*sqrt(dt)+r*dt-q*dt)/(0.5*(exp(sigma*sqrt(dt))+exp(-sigma*sqrt(dt))))

m<-matrix(nrow = n,ncol = n)
m[1,1]<-s_now
for (j in 2:n) {
  m[1,j]<-m[1,j-1]*u
}

for (i in 2:n) {
  
  
  for (j in i:n) {
    
    m[i,j]<-m[i-1, j-1]*d
    
  }
}

if(Call_or_Put==1){

for (i in 1:n) {
  
  
  for (j in i:n) {
    
    m[i,j]<-max(0,(m[i,j]-strike_price))
    
  }
}


p<-matrix(nrow = n,ncol = n)

p[,n]<-m[,n]

for (jj in 1:(n-1)) {
  
  j<-n-jj
    
  for (i in 1:j) {
    
    p[i,j]<-max((exp(-r*dt)*(p1*p[i,j+1]+p2*p[i+1,j+1])),m[i,j])
    
    
  } 
    
  
}

EU<-matrix(nrow = n,ncol = n)

EU[,n]<-m[,n]

for (jj in 1:(n-1)) {
  
  j<-n-jj
  
  for (i in 1:j) {
    
    EU[i,j]<-exp(-r*dt)*(p1*EU[i,j+1]+p2*EU[i+1,j+1])
    
  } 
  
}


  cat("American call option price:", p[1,1])
  cat(sep = "\n")
  cat("European call option price:", EU[1,1])
}


  
  for (i in 1:n) {
    
    
    for (j in i:n) {
      
      m[i,j]<-max(0,(strike_price-m[i,j]))
      
    }
  }
  
  
  p<-matrix(nrow = n,ncol = n)
  
  p[,n]<-m[,n]
  
  for (jj in 1:(n-1)) {
    
    j<-n-jj
    
    for (i in 1:j) {
      
      p[i,j]<-max((exp(-r*dt)*(p1*p[i,j+1]+p2*p[i+1,j+1])),m[i,j])
      
      
    } 
    
    
  }
  
  EU<-matrix(nrow = n,ncol = n)
  
  EU[,n]<-m[,n]
  
  for (jj in 1:(n-1)) {
    
    j<-n-jj
    
    for (i in 1:j) {
      
      EU[i,j]<-exp(-r*dt)*(p1*EU[i,j+1]+p2*EU[i+1,j+1])
      
    } 
    
  }
  
  
  cat("American put option price:", p[1,1])
  cat(sep = "\n")
  cat("European put option price:", EU[1,1])
}



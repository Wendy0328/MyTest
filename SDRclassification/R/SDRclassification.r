
classification_by_SDR<-function(col_num,data){
  
  qq<-length(data)
  try<-data.frame()
  BOUND<-data.frame()
  fanbase<-list(data)
  order<-data.frame()
  for (j in 1:col_num) {
    cat("layer",j)
    cat(sep = "\n")
    cc<-2^(j-1)
    c<-2^j
    
    fan <- as.list(rep(1:c))
    length<-length(data)
    length<-length-1
    
    for (k in 1:cc) {
      
      q<-nrow(fanbase[[k]])
      
      if(q>3){
        
        
        playsdr<-data.frame()
        for (e in 1:length) {
          for (f in 2:(q-2)) {
            p<-fanbase[[k]][,c(e,qq)]
            p<-p[order(p[,1]), ]
            t<-p[,2]
            m<-p[c(1:f),2]
            n<-p[-c(1:f),2]
            
            SDR<-sd(t)-(f/q*sd(m)+(q-f)/q*sd(n))
            order<-rbind(order,SDR)
            
          }
          max<-which(order==max(order),arr.ind=TRUE)
          
          TRY<-order[max[1,1],1]
          MAX<-cbind(TRY,e)
          
          order<-data.frame()
          playsdr<-rbind(playsdr,MAX)
          
        }
        aa<-rep(1:length,times=1)
        playsdr<-cbind(playsdr,aa)
        colnames(playsdr)<-c("sdr","number")
        
        playsdr<-playsdr[order(playsdr$sdr,decreasing=TRUE), ]
        a<-playsdr[1,2]
        
        
        for (i in 2:(q-2)) {
          p<-fanbase[[k]][,c(a,qq)]
          p<-p[order(p[,1]), ]
          t<-p[,2]
          m<-p[c(1:i),2]
          n<-p[-c(1:i),2]
          
          SDR<-sd(t)-(i/q*sd(m)+(q-i)/q*sd(n))
          try<-rbind(try,SDR)
          
        }
        max<-which(try==max(try),arr.ind=TRUE)
        amount1<-p[max[,1]+1,1]
        amount2<-p[max[,1]+2,1]
        bound<-mean(amount1,amount2)
        BOUND<-rbind(BOUND,bound)
        cat("group",k,",criteria:column",a,",threshold:",bound)
        cat(sep = "\n")
        fan[[2*k-1]]<-fanbase[[k]][c(1:(max[,1]+1)),]
        fan[[2*k]]<-fanbase[[k]][-c(1:(max[,1]+1)),]
      }
      else{
        sub<-0
        sub<-data.frame(sub)
        
        fan[[2*k-1]]<-fanbase[[k]]
        fan[[2*k]]<-sub
      }
      try<-data.frame()
      
    }
    fanbase<-fan
    cat(sep = "\n")
  }
  
}




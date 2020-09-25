haplotypes<-function(table) {
  #Define the positions
  first<-seq(from=3,to=length(table[1,]), by=3)
  second<-seq(from=4,to=length(table[1,]),by=3)
  third<-seq(from=5,to=length(table[1,]),by=3)
  
  #Define the resulting dataframe
  results<-data.frame()
  for(i in 1:length(table[,1])) {
    if(length(results)==0) {
      results<-rbind(results,c(paste(table[i,1],1,sep="_"),table[i,2],table[i,first]))
    }
    else {
      results<-rbind(results,c(paste(table[i,1],1,sep="_"),as.character(table[i,2]),as.numeric(table[i,first])))
    }
    results<-rbind(results,c(paste(table[i,1],2,sep="_"),as.character(table[i,2]),as.numeric(table[i,second])))
    results<-rbind(results,c(paste(table[i,1],3,sep="_"),as.character(table[i,2]),as.numeric(table[i,third])))
  }
  return(results)
}

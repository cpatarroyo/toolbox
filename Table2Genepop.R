tab2genepop<-function(table,filen,abc=TRUE,sexratio=1,header=NULL) {
  
  #Define replacement function to replace 0 with "000" in the loci table
  replacement<-function(x) {
    if(x==0) {
      x<-"000"
    }
    else {
      x<-x
    }
  }
  
  #Check if the file already exists and erase it
  if(file.exists(filen)) {
    unlink(filen)
  }
  
  #Open the file connection
  filecon<-file(filen,open="a")
  
  #Extract the key information from the table
  loci<-grep('[0-9]',colnames(table),value=TRUE)
  populations<-levels(as.factor(table$Pop))
  
  #Make a temporary table without the population column and apply the replacement function
  preptable<-table[,-grep("Pop",colnames(table))]
  preptable$Ind<-paste(preptable$Ind,",",sep=" ")
  preptable<-apply(preptable,c(1,2),FUN=replacement)
  
  #Write the header for the file
  if(abc) {
    if(is.null(header) ) {
      writeLines(paste("Output Genepop file ","<NM=",sexratio,"NF>",sep=""),filecon)
    }
    else {
      writeLines(paste(header," <NM=",sexratio,"NF>",sep=""),filecon)
    }
  }
  else {
    if(is.null(header) ) {
      writeLines("Output Genepop file ",filecon)
    }
    else {
      writeLines(header,filecon)
    }
  }
  
  #Write the SSR markers
  for(h in loci) {
    if(abc) {
      writeLines(paste(h,"<H>",sep = "\t"),filecon)
    }
    else {
      writeLines(h,filecon)
    }
  }
  
  for(i in populations) {
    indpop<-grep(i,table$Pop)
    writeLines("POP",filecon)
    for(j in indpop) {
      writeLines(paste(preptable[j,],collapse="\t"),filecon)
    }
  }
  
  close(filecon)
  
}

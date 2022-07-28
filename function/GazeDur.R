
## Zeynep Gunes OZKAN 2022

data = raw_data2
gazedur <- function(data){
  
  for(a in 1:data$sub){
    
    SubjDat <- subset(data,data$sub == a)
    
    for(b in 1:SubjDat){
      
      itemDat <- subset(SubjDat,SubjDat$item == b)
      
      #itemDat <- head(itemDat,11)
      
      seq1 <- data.frame(matrix(ncol = 1, nrow = nrow(itemDat)))
      
      colnames(seq1) <- c('sequence')
      
      for(c in 1:nrow(itemDat)){
        if(!is.na(itemDat$wordN[c])){
          if(cummax(itemDat$wordN[c]) == as.numeric(itemDat$wordN[c])){
          
            seq1$sequence[c] <- 'first'
          
          }else{
          
            seq1$sequence[c] <- 'not first'
          
        }
      } 
    }#cum end
      
      seq1 <- rbind(seq1,seq1)
      
    }
  }

data <- cbind(data,seq1)

return(data)

}


#read data 
path <- "C:/Users/PC/Desktop/etou/"
fileNames <- dir(path) 
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})      
data <- lapply(filePath, function(x){
  read.csv(x,skip = 4,header = F)[,-1]})  

#analysis data
res=c()

for (i in 1:length(fileNames)){
  temp <- unlist(data[[i]])
  min.max.norm <- function(x){
    (x-min(x))
  }
  normalized_temp <- round(min.max.norm(temp),1)
  table_normalized_temp <- table(normalized_temp)
  prop_normalized_temp <- prop.table(table_normalized_temp)
  entropy <- prop_normalized_temp*log2(prop_normalized_temp)
  sum_entropy <- sum(entropy)*-1
  res=append(res,sum_entropy)
}

write.table(res,paste(path,"entropy.txt",sep = ""),sep = "\t",quote = F,row.names = F,col.names = F)


April <- list.files("C:\\data\\201504fut")
May <- list.files("C:\\data\\201505fut")
June <- list.files("C:\\data\\201506fut")
Header <- read.csv("C:\\data\\future_price_header.csv",sep = "\t")
for(i in 1:length(April))
{
  Filename <- paste("C:\\data\\201504fut",April[i],sep = "\\")
  Data <- read.table(Filename,sep = "\t")
  names(Data) <- names(Header)
  save(Data,file = paste(strsplit(Filename,".txt"),"binary",sep = "."))
}
for(i in 1:length(May))
{
  Filename <- paste("C:\\data\\201505fut",May[i],sep = "\\")
  Data <- read.table(Filename,sep = "\t")
  names(Data) <- names(Header)
  save(Data,file = paste(strsplit(Filename,".txt"),"binary",sep = "."))
}
for(i in 1:length(June))
{
  Filename <- paste("C:\\data\\201506fut",June[i],sep = "\\")
  Data <- read.table(Filename,sep = "\t")
  names(Data) <- names(Header)
  save(Data,file = paste(strsplit(Filename,".txt"),"binary",sep = "."))
}
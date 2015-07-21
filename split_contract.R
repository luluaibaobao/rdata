April <- list.files("C:\\data\\201504")
May <- list.files("C:\\data\\201505")
June <- list.files("C:\\data\\201506")
for(i in 1:length(April))
{
  Filename <- paste("C:\\data\\201504",April[i],sep = "\\")
  load(Filename)
  Contract_name <- unique(Data$CONTRACTID)
  for(j in 1:length(Contract_name))
  {
    Future_data <- Data[Data$CONTRACTID == Contract_name[j],]
    Future_data <- data.frame(TDATE = Future_data$TDATE,TTIME = Future_data$TTIME,UPDATEMILLISEC = Future_data$UPDATEMILLISEC,
                                CONTRACTID = Future_data$CONTRACTID,LASTPX = Future_data$LASTPX,HIGHPX = Future_data$HIGHPX,
                                LOWPX = Future_data$LOWPX,TQ = Future_data$TQ,TM = Future_data$TM,INITOPENINTS = Future_data$INITOPENINTS,
                                OPENINTS = Future_data$OPENINTS,S1 = Future_data$S1,B1 = Future_data$B1,OPENPX = Future_data$OPENPX)
    if(!dir.exists(paste("C:\\data\\",Contract_name[j],sep = "")))
      dir.create(paste("C:\\data\\",Contract_name[j],sep = ""))
    save(Future_data,file = paste(paste("C:\\data\\",Contract_name[j],sep = ""),April[i],sep = "\\"))
  }
}
for(i in 1:length(May))
{
  Filename <- paste("C:\\data\\201505",May[i],sep = "\\")
  load(Filename)
  Contract_name <- unique(Data$CONTRACTID)
  for(j in 1:length(Contract_name))
  {
    Future_data <- Data[Data$CONTRACTID == Contract_name[j],]
    Future_data <- data.frame(TDATE = Future_data$TDATE,TTIME = Future_data$TTIME,UPDATEMILLISEC = Future_data$UPDATEMILLISEC,
                       CONTRACTID = Future_data$CONTRACTID,LASTPX = Future_data$LASTPX,HIGHPX = Future_data$HIGHPX,
                       LOWPX = Future_data$LOWPX,TQ = Future_data$TQ,TM = Future_data$TM,INITOPENINTS = Future_data$INITOPENINTS,
                       OPENINTS = Future_data$OPENINTS,S1 = Future_data$S1,B1 = Future_data$B1,OPENPX = Future_data$OPENPX)
    if(!dir.exists(paste("C:\\data\\",Contract_name[j],sep = "")))
      dir.create(paste("C:\\data\\",Contract_name[j],sep = ""))
    save(Future_data,file = paste(paste("C:\\data\\",Contract_name[j],sep = ""),May[i],sep = "\\"))
  }
}
for(i in 1:length(June))
{
  Filename <- paste("C:\\data\\201506",June[i],sep = "\\")
  load(Filename)
  Contract_name <- unique(Data$CONTRACTID)
  for(j in 1:length(Contract_name))
  {
    Future_data <- Data[Data$CONTRACTID == Contract_name[j],]
    Future_data <- data.frame(TDATE = Future_data$TDATE,TTIME = Future_data$TTIME,UPDATEMILLISEC = Future_data$UPDATEMILLISEC,
                       CONTRACTID = Future_data$CONTRACTID,LASTPX = Future_data$LASTPX,HIGHPX = Future_data$HIGHPX,
                       LOWPX = Future_data$LOWPX,TQ = Future_data$TQ,TM = Future_data$TM,INITOPENINTS = Future_data$INITOPENINTS,
                       OPENINTS = Future_data$OPENINTS,S1 = Future_data$S1,B1 = Future_data$B1,OPENPX = Future_data$OPENPX)
    if(!dir.exists(paste("C:\\data\\",Contract_name[j],sep = "")))
      dir.create(paste("C:\\data\\",Contract_name[j],sep = ""))
    save(Future_data,file = paste(paste("C:\\data\\",Contract_name[j],sep = ""),June[i],sep = "\\"))
  }
}
trim <- function(raw_result,para)
{
  result <- subset(raw_result,CLASS !=0)
  result <- data.frame(TDATE = result$TDATE,TTIME = result$TTIME,UPDATEMILLISEC = result$UPDATEMILLISEC,
                       CONTRACTID = result$CONTRACTID,LASTPX = result$LASTPX,HIGHPX = result$HIGHPX,
                       LOWPX = result$LOWPX,TQ = result$TQ,TM = result$TM,INITOPENINTS = result$INITOPENINTS,
                       OPENINTS = result$OPENINTS,S1 = result$S1,B1 = result$B1,OPENPX = result$OPENPX,
                       CLASS = result$CLASS, VWAP = result$VWAP, VOL = result$VOL/para, MONEY = result$MONEY/para)
  return(result)
}
group<-function(rawdata,multiplier)
{
  data<-rawdata
  data$LASTPX<-as.numeric(as.character(rawdata$LASTPX))
  data$S1<-as.numeric(as.character(rawdata$S1))
  data$B1<-as.numeric(as.character(rawdata$B1))
  datalength<-length(data[,1])
  result <- vector()
  volume <- vector()
  trading_money <- vector()
  actual_price <-vector()
  result[1] <- 0 
  volume[1] <- 0 
  actual_price[1] <- data[1,]$LASTPX 
  trading_money[1] <-0
  for(i in 2:datalength)
  {
    vol <- data[i,]$TQ - data[i-1,]$TQ
    delta_oi <- data[i,]$OPENINTS - data[i-1,]$OPENINTS
    money <- data[i,]$TM - data[i-1,]$TM
    Mid <- (data[i-1,]$S1+data[i-1,]$B1)/2
    volume[i] <- vol
    trading_money[i] <- money
    if(data[i,]$TQ == 0 | vol == 0 | is.na(Mid))
    {
      result[i] <- 0
      actual_price[i] <-  actual_price[i-1]
      next
    }else
    {
      Vwap <- money/vol/multiplier
      actual_price[i] <- Vwap
    }
    if(abs(Vwap - Mid) < 1e-10)
    {
      Mid <- ((data[i-1,]$S1+data[i-1,]$B1)/2+(data[i-2,]$S1+data[i-2,]$B1)/2)/2
    }
    if(data[i,]$TQ == 0 | vol == 0 | is.na(Mid))
    {
      result[i] <- 0
      next
    }
    if(Vwap > Mid)
    {#buy init
      if((delta_oi > 0)&(delta_oi < vol))
      {
        result[i] <- 1
        next
      }
      if(delta_oi == 0)
      {
        result[i] <- 2
        next
      }
      if(delta_oi == vol)
      {
        result[i] <- 3
        next
      }
      if((delta_oi < 0)&(delta_oi > -vol))
      {
        result[i] <- 9
        next
      }
      if(delta_oi == -vol)
      {
        result[i] <- 10
        next
      }
    }
    if(Vwap < Mid)
    {#sell init
      if((delta_oi > 0)&(delta_oi < vol))
      {
        result[i] <- 6
        next
      }
      if(delta_oi == 0)
      {
        result[i] <- 7
        next
      }
      if(delta_oi == vol)
      {
        result[i] <- 8
        next
      }
      if((delta_oi < 0)&(delta_oi > -vol))
      {
        result[i] <- 4
        next
      }
      if(delta_oi == -vol)
      {
        result[i] <- 5
        next
      }
    }
  }
  temp <- data.frame(CLASS = result, VWAP = actual_price, VOL = volume, MONEY = trading_money)
  data <- cbind(data,temp)
  return(data)
}
Class <- function(ContractID, Date, Multiplier, Para)
{
  Dir <- paste("C:\\data",ContractID,sep = "\\")
  Filename <- paste(paste(Dir,Date,sep = "\\"),"binary",sep = ".")
  load(Filename)
  future_data$S1<-as.numeric(as.character(future_data$S1))
  future_data$B1<-as.numeric(as.character(future_data$B1))
  Datalength<-length(future_data[,1])
  Result <- vector()
  Volume <- vector()
  MONEY <- vector()
  Actual_price <-vector()
  Result[1] <- 0 
  Volume[1] <- 0
  MONEY[1] <-0
  Actual_price[1] <- future_data[1,]$LASTPX 
  for(i in 2:Datalength)
  {
    Vol <- (future_data[i,]$TQ - future_data[i-1,]$TQ)/Para
    Delta_oi <- (future_data[i,]$OPENINTS - future_data[i-1,]$OPENINTS)/Para
    Money <- (future_data[i,]$TM - future_data[i-1,]$TM)/Para
    Mid <- (future_data[i-1,]$S1+future_data[i-1,]$B1)/2
    Volume[i] <- Vol
    MONEY[i] <- Money
    if(future_data[i,]$TQ == 0 | Vol == 0 | is.na(Mid))
    {
      Result[i] <- 0
      Actual_price[i] <- Actual_price[i-1]
      next
    }else
    {
      Vwap <- Money/Vol/Multiplier
      Actual_price[i] <- Vwap
    }
    if(abs(Vwap - Mid) < 1e-10)
    {
      Mid <- ((future_data[i-1,]$S1+future_data[i-1,]$B1)/2+(future_data[i-2,]$S1+future_data[i-2,]$B1)/2)/2
    }
    if(is.na(Mid))
    {
      Result[i] <- 0
      next
    }
    if(Vwap > Mid)
    {#buy init
      if((Delta_oi > 0)&(Delta_oi < Vol))
      {
        Result[i] <- 1
        next
      }
      if(Delta_oi == 0)
      {
        Result[i] <- 2
        next
      }
      if(Delta_oi == Vol)
      {
        Result[i] <- 3
        next
      }
      if((Delta_oi < 0)&(Delta_oi > -Vol))
      {
        Result[i] <- 9
        next
      }
      if(Delta_oi == -Vol)
      {
        Result[i] <- 10
        next
      }
    }
    if(Vwap < Mid)
    {#sell init
      if((Delta_oi > 0)&(Delta_oi < Vol))
      {
        Result[i] <- 6
        next
      }
      if(Delta_oi == 0)
      {
        Result[i] <- 7
        next
      }
      if(Delta_oi == Vol)
      {
        Result[i] <- 8
        next
      }
      if((Delta_oi < 0)&(Delta_oi > -Vol))
      {
        Result[i] <- 4
        next
      }
      if(Delta_oi == -Vol)
      {
        Result[i] <- 5
        next
      }
    }
  }
  temp <- data.frame(CLASS = Result, VWAP = Actual_price, VOL = Volume, MONEY = MONEY)
  future_data <- cbind(future_data,temp)
  future_data <- subset(future_data,CLASS !=0)
  return(future_data)
}

Bucket_position <- function(ContractID, Date,Date_start,Date_end, Multiplier,Bucket,Para)
{
  Data <- Class(ContractID, Date, Multiplier, Para)
  Bucket_name <- vector()
  Position <- data.frame()
  Length <- length(Data[,1])
  Dir <- paste("C:\\data","Bucket_files",sep = "\\")
  Filename <- paste(Dir,"\\",paste(ContractID,Date_start,Date_end,sep = "-"),".binary",sep = "")
  load(Filename)
  Vol_border <- Detail$Vol_Max
  for(i in 1:Bucket)
  {
    Bucket_name[2*i-1] <- paste(paste('bucket',i,sep = ""),'long')
    Bucket_name[2*i] <- paste(paste('bucket',i,sep = ""),'short')
    Position[1,2*i-1] <-0
    Position[1,2*i] <-0
  }
  for(i in 1:Length)
  {
    Which_bucket <- 0
    for(j in 1:(Bucket-1))
    {
      if(Data[i,]$VOL <= Vol_border[j])
      {
        Which_bucket <- j
        break
      }else
      {
        next
      }
    }
    if(Which_bucket == 0)
    {
      Which_bucket <- Bucket
    }
    Position[i,] <- 0
    if((Data[i,]$CLASS) >=1 & (Data[i,]$CLASS)<=3)
    {
      Position[i,2*Which_bucket-1] <- Data[i,]$VOL
      next
    }
    if((Data[i,]$CLASS) >=4 & (Data[i,]$CLASS)<=5)
    {
      Position[i,2*Which_bucket-1] <- -Data[i,]$VOL
      next
    }
    if((Data[i,]$CLASS) >=6 & (Data[i,]$CLASS)<=8)
    {
      Position[i,2*Which_bucket] <- Data[i,]$VOL
      next
    }
    if((Data[i,]$CLASS) >=9 & (Data[i,]$CLASS)<=10)
    {
      Position[i,2*Which_bucket] <- -Data[i,]$VOL
      next
    }
  }
  names(Position) <- Bucket_name
  Data <- cbind(Data,Position)
  return(Data)
}

Bucket_position_moving <- function(ContractID, Date, Date_start, Date_end, Multiplier,Bucket,Para,Size)
{
  library("caTools")
  Data <- Bucket_position(ContractID, Date, Date_start,Date_end, Multiplier,Bucket,Para)
  Position <- data.frame(c(1:length(Data[,1])))
  for(i in 1:(2 * Bucket))
  {
    Position[,i] <- data.frame(runmean(Data[,(18+i)],Size,align = "right"))
  }
  for(i in 1:length(Data[,i]))
  {
    if(i < Size)
    {
      Position[i,] <- Position[i,] * i
    }else
    {
      Position[i,] <- Position[i,] * Size
    }
  }
  names(Position) <- names(Data[,19:(18+(2 * Bucket))])
  return(cbind(Data[,1:18],Position))
}
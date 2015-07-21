Get_data <- function(Contractid,Date_start,Date_end,Para)
{
  Vol_money <- data.frame()
  for(i in as.Date(Date_start,"%Y%m%d"):as.Date(Date_end,"%Y%m%d"))
  {
    Date <- format(as.Date(i,"1970-01-01"),"%Y%m%d")
    Dir <- paste("C:\\data",Contractid,sep = "\\")
    Filename <- paste(paste(Dir,Date,sep = "\\"),"binary",sep = ".")
    if(!file.exists(Filename))
    {
      next
    }
    load(Filename)
    Vol_money_1day <- data.frame()
    Vol_money_1day[1,1] <- 0
    Vol_money_1day[1,2] <- 0
    for(i in 2:length(future_data[,1]))
    {
      Vol_money_1day[i,1] <- (future_data[i,]$TQ - future_data[i-1,]$TQ)/Para
      Vol_money_1day[i,2] <- (future_data[i,]$TM - future_data[i-1,]$TM)/Para
    }
    Vol_money <- rbind(Vol_money,Vol_money_1day)
  }
  names(Vol_money) <- c("VOL","MONEY")
  Vol_money <- subset(Vol_money, VOL!=0)
  return(Vol_money)
}
Class_by_money <- function(data,Buckets)
{
  MONEY <- data$MONEY
  MONEY <- sort(MONEY)
  Tmoney <- sum(MONEY)
  Ave <- Tmoney/Buckets
  Sum <- 0
  Flag <- 1 
  Money_border <- vector()
  for(i in 1:length(MONEY))
  {
    Sum <- Sum + MONEY[i]
    if( Sum > Flag * Ave)
    {
      Money_border[Flag] <- MONEY[i-1]
      Flag <- Flag + 1
    }
  }
  return(Money_border)
}
SUM_money_class <- function(Contractid,Date_start,Date_end,Buckets,Para)
{
  Detail <- data.frame()
  Money_Vol <- Get_data(Contractid,Date_start,Date_end,Para)
  Border <- Class_by_money(Money_Vol,Buckets)
  Detail[1,1] <- sum(Money_Vol[Money_Vol$MONEY <= Border[1],]$MONEY)
  Detail[1,2] <- mean(Money_Vol[Money_Vol$MONEY <= Border[1],]$MONEY)
  Detail[1,3] <- min(Money_Vol[Money_Vol$MONEY <= Border[1],]$MONEY)
  Detail[1,4] <- max(Money_Vol[Money_Vol$MONEY <= Border[1],]$MONEY)
  Detail[1,5] <- sum(Money_Vol[Money_Vol$MONEY <= Border[1],]$VOL)
  Detail[1,6] <- mean(Money_Vol[Money_Vol$MONEY <= Border[1],]$VOL)
  Detail[1,7] <- min(Money_Vol[Money_Vol$MONEY <= Border[1],]$VOL)
  Detail[1,8] <- max(Money_Vol[Money_Vol$MONEY <= Border[1],]$VOL)
  #sum <- vector()
  #sum[1] <- sum(trading_money[trading_money <= Border[1]])
  for(i in 1:(Buckets-2))
  {
    Detail[i+1,1] <- sum(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$MONEY)
    Detail[i+1,2] <- mean(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$MONEY)
    Detail[i+1,3] <- min(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$MONEY)
    Detail[i+1,4] <- max(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$MONEY)
    Detail[i+1,5] <- sum(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$VOL)
    Detail[i+1,6] <- mean(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$VOL)
    Detail[i+1,7] <- min(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$VOL)
    Detail[i+1,8] <- max(Money_Vol[Money_Vol$MONEY > Border[i] & Money_Vol$MONEY <= Border[i+1],]$VOL)
    #sum[i+1] <- sum(trading_money[trading_money > Border[i] & trading_money <= Border[i+1]])
  }
  Detail[Buckets,1] <- sum(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$MONEY)
  Detail[Buckets,2] <- mean(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$MONEY)
  Detail[Buckets,3] <- min(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$MONEY)
  Detail[Buckets,4] <- max(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$MONEY)
  Detail[Buckets,5] <- sum(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$VOL)
  Detail[Buckets,6] <- mean(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$VOL)
  Detail[Buckets,7] <- min(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$VOL)
  Detail[Buckets,8] <- max(Money_Vol[Money_Vol$MONEY > Border[Buckets-1],]$VOL)
  #sum[Buckets] <- sum(trading_money[trading_money > Border[Buckets-1]])
  names(Detail) <- c('Money_Sum','Money_Mean','Money_Min','Money_Max','Vol_Sum','Vol_Mean','Vol_Min','Vol_Max')
  return(Detail)
}
get_data <- function(contractid,days,para)
{
  dir <- paste("C:\\data",contractid,sep = "\\")
  filename <- paste(paste(dir,days,sep = "\\"),"binary",sep = ".")
  load(filename)
  vol_money <- data.frame()
  vol_money[1,1] <- 0
  vol_money[1,2] <- 0
  for(i in 2:length(future_data[,1]))
  {
    vol_money[i,1] <- (future_data[i,]$TQ - future_data[i-1,]$TQ)/para
    vol_money[i,2] <- (future_data[i,]$TM - future_data[i-1,]$TM)/para
  }
  names(vol_money) <- c("VOL","MONEY")
  vol_money <- subset(vol_money, VOL!=0)
  return(vol_money)
}
Class_by_money <- function(data,buckets)
{
  MONEY <- data$MONEY
  MONEY <- sort(MONEY)
  Tmoney <- sum(MONEY)
  ave <- Tmoney/buckets
  sum <- 0
  flag <- 1 
  money_border <- vector()
  for(i in 1:length(MONEY))
  {
    sum <- sum + MONEY[i]
    if( sum > flag * ave)
    {
      money_border[flag] <- MONEY[i-1]
      flag <- flag + 1
    }
  }
  return(money_border)
}
SUM_money_class <- function(contractid,days,buckets,para)
{
  detail <- data.frame()
  Money_Vol <- get_data(contractid,days,para)
  border <- Class_by_money(Money_Vol,buckets)
  detail[1,1] <- sum(Money_Vol[Money_Vol$MONEY <= border[1],]$MONEY)
  detail[1,2] <- mean(Money_Vol[Money_Vol$MONEY <= border[1],]$MONEY)
  detail[1,3] <- min(Money_Vol[Money_Vol$MONEY <= border[1],]$MONEY)
  detail[1,4] <- max(Money_Vol[Money_Vol$MONEY <= border[1],]$MONEY)
  detail[1,5] <- sum(Money_Vol[Money_Vol$MONEY <= border[1],]$VOL)
  detail[1,6] <- mean(Money_Vol[Money_Vol$MONEY <= border[1],]$VOL)
  detail[1,7] <- min(Money_Vol[Money_Vol$MONEY <= border[1],]$VOL)
  detail[1,8] <- max(Money_Vol[Money_Vol$MONEY <= border[1],]$VOL)
  #sum <- vector()
  #sum[1] <- sum(trading_money[trading_money <= border[1]])
  for(i in 1:(buckets-2))
  {
    detail[i+1,1] <- sum(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$MONEY)
    detail[i+1,2] <- mean(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$MONEY)
    detail[i+1,3] <- min(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$MONEY)
    detail[i+1,4] <- max(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$MONEY)
    detail[i+1,5] <- sum(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$VOL)
    detail[i+1,6] <- mean(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$VOL)
    detail[i+1,7] <- min(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$VOL)
    detail[i+1,8] <- max(Money_Vol[Money_Vol$MONEY > border[i] & Money_Vol$MONEY <= border[i+1],]$VOL)
    #sum[i+1] <- sum(trading_money[trading_money > border[i] & trading_money <= border[i+1]])
  }
  detail[buckets,1] <- sum(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$MONEY)
  detail[buckets,2] <- mean(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$MONEY)
  detail[buckets,3] <- min(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$MONEY)
  detail[buckets,4] <- max(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$MONEY)
  detail[buckets,5] <- sum(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$VOL)
  detail[buckets,6] <- mean(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$VOL)
  detail[buckets,7] <- min(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$VOL)
  detail[buckets,8] <- max(Money_Vol[Money_Vol$MONEY > border[buckets-1],]$VOL)
  #sum[buckets] <- sum(trading_money[trading_money > border[buckets-1]])
  names(detail) <- c('Money_Sum','Money_Mean','Money_Min','Money_Max','Vol_Sum','Vol_Mean','Vol_Min','Vol_Max')
  return(detail)
}
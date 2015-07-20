MONEY_class<-function(data,groups)
{
  MONEY <- data$MONEY
  MONEY <- sort(MONEY)
  Tmoney <- sum(MONEY)
  ave <- Tmoney/groups
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
SUM_money_class <- function(data,groups)
{
  detail <- data.frame()
  border <- MONEY_class(data,groups)
  Money_Vol <- data.frame(MONEY = data$MONEY,VOL = data$VOL)
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
  for(i in 1:(groups-2))
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
  detail[groups,1] <- sum(Money_Vol[Money_Vol$MONEY > border[groups-1],]$MONEY)
  detail[groups,2] <- mean(Money_Vol[Money_Vol$MONEY > border[groups-1],]$MONEY)
  detail[groups,3] <- min(Money_Vol[Money_Vol$MONEY > border[groups-1],]$MONEY)
  detail[groups,4] <- max(Money_Vol[Money_Vol$MONEY > border[groups-1],]$MONEY)
  detail[groups,5] <- sum(Money_Vol[Money_Vol$MONEY > border[groups-1],]$VOL)
  detail[groups,6] <- mean(Money_Vol[Money_Vol$MONEY > border[groups-1],]$VOL)
  detail[groups,7] <- min(Money_Vol[Money_Vol$MONEY > border[groups-1],]$VOL)
  detail[groups,8] <- max(Money_Vol[Money_Vol$MONEY > border[groups-1],]$VOL)
  #sum[groups] <- sum(trading_money[trading_money > border[groups-1]])
  names(detail) <- c('Money_Sum','Money_Mean','Money_Min','Money_Max','Vol_Sum','Vol_Mean','Vol_Min','Vol_Max')
  return(detail)
}
bucket_position <- function(data,bucket)
{
  bucket_name <- vector()
  position <- data.frame()
  length <- length(data[,1])
  border <- MONEY_class(data,bucket)
  for(i in 1:bucket)
  {
    bucket_name[2*i-1] <- paste(paste('bucket',i,sep = ""),'long')
    bucket_name[2*i] <- paste(paste('bucket',i,sep = ""),'short')
    position[1,2*i-1] <-0
    position[1,2*i] <-0
  }
  for(i in 1:length)
  {
    which_bucket <- 0
    for(j in 1:(bucket-1))
    {
      if(data[i,]$MONEY <= border[j])
      {
        which_bucket <- j
        break
      }else
      {
        next
      }
    }
    if(which_bucket == 0)
    {
      which_bucket <-bucket
    }
    position[i+1,] <- position[i,]
    if((data[i,]$CLASS) >=1 & (data[i,]$CLASS)<=3)
    {
      position[i+1,2*which_bucket-1] <- position[i,2*which_bucket-1] + (data[i,]$VOL)
      next
    }
    if((data[i,]$CLASS) >=4 & (data[i,]$CLASS)<=5)
    {
      position[i+1,2*which_bucket-1] <- position[i,2*which_bucket-1] - (data[i,]$VOL)
      next
    }
    if((data[i,]$CLASS) >=6 & (data[i,]$CLASS)<=8)
    {
      position[i+1,2*which_bucket] <- position[i,2*which_bucket] + (data[i,]$VOL)
      next
    }
    if((data[i,]$CLASS) >=9 & (data[i,]$CLASS)<=10)
    {
      position[i+1,2*which_bucket] <- position[i,2*which_bucket] - (data[i,]$VOL)
      next
    }
  }
  position <- position[-1,]
  names(position) <- bucket_name
  data <- cbind(data,position)
  return(data)
}
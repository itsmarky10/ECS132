parta <- function(nreps){
  i <- 1
  result <- vector(length=nreps)
  x <- c(1,2,3)
  while( i <= nreps) 
  {
    sum <- 0;
    item <- 0;
    while(sum < 4)
    {
      toss = sample(x,1);
      if (toss + sum <= 4 )
      {
        item <- item + 1;
        sum <- sum + toss;
      }
      else {
        break;
      }
    }
    
    if (item == 3)
    {
      result[i] <- 1;
    }  
    else {   
      result[i] <- 0;
    }
    i <- i + 1;
  }
  
  average <- sum(result)/length(result)
  return(average)
}

partb <- function(nreps){
  i <- 0
  result <- numeric(nreps)
  x <- c(1,2,3)
  for (i in 1:nreps)
  {
    sum <- 0;
    item <- 0;
    while(sum < 4)
    {
      toss = sample(x,1);
      if (toss + sum <= 4 )
      {
        item <- item + 1;
        sum <- sum + toss;
      }
      else {
        break;
      }
    }
    
    if (sum < 4)
    {
      result[i] <- 1;
    }  else {
      
      result[i] <- 0;
    }
  }
  
  average <- sum(result)/length(result)
  return(average)
}

partc <- function(nreps){
  
  result <- numeric(nreps)
  x <- c(1,2,3)
  for (i in 1:nreps)
  {
    sum <- 0;
    item <- 0;
    while(sum <= 4)
    {
      toss = sample(x,1);
      if (toss + sum <= 4 )
      {
        item <- item + 1;
        sum <- sum + toss;
      }
      else {
        break;
      }
    }
    
    if (toss == 1)
    {
      result[i] <- 1;
    }  else {
      
      result[i] <- 0;
    }
    
  }
  
  average <- sum(result)/length(result)
  return(average)
}

partd <- function(nreps){
  
  result <- numeric(nreps)
  first <- numeric(nreps)
  x <- c(1,2,3)
  for (i in 1:nreps)
  {
    sum <- 0;
    item <- 0;
    while(sum <= 4)
    {
      
      toss = sample(x,1);
      
      if (item == 0)
      {
        first[i] = toss;
      }
      
      if (toss + sum <= 4 )
      {
        item <- item + 1;
        sum <- sum + toss;
      }
      else {
        break;
      }
    }
 
    if ( toss == 1)
    {
      result[i] <- 1;
    }  else {
      
      result[i] <- 0;
    }
    
  }
  
  LastTossOne = sum(result);
  count <- 0;
  
  for ( i in 1:nreps) {
    if ( result[i] == 1 && first[i] == 1)
    {
      count <- count + 1;
    }
  }
  
  return(count/LastTossOne)
}
parta <- function(nreps){
  x <- c(1,2,3,4,5,6)
  result <- numeric(nreps)
  
  for (i in 1:nreps)
  {
    jack <- 2
    toss = sample(x,1);
    if (jack + toss == 3)
    {
      jack <- jack + toss;
      toss = sample(x,1);
    }
    jack <- (jack + toss)%%8
    result[i] = (jack == 0);
  }
  
  return(sum(result)/nreps)

}

partb <- function(nreps){
  x <- c(1,2,3,4,5,6)
  result <- numeric(nreps)
  
  for (i in 1:nreps)
  {
    jack <- 2
    jill <- 0
    
    tossSum <- sample(x,1);
    toss2Sum <- sample(x,1);
    
    if (jill + toss2Sum == 3)
    {	
      toss2Sum <- sample(x,1) + toss2Sum;
      
    }
    
    if (jack + tossSum == 3)
    {
      tossSum <- sample(x,1) + tossSum;
    }
    
    result[i] <- ((toss2Sum - tossSum) >= 2)
    
    
  }
  
  return(sum(result)/nreps)
}

partc <- function(nreps){
  x <- c(1,2,3,4,5,6)
  result <- numeric(nreps)
  bonus <- numeric(nreps)
  
  for (i in 1:nreps)
  {
    jack <- 2
    jill <- 0
    bonus[i] <- 0
    tossSum <- sample(x,1);
    toss2Sum <- sample(x,1);
    
    if (jill + toss2Sum == 3)
    {	
      toss2Sum <- sample(x,1) + toss2Sum;
      bonus[i] <- 1
    }
    
    if (jack + tossSum == 3)
    {
      tossSum <- sample(x,1) + tossSum;
      bonus[i] <- 1
    }
    
    result[i] <- ((toss2Sum - tossSum) == 2)
    
  }
  count <- 0;
  res <- sum(result);
  for (i in 1:nreps)
  {
    
    if (result[i])
    {
      if (bonus[i] == 0)
        count <- count + 1
    }
  }
  
  return(count/res) 
}
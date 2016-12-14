parta <- function(nreps)
{
  dice <- c(1,2)
  dice2 <- c(1,2,3)
  result <- numeric(nreps)
  for (i in 1:nreps )
  {
    degree <- rep(1,3)
    
    choice1 <- sample(dice,1)
    
    if (choice1 == 1)
      degree[1] = degree[1] + 1
    else
      degree[2] = degree[2] + 1
    
    choice2 <- sample(dice2, 1, replace=TRUE, prob = c(degree[1]/4, degree[2]/4, degree[3]/4))
    
    result[i] = degree[choice2]
    
  }
  print(mean(result))
}

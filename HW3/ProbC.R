parta <- function(nreps)
{
  a <- numeric(nreps)
  for (i in 1:nreps )
  {
    
    a[i] =  sum(sample(0:2,3, replace=TRUE, prob<-c(0.5,0.4,0.1)))
  }
    
  distribution <- numeric(7)
  for (i in 0:6 )
  {
    distribution[i+1] = sum(a==i)/length(a)
  } 
  print(distribution)
}

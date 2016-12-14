sim <- function(nreps)
{
  shipment <- rpois(nreps,50)
  sum <- 0
  for (i in 1:nreps)
  {
    if(shipment[i] <= 42)
    {
      if(rbinom(1,shipment[i],0.05)==3)
        sum <- sum + 1
    }
  }
  return (sum/nreps)
}

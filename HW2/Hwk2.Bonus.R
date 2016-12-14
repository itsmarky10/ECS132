parta <-function(p, q, r, nreps)
{
	rat <- matrix(0, nreps, 2)
	y0x1 <- 1 - r/p
	y1x1 <- r/p
	y1x0 <- q*(1-r/q)/(1-p)
	y0x0 <- 1 - q*(1-r/q)/(1-p)
	
	for(i in 1:nreps)
	{
		rat[i,1] <- sample(1:0,1,prob=c(p,1-p))
		
		if (rat[i,1] == 1)
		{
			rat[i,2] <- sample(c(1:0),1,prob=c(y1x1,1-y1x1))
		}
		else
		{
			rat[i,2] <- sample(c(1:0),1,prob=c(y1x0,1-y1x0))
		}
	}
	r <- var(rat[,1]+rat[,2])
	return (r)
}
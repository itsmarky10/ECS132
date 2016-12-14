parta <- function(nreps)
{
	L1 <- numeric(nreps)
	L2 <- numeric(nreps)
	dice <- c(0,1,2)
    for (i in 1:nreps )
	{
		
		L1[i] <- sample(dice,1,replace=TRUE, prob <- c(0.5,0.4,0.1) )
		L2[i] <- L1[i]

		for( j in 1:L1[i] )
		{
		  if (runif(1) <= 0.2 && L1[i] > 0 )  	
		  	L2[i] <- L2[i] - 1
                  if (L2[i] < 0)
                    print(L1[i])
		}

		L2[i] <- L2[i] + sample(dice,1,replace=TRUE,prob <- c(0.5,0.4,0.1))
	}
	
	print(cov(L1,L2))
        
	print(mean(L2))
}

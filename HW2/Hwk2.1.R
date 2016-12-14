parta <- function(nreps)
{

result <- numeric(nreps)
x <- c(1,2,3)
for (i in 1:nreps)
{
	sum <- 0;
   while(sum <= 4)
   {
   		toss = sample(x,1);
   		if (toss + sum <= 4 )
   		{
			sum <- sum + toss;
   		}
   		else {
   			break;
   		}
 	}
	
	result[i] = toss

}

print(mean(result))
		
}


partb <- function(nreps)
{
result <- numeric(nreps)
x <- c(1,2,3)
for (i in 1:nreps)
{
	sum <- 0;
   while(sum <= 4)
   {
   		toss = sample(x,1);
   		if (toss + sum <= 4 ) {		
			sum <- sum + toss;
		}
		
   		else {
   			break;
   		}
 	}
	result[i] = toss
}

print( var(result))	
		
}
#' dgeomk function
#' 
#' p is the "success" probability 
#' k is the right end of the support of the distribution 
#' to find P(X = x) 
#
#
#' dgeomk(x, p, k)
dgeomk <- function(x, p, k)
{
  if (x>k || p>1)
    return (NaN)
  c <- 1/(1-(1-p)^k)
  x <- (c*(1-p)^(x-1)*p)
  return (x)	
}

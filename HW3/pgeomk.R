#' pgeomk function
#' 
#' p is the "success" probability 
#' k is the right end of the support of the distribution 
#' to find P(X <= x) 
#
#
#' pgeomk(x, p, k)
pgeomk <- function(x, p, k)
{
  if (x>k || p>1)
    return (NaN)
  c <- 1/(1-(1-p)^k)
  x <- c - c*(1-p)^x
  return (x)	
}

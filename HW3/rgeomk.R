#' rgeomk function
#' 
#' p is the "success" probability 
#' k is the right end of the support of the distribution 
#' to generate n independent values of X 
#
#
#' rgeomk(n, p, k)
rgeomk <- function(n, p, k)
{
  c <- 1/(1-(1-p)^k)
  return (as.integer(log(runif(n)/c,base = (1-p))))
}
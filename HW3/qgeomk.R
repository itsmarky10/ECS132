#' qgeomk function
#' 
#' p is the "success" probability 
#' k is the right end of the support of the distribution 
#' to find c such that P(X<=c) = q
#
#
#' qgeomk(q, p, k)
qgeomk <- function(q, p, k)
{
  if (q>1 ||p >1)
    return (NaN)
  c <- 1/(1-(1-p)^k)

  if ((log(1-q/c,base = 1-p) %% 1) == 0)
    return (log(1-q/c,base = 1-p))
  else
    return (as.integer(log(1-q/c,base = 1-p)) +0.5)	
}

inv.logit <- function(a) { # inverse logit function
  ans <- exp(a)/(1+exp(a))
  ans[is.nan(ans)] <- 1  # If a == Inf then ans should be 1
  return(ans)
}


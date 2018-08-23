illustrate_binom <- function(n, p, x = -1, q = 0, equal_to = FALSE){

  freqs <- data.frame(cbind(factor(0:n), sapply(0:n, function(x) dbinom(x, n, p))))
  names(freqs) = c("Value", "Prob")
  
  if(equal_to){
    freqs$Area = freqs$Value == x
  }else if(q != 0){
    freqs$Area = cumsum(freqs$Prob) <= q
  }else{
    freqs$Area = freqs$Value <= x
  }
  
  ggplot(freqs, aes(x = Value, y = Prob, fill = !Area)) + geom_col() + 
    ylab("Probability") + xlab("Number of successes") + ggtitle("Distribution of X") + 
    guides(fill=FALSE) + scale_x_continuous(breaks = 0:(n+1))
  
}

illustrate_binom(10, 0.5, q = .95)

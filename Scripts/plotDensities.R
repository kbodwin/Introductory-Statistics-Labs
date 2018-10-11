illustrate_binom <- function(n, p, x = 0, q = 0, equal_to = FALSE){

  n = as.integer(n)
  p = as.numeric(p)
  x = as.integer(x)
  q = as.numeric(q)
  
  freqs <- data.frame(cbind(0:n, sapply(0:n, function(x) dbinom(x, n, p))))
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

illustrate_norm <- function(mu, sigma, x = NA, q = NA){
  
  mu = as.numeric(mu)
  sigma = as.numeric(sigma)
  x = as.numeric(x)
  q = as.numeric(q)
  
  if(is.na(q) & is.na(x)){
    cutoff = mu - 4*sigma
    q = 0
  }else if(is.na(q)){
    cutoff = x
    q = round(pnorm(x, mu, sigma), 3)
  }else{
    cutoff = qnorm(q, mu, sigma)
  }
  
  ulim = max(cutoff, mu + 3*sigma) + sigma/2
  llim = min(cutoff, mu - 3*sigma) - sigma/2
  
  p <- ggplot(data = data.frame(x = c(llim, ulim)), aes(x)) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), lwd = 2) + 
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                  xlim = c(llim, cutoff),
                  geom = "area", fill = "blue", alpha = 0.5) +
    geom_vline(xintercept = cutoff, lwd = 1.5, col = "red") +
    ylab("") + xlab("values") +
    scale_y_continuous(breaks = NULL)
 
    
    if(q > 0.1){
      xpos <- cutoff - (cutoff-llim)/3
      ypos <- dnorm(xpos, mean = mu, sd = sigma)/3
    }else{
      xpos <- cutoff - (cutoff-llim)/2
      ypos <- dnorm(xpos, mean = mu, sd = sigma)*4
    }
    
    
    xbreaks = c(sigma*(-3:3) + mu, cutoff)
    xcols <- c(rep("black", 7), "red")
    
    p <- p + 
      annotate("text", x = xpos, y = ypos, label = paste0(q*100, "%"), size = 7, color = "red") +
      scale_x_continuous(breaks = xbreaks, labels = xbreaks) + 
      theme(axis.text.x = element_text(colour = xcols, size = c(rep(10, 7), 20)), panel.grid.minor = element_blank())
      
    
    p
  
}

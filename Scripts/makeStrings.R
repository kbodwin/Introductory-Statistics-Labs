

makePrintText <- function(base_string, old_input = "", new_input = ""){
  
  # new inputs evaluates first!
  
  red_inputs <- sapply(new_input, function(x) paste0("<font color='red'>", x, "</font>"))
  blue_inputs <- sapply(old_input, function(x) paste0("<font color='blue'>", x, "</font>"))
  
  print_string <- Reduce(function(x,y) sub("%ni", y, x), c(base_string, red_inputs))
  print_string <- Reduce(function(x,y) sub("%oi", y, x), c(print_string, blue_inputs))
  
  paste("<pre class='r'><code>", print_string, "</code></pre>")
  
}

makeEvalText <- function(base_string, old_input = "", new_input = ""){
  
  # old inputs evaluate first, can contain %ni's
  print_string <- Reduce(function(x,y) sub("%ni", y, x), c(base_string, new_input))
  print_string <- Reduce(function(x,y) sub("%oi", y, x), c(print_string, old_input))
  
  return(print_string)
  
}
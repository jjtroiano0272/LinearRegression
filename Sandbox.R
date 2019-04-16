err_term <- 1
gamma_val <- c(0.0001, 0.0100, 0.1000)
iteration_count <- 0

for (x in gamma_val) { # The variable gamma is global so the functions alpha & beta will be able to see it.
  if (err_term < 0.0005 | iteration_count > 2000) {
    break
  }
  
  # TODO: This is where the real algorithm runs.
  # TODO: Probably just paste the fuller version of alpha, beta, into here.
  
  iteration_count <- iteration_count + 1
  
  print("hi")
}
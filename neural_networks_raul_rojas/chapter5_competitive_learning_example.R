####################################
##  competitive_learning_example  ##
####################################

# Params
########
   
   iterations <- 100

# Data
######

   w <- c(-0.1, 0.1)
   x <- c(-1.3, -1.0, -0.7, 0.7, 1.0, 1.3)
   
# Iterate
#########
   
   for (iteration in 1:iterations)
   {
      cat("Iteration:", iteration, "\n")
      randomly_sampled <- sample(x, length(x), replace=FALSE)

      for (x_sampled in 1:length(randomly_sampled))
      {
         products <- randomly_sampled[x_sampled]*w
         max_index <- which(products==max(products))
         w[max_index] <- w[max_index] + 0.2*(randomly_sampled[x_sampled] - w[max_index])
      }
      print(w)
   }
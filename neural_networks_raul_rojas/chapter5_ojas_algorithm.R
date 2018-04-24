################################
##  chapter_5_ojas_algorithm  ##
################################

remove(list=ls())
library(animation)
ani.options(interval=1.2)

# Params
########

   w <- data.frame(x=runif(1,-1,1),y=runif(1,-1,1))
   lambda <- 0.05
   iterations <- 100
   slope <- 2.5
   
# Data
######
   
   x <- seq(-2, 2, by=0.005)
   y <- slope*x + rnorm(length(x),0,1)
   
   df <- data.frame(x=x,y=y)
   
# Oja's Algorithm
#################
   
   saveGIF({
   for (iter in 1:iterations)
   {

      # Plot the points
      plot(df$x, df$y, type="p", main=paste("Iteration", iter), col="black")
      arrows(0,0,as.numeric(w[1]),as.numeric(w[2]),col="red",lwd=5)
      abline(0,slope, col="blue",lwd=5)
      
      # Rotation Vector
      ind <- sample(x=1:nrow(df),1)
      this_vector <- as.vector(df[ind,])
      scal_prod <- sum(this_vector*w)
      adjust <- lambda*scal_prod*this_vector
      forget <- -lambda*scal_prod*scal_prod*w

      # The rotation vector
      arrows(w$x,
             w$y,
             w$x + adjust$x + forget$x, 
             w$y + adjust$y + forget$y, col="green", lwd=5)
      points(this_vector, col="dodgerblue", pch=8, cex=2, lwd=5)
      
      # Adjust and Forget
      w <- w + adjust + forget
      lamba <- lambda/2
      
   }
   }, "/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/ojas_algorithm_example.gif")
   


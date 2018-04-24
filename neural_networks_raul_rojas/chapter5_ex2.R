###############################################
##  chapter5_competitive_learning_algorithm  ##
###############################################

remove(list=ls())
source('/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/mnist.R')

# Input
#######

   clusters <- 10
   iterations <- 72000
   lambda <- 0.5
   
# Data
######
   
   load_mnist()
   
# References
############
   
   min_data_val <- min(mnist_train$x)
   max_data_val <- max(mnist_train$x)

# Starting Weights
##################

   sampled <- sample(1:nrow(mnist_train$x), size=clusters, replace=FALSE)
   weights <- mnist_train$x[sampled,]
   
   initial_weights <- weights
   
# Algorithm
###########

   for (iter in 1:iterations)
   {
      # Randomly choose a sample
      this_j <- sample(1:nrow(mnist_train$x), 1)
      this_sample <- mnist_train$x[this_j,]
      
      # Compute distances
      for (this_i in 1:nrow(weights))
      {
         this_weight <- weights[this_i,]
         this_dot <- -sum((this_sample-this_weight)^2)
         
         if (this_i == 1)
         {
            max_i <- 1
            max_dot <- this_dot
         }
         else if (this_dot > max_dot)
         {
            max_i <- this_i
            max_dot <- this_dot
         }
      }
      if ((iter %% 1000) == 0) print(iter)
      
      # Update
      weights[max_i,] <- weights[max_i,] + 
                         lambda*((iterations-iter)/iterations)*(this_sample - weights[max_i,])

   }
  
# The weights
#############
   
   par(mfrow=c(2,5), mai = c(1, 0.1, 0.1, 0.1))
   
   for (w in 1:nrow(weights))
      image(matrix(weights[w,], nrow=28)[,28:1], 
            col=gray(12:1/12), xaxt='n', yaxt='n', ann=FALSE)
   
# Predict
#########
   
   weight_rep <- vector(mode="integer",length=length(mnist_test$y))
   for (this_j in 1:nrow(mnist_test$x)){
      this_test <- mnist_test$x[this_j,]
      
      # Compute distances
      for (this_i in 1:nrow(weights))
      {
         this_weight <- weights[this_i,]
         this_dot <- -sum((this_test-this_weight)^2)
         
         if (this_i == 1)
         {
            max_i <- 1
            max_dot <- this_dot
         }
         else if (this_dot > max_dot)
         {
            max_i <- this_i
            max_dot <- this_dot
         }
      }
      
      # Prediction
      weight_rep[this_j] <- max_i
   }
   
# Show
######
   
   weight_to_show <- 10
   
   par(mfrow=c(2,5), mai = c(0.1, 0.1, 0.1, 0.1))
   
   image(matrix(weights[weight_to_show,], nrow=28)[,28:1], 
         col=gray(12:1/12), xaxt='n', yaxt='n', ann=FALSE)
   
   ind_samples <- sample(which(weight_rep == weight_to_show), 9)
   
   for (im in ind_samples){
      image(matrix(mnist_test$x[im,], nrow=28)[,28:1], 
            col=gray(12:1/12), xaxt='n', yaxt='n', ann=FALSE)
   }
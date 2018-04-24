####################
##  chapter5_ex5  ##
####################

remove(list=ls())
library(imager)
library(animation)
ani.options(interval=0.01)

# Params
########

   clusters <- 64
   codebook_dims <- data.frame(rows=8,cols=8)
   
   iterations <- 1000
   lambda <- 0.5

# Data In
#########

   dog_image <- load.image('/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/warrior_cat.jpg')
   train_image <- grayscale(dog_image)[,,1,1]
   train_image <- train_image[,ncol(train_image):1]
   image(train_image, axes = FALSE, col = grey(seq(0, 1, length = 256)))
   
# Rows and Cols
###############
   
   num_rows <- nrow(train_image)/8
   num_cols <- nrow(train_image)/8

# Starting Weights
##################

   weights <- data.frame(matrix(0,nrow=clusters,
                                ncol=codebook_dims$rows*codebook_dims$cols))
   
   for (w in 1:clusters)
   {
      sample_x <- sample(1:(num_rows-1),1)
      sample_y <- sample(1:(num_cols-1),1)
      temp <- train_image[(sample_x*8):(sample_x*8+7),
                          (sample_y*8):(sample_y*8+7)]
      
      weights[w,] <- as.vector(temp)
   }

# Algorithm
###########

   for (iter in 1:iterations)
   {
      # Randomly choose a sample
      this_x <- sample(1:(num_rows-1),1)
      this_y <- sample(1:(num_cols-1),1)
      
      this_sample <- train_image[(this_x*8):(this_x*8+7),
                                 (this_y*8):(this_y*8+7)]
      
      # Compute distances
      for (this_w in 1:nrow(weights))
      {
         this_weight <- weights[this_w,]
         this_dot <- -sum((this_sample-this_weight)^2)
         
         if (this_w == 1)
         {
            max_w <- 1
            max_dot <- this_dot
         }
         else if (this_dot > max_dot)
         {
            max_w <- this_w
            max_dot <- this_dot
         }
      }
      if ((iter %% 10) == 0) print(iter)
      
      # Update
      weights[max_w,] <- weights[max_w,] + 
                         lambda*((iterations-iter)/iterations)*
                         as.vector(this_sample - weights[max_w,])
      
   }
  
# Plot
######
   
   par(mfrow=c(8,8),
       mai = c(0.1, 0.1, 0.1, 0.1))
   
   for (w in 1:64)
   {
      w_mat <- matrix(as.vector(as.matrix(weights[w,])), nrow=8)
      image(w_mat, axes = FALSE, col = grey(seq(0, 1, length = 256)))
   }

# Reconstruct Image
###################
   
   reconstructed_image <- train_image
   
   par(mfrow=c(1,1))
   counter <- 1
   
   saveGIF({
   for (r in 1:(num_rows-1))
   {
      for (c in 1:(num_cols-1))
      {
         print(paste(counter, "of", (num_rows-1)*(num_cols-1)))
         counter <- counter+1
         
         image(reconstructed_image, axes = FALSE, col = grey(seq(0, 1, length = 256)))
         this_focus <- reconstructed_image[(r*8):(r*8+7),
                                           (c*8):(c*8+7)]
         
         # Compute distances
         for (this_w in 1:nrow(weights))
         {
            this_weight <- weights[this_w,]
            this_dot <- -sum((this_focus-this_weight)^2)
            
            if (this_w == 1)
            {
               max_w <- 1
               max_dot <- this_dot
            }
            else if (this_dot > max_dot)
            {
               max_w <- this_w
               max_dot <- this_dot
            }
         }
         
         this_sub <- matrix(as.vector(as.matrix(weights[max_w,])), nrow=8)
         reconstructed_image[(r*8):(r*8+7), (c*8):(c*8+7)] <- this_sub
         image(reconstructed_image, axes = FALSE, col = grey(seq(0, 1, length = 256)))
         ani.pause()
      }
      
   }
      
   }, "/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/reconstruction_warrior_cat.gif")

   
   par(mfrow=c(1,2),
       mai = c(0.1, 0.1, 0.1, 0.1))
   
   image(train_image, axes = FALSE, col = grey(seq(0, 1, length = 256)))
   image(reconstructed_image, axes = FALSE, col = grey(seq(0, 1, length = 256)))
   
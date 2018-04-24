###############################################
##  chapter5_competitive_learning_algorithm  ##
###############################################

remove(list=ls())
library(animation)
ani.options(interval=1.2)

# Input
#######

   clusters <- 6
   iterations <- 200
   lambda <- 0.5
   
# Data
######

   centers <- data.frame()
   centers <- rbind(centers, c(0,5))
   centers <- rbind(centers, c(5,5))
   centers <- rbind(centers, c(0,0))
   centers <- rbind(centers, c(5,0))
   
   sd <- data.frame()
   sd <- rbind(sd, c(3,3))
   sd <- rbind(sd, c(2,5))
   sd <- rbind(sd, c(5,2))
   sd <- rbind(sd, c(4,1))
   
   obs <- data.frame()
   obs <- rbind(obs, 300)
   obs <- rbind(obs, 300)
   obs <- rbind(obs, 300)
   obs <- rbind(obs, 300)
   
# X
###
   
   X <- data.frame()
   for (cluster in 1:nrow(centers)){
      temp <- data.frame(x1=rep(0,obs[cluster,]))
      for (var in 1:ncol(centers)){
         temp[[paste("x",var,sep="")]] <- rnorm(obs[cluster,],centers[cluster,var], sd[cluster,var])
      }
      X <- rbind(X, temp)
   }
   
# References
############
   
   min_data_val <- min(X)
   max_data_val <- max(X)

# Starting Weights
##################

   sampled <- sample(1:nrow(X), size=clusters, replace=FALSE)
   weights <- X[sampled,]
   
   initial_weights <- weights
   
# Algorithm
###########
   
   saveGIF({
   for (iter in 1:iterations)
   {
      # Randomly choose a sample
      this_j <- sample(1:nrow(X), 1)
      this_sample <- X[this_j,]
      
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
      
      ##  Plot  ##
      # The clusters
      if (ncol(centers) <=2){
         for (cluster in 1:nrow(centers))
         {
            if (cluster==1){
               start <- 1
               end <- obs[cluster,]
               plot(X[start:end,], col=cluster+1, 
                    xlim=c(min_data_val-1,max_data_val+1),
                    ylim=c(min_data_val-1,max_data_val+1),
                    main=paste("Iteration", iter))
            }
            else{
               start <- sum(obs[1:(cluster-1),])+1
               end <- sum(obs[1:(cluster-1),]) + obs[cluster,]
               points(X[start:end,], col=cluster+1)
            }
            
         }
      }
      
      # The point
      points(this_sample, col="black", pch=8, cex=2, lwd=5)
      
      # The real centers
      for (center in 1:nrow(centers))
      {
         points(centers[center,], col="black", pch=8, cex=4, lwd=5)
      }
      
      # The weights
      for (weight in 1:nrow(weights))
      {
         if (weight == max_i){ weight_col="red"} else { weight_col="brown"}
         points(weights[weight,], col=weight_col, pch=14+weight, cex=3)
      }
      
      # Update
      weights[max_i,] <- weights[max_i,] + 
                         lambda*((iterations-iter)/iterations)*(this_sample - weights[max_i,])
      ani.pause()
      
   }
   }, "/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/competitive_learning_algorithm_example.gif")

   
####################
##  chapter_5_3D  ##
####################

remove(list=ls())

# Points
########
   
   cluster_df <- data.frame(x=c(-2,-1,-2,-1,1,2,1,2)
                            , y=c(1,1,-1,-1,1,1,-1,-1))
   
# Weight Positions
##################
   
   w1 <- data.frame(x=-1,y=1)
   w2 <- data.frame(x=1,y=-1)
   
# Clusters
##########
   
   w1_cluster_df <- data.frame(x=vector(),y=vector())
   w2_cluster_df <- data.frame(x=vector(),y=vector())
   for (p in 1:nrow(cluster_df)){
      point <- cluster_df[p,]
      w1_dist <- (w1$x - point$x)^2 + (w1$y - point$y)^2
      w2_dist <- (w2$x - point$x)^2 + (w2$y - point$y)^2
      if (w1_dist < w2_dist){
         w1_cluster_df <- rbind(w1_cluster_df, point)
      } else if (w1_dist > w2_dist){
         w2_cluster_df <- rbind(w2_cluster_df, point)
      } else{
         w1_cluster_df <- rbind(w1_cluster_df, point)
         w2_cluster_df <- rbind(w2_cluster_df, point)
      }
   }
   
# Energy function
#################
   
   w_sampled <- seq(from=-2.6,to=2.6, by=0.02)
   w_sampled <- merge(data.frame(x=w_sampled)
                      , data.frame(y=w_sampled))
   energies <- vector()
   counter <- 0
   
   for (s in 1:nrow(w_sampled))
   {
      # Sample
      sample <- w_sampled[s,]
      
      # Where are we?
      counter <- counter+1
      print(paste(counter, "of", nrow(w_sampled)))
      
      # Set the energy
      this_energy_w1_cluster <- 0
      this_energy_w2_cluster <- 0
      
      # Compute squared distances
      for (p in 1:nrow(w1_cluster_df))
      {
         point <- w1_cluster_df[p,]
         dist <- (point$x - sample$x)^2 + (point$y - sample$y)^2
         this_energy_w1_cluster <- this_energy_w1_cluster + dist
      }
      
      for (p in 1:nrow(w2_cluster_df))
      {
         point <- w2_cluster_df[p,]
         dist <- (point$x - sample$x)^2 + (point$y - sample$y)^2
         this_energy_w2_cluster <- this_energy_w2_cluster + dist
      }
      
      # What weight are we closest to?
      energies <- c(energies, min(this_energy_w1_cluster, this_energy_w2_cluster))
   }
   
# Plot
######
   
   z <- matrix(energies, nrow=length(seq(from=-2.6,to=2.6, by=0.02)), byrow = FALSE)
   
   plot(c(cluster_df$x), c(cluster_df$y), xlim=c(-2.8,2.8),ylim=c(-2.8,2.8))
   points(c( w1$x, w2$x), c(w1$y, w2$y), col="red")
   contour(unique(w_sampled$x), unique(w_sampled$y), z, nlevels = 30, add=TRUE)
   
   
##################################
##  chapter_5_multidimensional  ##
##################################

remove(list=ls())

# Params
########

   cluster_sizes <- 100
   cluster_sd <- 0.25
   
# Cluster centers
#################

   cluster1_center_x1 <- -1
   cluster1_center_x2 <- 1
   
   cluster2_center_x1 <- 1
   cluster2_center_x2 <- -1

# cluster samples
#################

   cluster1_x1 <- rnorm(n=cluster_sizes, mean=cluster1_center_x1, sd=cluster_sd)
   cluster1_x2 <- rnorm(n=cluster_sizes, mean=cluster1_center_x2, sd=cluster_sd)

   cluster2_x1 <- rnorm(n=cluster_sizes, mean=cluster2_center_x1, sd=cluster_sd)
   cluster2_x2 <- rnorm(n=cluster_sizes, mean=cluster2_center_x2, sd=cluster_sd)
   
   plot(cluster1_x1, cluster1_x2, col="red", xlim=c(-2,2), ylim=c(-2,2))
   points(cluster2_x1, cluster2_x2, col="blue")
   
# Energy Function
#################
   
   library(rgl)
   
   w_sampled <- seq(from=-2,to=2, by=0.02)
   energies <- vector()
   counter <- 0
   for (w1 in w_sampled)
   {
      for (w2 in w_sampled)
      {
         counter <- counter+1
         print(paste(counter, "of", length(w_sampled)^2))
         
         d_cluster1 <- sum((c(w1,w2)-c(cluster1_center_x1,cluster1_center_x2))^2)
         d_cluster2 <- sum((c(w1,w2)-c(cluster2_center_x1,cluster2_center_x2))^2)
         
         this_energy <- 0
         if (d_cluster1 <= d_cluster2)
         {
            for (point in 1:cluster_sizes)
            {
               d_point <- sum((c(w1,w2)-c(cluster1_x1[point],cluster1_x2[point]))^2)
               this_energy <- this_energy + d_point
            }
         }
         else
         {
            for (point in 1:cluster_sizes)
            {
               d_point <- sum((c(w1,w2)-c(cluster2_x1[point],cluster2_x2[point]))^2)
               this_energy <- this_energy + d_point
            }
         }
         
         energies <- c(energies, this_energy)
      }  
   }
   
   plot3d(sort(rep(w_sampled, length(w_sampled))), 
          rep(w_sampled, length(w_sampled)), 
          energies,
          type="s",
          col="red")
######################################
##  chapter_3_basins_of_attraction  ##
######################################

w_sampled <- seq(from=-2,to=2, by=0.02)

cluster_1 <- c(-1.3,-1,-0.7)
cluster_2 <- c(1.3,1,0.7)

energies <- vector()
for (w in w_sampled)
{
   if (w < 0)
      distances <- (cluster_1 - w)^2
   else
      distances <- (cluster_2 - w)^2
   
   energy <- sum(distances)
   energies <- c(energies, energy)
   
}

plot(w_sampled, energies, type="l")

# Two-dimensional
#################

library(rgl)

energies <- vector()
counter <- 0
   for (alpha in w_sampled)
   {
      for (beta in w_sampled)
      {
         counter <- counter+1
         print(paste(counter, "of", length(w_sampled)^2))
         this_energy <- 0
         for (point in c(cluster_1, cluster_2)){
            if (abs(point - alpha) < abs(point - beta))
            {
               this_energy <- this_energy + abs(point-alpha)
            }
            else
            {
               this_energy <- this_energy + abs(point-beta)
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
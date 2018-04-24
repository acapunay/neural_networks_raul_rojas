####################
##  chapter6_ex1  ##
####################

options(scipen=999)
library(plotly)
library(scatterplot3d)

# Params
########

   iterations <- 50000
   dimension <- 2
   
# Inputs
########
   
   for (data_line in 1:dimension){
      if (data_line == 1){
         input <- data.frame(x1=c(0,1))
      }
      else{
         temp <- data.frame(x=c(0,1))
         names(temp) <- paste("x", data_line, sep="")
         input <- merge(input, temp)
      }
   }
   
   input[[paste("x",dimension+1,sep="")]] <- 1
   
# Functions
###########
   
   for (function_line in 1:(2^dimension)){
      temp <- rep(c(rep(0,2^(function_line-1)),rep(1,2^(function_line-1))), (2^(2^dimension))/(2^function_line))
      
      if (function_line==1){
         f_matrix <- matrix(temp, ncol=2^(2^dimension),byrow=TRUE)
      }
      else{
         f_matrix <- rbind(f_matrix, temp)
      }
   }
   
   funcs <- as.data.frame(f_matrix, row.names=FALSE)
   names(funcs) <- paste("f", 0:(2^(2^dimension)-1), sep="")
   
   
# Loop
######
   
   sizes <- data.frame(func=0:15,num_size=0)
   chosen_ws <- data.frame(x=rep(0,iterations),y=rep(0,iterations),z=rep(0,iterations),f=rep(-1,iterations))
   
   for (iter in 1:iterations)
   {
      
      if ((iter %% 1000) == 0) print(iter)
      # Random Weights
      w1 <- runif(1,-1,1)
      w2 <- runif(1,-sqrt(1-sum(w1^2)),sqrt(1-sum(w1^2)))
      w3 <- sample(c(-1,1),1)*sqrt(1-sum(c(w1,w2)^2))
      w <- sample(c(w1,w2,w3))
      
      # print(paste("w=", paste(w,collapse=",")))
      chosen_ws[iter,] <- c(w,-1)
      
      # What function?
      this_output <- t(t(input)*w)
      this_output <- as.numeric(rowSums(this_output) > 0)
      # print(paste("this_output=", paste(this_output,collapse=" ")))
      
      for (f in 0:(ncol(funcs)-1)){
         f_vals <- funcs[[paste("f",f,sep="")]]
         # print(paste("f_vals=", paste(f_vals,collapse=" ")))
         # print(paste("f_vals==this_output",all(f_vals == this_output)))
         if (all(f_vals == this_output))
         {
            # print(paste("chosen f=",f))
            sizes[f+1,"num_size"] <- sizes[f+1,"num_size"]+1
            chosen_ws[iter,"f"] <- f
            break
         }
      }
   }
   
# Results
#########
   
   sizes$relative_size <- (sizes$num_size/sum(sizes$num_size))*100
   sizes$area_estimate <- (sizes$num_size/sum(sizes$num_size))*4*pi
   chosen_ws$f <- as.factor(chosen_ws$f)
   plot_ly(chosen_ws, x=~x, y=~y, z=~z, color=~f)
   
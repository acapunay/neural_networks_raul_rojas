######################
##  chapter_3_ex_1  ##
######################

# Params
########

   dimension <- 2

# Functions
###########

   for (data_line in 1:dimension){
      if (data_line == 1){
         df <- data.frame(x1=c(0,1))
      }
      else{
         temp <- data.frame(x=c(0,1))
         names(temp) <- paste("x", data_line, sep="")
         df <- merge(df, temp)
      }
   }
   
   for (function_line in 1:(2^dimension)){
      temp <- rep(c(rep(0,2^(function_line-1)),rep(1,2^(function_line-1))), (2^(2^dimension))/(2^function_line))
      
      if (function_line==1){
         f_matrix <- matrix(temp, ncol=2^(2^dimension),byrow=TRUE)
      }
      else{
         f_matrix <- rbind(f_matrix, temp)
      }
   }
   
   f_df <- as.data.frame(f_matrix, row.names=FALSE)
   names(f_df) <- paste("f", 1:(2^(2^dimension)), sep="")
   
   df <- cbind(df, f_df)
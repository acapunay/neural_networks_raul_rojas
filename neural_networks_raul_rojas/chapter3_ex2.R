   ######################
   ##  chapter_3_ex_2  ##
   ######################
   
   # Params
   ########
   
      dimension <- 4
   
   # Functions
   ###########
   
      for (data_line in 1:dimension){
         if (data_line == 1){
            df <- data.frame(x1=c(-1,1))
         }
         else{
            temp <- data.frame(x=c(-1,1))
            names(temp) <- paste("x", data_line, sep="")
            df <- merge(df, temp)
         }
      }
      
      for (function_line in 1:(2^dimension)){
         temp <- rep(c(rep(-1,2^(function_line-1)),rep(1,2^(function_line-1))), (2^(2^dimension))/(2^function_line))
         
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
      
   # Build the models
   ##################
      
      values <- as.matrix(df[,c(1:dimension)])
      
      for (model in 1:(2^dimension))
      {
         model_params <- as.numeric(df[model,1:dimension])
         model_values <- matrix(rep(model_params,2^dimension), nrow=2^dimension, byrow=TRUE)
         
         df[[paste("m",model,sep="")]] <- rowSums(values*model_values)
      }
      
   # Delete copies
   ###############
      
      delete_cols <- vector()
      for (model in 1:((2^dimension)-1))
      {
         model_vals <- as.numeric(df[[paste("m",model,sep="")]])
         for (sec_model in (model+1):(2^dimension))
            {
               sec_model_vals <- as.numeric(df[[paste("m",sec_model,sep="")]])
               if (all.equal(model_vals,-sec_model_vals) == TRUE)
               {
                  delete_cols <- c(delete_cols, sec_model)
               }
            }
      }
      
      delete_cols <- unique(delete_cols)
      
      for (col in delete_cols)
      {
         df[[paste("m",col,sep="")]] <- NULL
      }
      
   # Count them
   ############
      
      counter <- 0
      for (func in 1:(2^(2^dimension)))
      {
         cat("\n", paste(func,"of",2^(2^dimension)))
         func_computable <- FALSE
         
         for (model in 1:((2^dimension) - length(delete_cols)))
         {
            
            if (func_computable) break
            
            temp_df <- df[,c(paste("f",func,sep=""), paste("m",model,sep=""))]
            
            # print(temp_df)
            
            greater_than <- temp_df[temp_df[[paste("m",model,sep="")]] > 0, paste("f",func,sep="")]
            equal_to <- temp_df[temp_df[[paste("m",model,sep="")]] == 0, paste("f",func,sep="")]
            less_than <- temp_df[temp_df[[paste("m",model,sep="")]] < 0, paste("f",func,sep="")]
            
            # print(paste("greater:", paste(greater_than, collapse=",")))
            # print(paste("equal:", paste(equal_to, collapse=","), "; length=", length(equal_to)))
            # print(paste("less_than:", paste(less_than, collapse=",")))
            
            unique_greater_than <- unique(greater_than)
            unique_equal_to <- unique(equal_to)
            unique_less_than <- unique(less_than)
            
            if (length(unique_greater_than) == 1 &&
                (length(unique_equal_to) == 1  | length(equal_to) == 0) && 
                length(unique_less_than) == 1 &&
                unique_greater_than == -unique_less_than)
            {
               counter = counter+1
               func_computable <- TRUE
               cat(" YES: ", counter)
            }
         }
      }
      
      print(counter)
      
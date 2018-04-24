
remove(list=ls())

library(jpeg)
library(png)

# Initialize Random Weights
weights <- matrix(runif(min=0,max=0.1,n=9), nrow=3)
cons_weight <- runif(min=0,max=0.1,n=1)

# Get Input/Output
img_matrix <- readPNG('/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/portugal_flag.png')
input <- img_matrix[,,1]
output <- readRDS('/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/output_sample.rds')

# Sample
sample_order <- sample(nrow(output)*ncol(output), replace = FALSE)

# Iterations
iterations <- 100000

for (iter in 1:iterations)
{
   print(iter)
   # This sample's row and col
   this_sample <- sample_order[iter]
   this_row <- (this_sample %/% ncol(output))
   if (this_row == 0) this_row <- 1
   this_col <- (this_sample %%  ncol(output)) 
   if (this_col == 0) this_col <- ncol(output)
   
   # This sample's output
   this_output <- output[this_row,this_col]
   
   # Params for matrix that we need
   this_start_row <- this_row + 1 - 1
   this_end_row <- this_row + 1 + 1
   this_start_col <- this_col + 1 - 1
   this_end_col <- this_col + 1 + 1
   
   # Here's the matrix
   this_matrix <- input[this_start_row:this_end_row,
                        this_start_col:this_end_col]
   
   # Perceptron output
   perc_output <- sum(this_matrix*weights) + cons_weight
   
   if (this_output == 1 && perc_output > 0)
   {
      next
   }
   else if (this_output == 1 && perc_output <= 0)
   {
      weights <- weights + this_matrix
      cons_weight <- cons_weight + 1
   }
   else if (this_output == 0 && perc_output < 0)
   {
      next
   }
   else
   {
      weights <- weights - this_matrix
      cons_weight <- cons_weight - 1
   }
}

# Test
#######

test_matrix <- readPNG('/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/winnie.png')

operator <- weights

test_matrix <- test_matrix[,,1]

edge_values <- vector()
for (row in 2:(nrow(test_matrix)-1))
{
   print(paste(row, "of", nrow(test_matrix)-2))
   start_row <- row-1
   end_row <- row+1
   for (col in 2:(ncol(test_matrix)-1))
   {
      start_col <- col-1
      end_col <- col+1
      
      temp_matrix <- test_matrix[start_row:end_row,start_col:end_col]
      edge_value <- as.numeric(sum(operator*temp_matrix)>cons_weight)
      
      edge_values <- c(edge_values, edge_value)
   }
}

edge_matrix <- matrix(edge_values, byrow=TRUE, nrow=nrow(test_matrix)-2)

image(t(test_matrix))
image(t(edge_matrix), col = grey(seq(0, 1, length = 256)))

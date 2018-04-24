library(jpeg)
library(png)

# Params
########

   threshold <- 0.1
   
# Data In
#########

   img_matrix <- readPNG('/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/winnie.png')

   operator <- matrix(c(1,1,1,1,0,1,1,1,1), nrow=3)
   neighborhood_size <- length(operator[operator != 0])

   img_matrix1 <- img_matrix[,,1]

# Computation
#############
   
   edge_values <- vector()
   for (row in 2:(nrow(img_matrix1)-1))
   {
      print(paste(row, "of", nrow(img_matrix1)-2))
      start_row <- row-1
      end_row <- row+1
      for (col in 2:(ncol(img_matrix1)-1))
      {
         start_col <- col-1
         end_col <- col+1
         
         temp_matrix <- img_matrix1[start_row:end_row,start_col:end_col]
         
         prod_values <- operator*temp_matrix
         prod_values <- prod_values[operator != 0]
         geometric_mean <- as.numeric(prod(prod_values)^(1/neighborhood_size))
         photoreceptor_luminosity <- as.numeric(img_matrix1[row,col])
         
         relative_luminosity <- log(photoreceptor_luminosity/geometric_mean)
         edge_value <- as.numeric(relative_luminosity > threshold)
         
         edge_values <- c(edge_values, edge_value)
      }
   }

edge_matrix <- matrix(edge_values, byrow=TRUE, nrow=nrow(img_matrix1)-2)

image(t(img_matrix1))
image(t(edge_matrix), col = grey(seq(1, 0, length = 256)))
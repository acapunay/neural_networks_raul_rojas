library(jpeg)
library(png)

img_matrix <- readPNG('/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/portugal_flag.png')

operator <- matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1), nrow=3)

img_matrix1 <- img_matrix[,,1]

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
      edge_value <- as.numeric(sum(operator*temp_matrix)>0)
      
      edge_values <- c(edge_values, edge_value)
   }
}

edge_matrix <- matrix(edge_values, byrow=TRUE, nrow=nrow(img_matrix1)-2)

image(t(img_matrix1))
image(t(edge_matrix), col = grey(seq(1, 0, length = 256)))

# Save
saveRDS(img_matrix1,
        '/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/input_sample.rds')
saveRDS(edge_matrix,
        '/Users/acapunay/Google Drive/statistical_learning/neural_networks_raul_rojas/output_sample.rds')
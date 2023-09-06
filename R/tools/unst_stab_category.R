unst_stab_category <- function (z_L) {
# Classifying data into stability categories for the purpose of box plotting
# The categories are as follows:
# z/L = -10 to -1 = category 1
# z/L = -1 to -0.5 = category 2
# z/L = -0.5 to -0.1 = category 3
# z/L = -0.1 to -0.05 = category 4
# z/L = -0.05 to 0 = category 5
# z/L =  0 to 0.05 = category 6
# z/L =  0.05 to 0.10 = category 7
# z/L =  0.10 to 0.50 = category 8
# z/L =  0.50 to 1.00 = category 9
# z/L =  1 to 10 = category 10
  
  if (is.na(z_L)) stability_no <- NA
  else if (z_L >= -10.0 & z_L < -1.00) stability_no <- 1
  else if (z_L >= -1.00 & z_L < -0.50) stability_no <- 2
  else if (z_L >= -0.50 & z_L < -0.10) stability_no <- 3
  else if (z_L >= -0.10 & z_L < -0.05) stability_no <- 4
  else if (z_L >= -0.05 & z_L < 0.00) stability_no <- 5
  else if (z_L >= 0.00 & z_L < 0.05) stability_no <- 6
  else if (z_L >= 0.05 & z_L < 0.10) stability_no <- 7
  else if (z_L >= 0.10 & z_L < 0.50) stability_no <- 8
  else if (z_L >= 0.50 & z_L < 1.00) stability_no <- 9
  else if (z_L >= 1.00 & z_L < 10.0) stability_no <- 10
  
  return(stability_no)

}



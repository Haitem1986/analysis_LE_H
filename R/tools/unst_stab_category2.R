unst_stab_category2 <- function (z_L) {
# Classifying data into unstable or stable categories
# If stability_no = 1,2,3,4,5 -> unstable
# If stability_no = 6,7,8,9,10 -> stable
  
  if (is.na(z_L)) stability <- NA
  else if (z_L >= -10.0 & z_L < -1.00) stability <- 'Unstable'
  else if (z_L >= -1.00 & z_L < -0.50) stability <- 'Unstable'
  else if (z_L >= -0.50 & z_L < -0.10) stability <- 'Unstable'
  else if (z_L >= -0.10 & z_L < -0.05) stability <- 'Unstable'
  else if (z_L >= -0.05 & z_L < 0.00) stability <- 'Unstable'
  else if (z_L >= 0.00 & z_L < 0.05) stability <- 'Stable'
  else if (z_L >= 0.05 & z_L < 0.10) stability <- 'Stable'
  else if (z_L >= 0.10 & z_L < 0.50) stability <- 'Stable'
  else if (z_L >= 0.50 & z_L < 1.00) stability <- 'Stable'
  else if (z_L >= 1.00 & z_L < 10.0) stability <- 'Stable'
  
  return(stability)
}


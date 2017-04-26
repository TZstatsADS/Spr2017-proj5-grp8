feature <- function(img_dir) {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only grayscale features.
  
  n_files <- length(list.files(img_dir))
  file_names <- list.files(img_dir, pattern = "[[:digit:]].jpg")
  #file_names <- sort(file_names)
  file_paths <- rep(NA_character_, length(file_names))
  for (i in 1:length(file_names)) {
    file_paths[i] <- paste(img_dir, file_names[i], sep = "/")
  }
  #file_paths <- sort(file_paths)
  
  rgb_feature <- matrix(NA, nrow = 256, ncol = n_files)
  
  #       constructed for those images
  for (i in 1:n_files) {
    tryCatch({
      img_rgb <- readImage(file_paths[i])
      mat <- imageData(img_rgb)
      n <- 256
      nBin <- seq(0, 1, length.out = n)
      freq_rgb <- as.data.frame(table(factor(findInterval(mat, nBin), levels = 1:n)))
      rgb_feature[,i] <- as.numeric(freq_gray$Freq)/(ncol(mat)*nrow(mat))
    }, 
    error = function(c) "invalid or corrupt JPEG file")
  }
  return(rgb_feature)
}
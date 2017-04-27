library("EBImage")

### Count number of images
n_files <- length(list.files(img_dir_full))

### RGB feature extraction prep 
nR <- 8 
nG <- 8
nB <- 8 # Caution: the bins should be consistent across all images!
rBin <- seq(0, 1, length.out=nR)
gBin <- seq(0, 1, length.out=nG)
bBin <- seq(0, 1, length.out=nB)
mat=array()
freq_rgb=array()
rgb_feature=matrix(nrow=n_files, ncol=nR*nG*nB)


### Extract 800 RGB features

img_dir_full <- "/Users/xuehan/Desktop/Images/" #local path of 3000 images
file.names <- list.files(img_dir_full,pattern="*.jpg") 
for (i in 1:10){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}


for (i in 11:500){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}

for (i in 501:1000){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}


for (i in 1001:1500){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}


for (i in 1501:2000){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}

for (i in 2001:2500){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}

for (i in 2501:3000){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}

for (i in 3001:3008){
  mat <- imageData(resize(readImage(paste0(img_dir_full, "/",file.names[[i]])),200,200))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), factor(findInterval(mat[,,2], gBin), levels=1:nG), factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
}
### Rename features
colnames(rgb_feature) <- paste0("RGB",1:512)

RGB_Feature <- as.data.frame(rgb_feature)

### output constructed features
img_dir_rgb<-"../output"
save(RGB_Feature, file=paste0(img_dir_rgb,"/RGB_Feature.RData"))
RGB_Feature

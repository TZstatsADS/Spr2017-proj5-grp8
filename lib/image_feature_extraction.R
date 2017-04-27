
library(EBImage)
#install.packages("jpeg")
library(jpeg)
library(OpenImageR)

##########Your local path of 3000 images#################
img_dir_full <- "/Users/xuehan/Desktop/Images/" 
########################################################

###Extract HoG Features from images


source("../lib/HOGFeature.R")


h<-hogfeature(img_dir_full,export=T)
h<-data.frame(h)

file.names <- list.files(img_dir_full,pattern="*.jpg") 


file.names.short<-NULL
for (i in 1:length(file.names)){
  file.names.short[i]<-substr(file.names[[i]],1,nchar(file.names[[i]])-4)
}

h$id<-file.names.short
write.csv(h,file="../data/HoG.csv")
#HoG.csv is under the doc 




###Extract RGB Features from images 

source("../lib/RGBfeature.R")
#dir <- "../data/"
#img_dir_full <- paste(dir, "Images_full", sep="")

rgb_feature<-rgbfeature(img_dir_full,file.names)

rgb<-data.frame(rgb_feature)

rgb$id<-file.names.short

write.csv(rgb,file="../data/RGB.csv") #stored under doc




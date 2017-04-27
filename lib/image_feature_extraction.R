title: "Image Anlysis"
author: "Xuehan Liu (xl2615)"
date: "4/21/2017"
output: pdf_document

library(EBImage)
#install.packages("jpeg")
library(jpeg)
library(OpenImageR)



###Extract HoG Features from images


source("../lib/HOGFeature.R")
#dir <- "../data/"
#img_dir_full <- paste(dir, "Images_full/", sep="")

img_dir_full <- "/Users/xuehan/Desktop/Images/" #local path of 3000 images
h<-hogfeature(img_dir_full,export=T)
h<-data.frame(h)

file.names <- list.files(img_dir_full,pattern="*.jpg") 
#file.names

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

rgb<-rgbfeature(img_dir_full,file.names)

rgb<-data.frame(rgb)

rgb$id<-file.names.short

write.csv(rgb,file="RGB.csv") #stored under doc




setwd("../doc")


AnimalFeature = function(description_vector, title_vector)
{
  animal_list_filename = "../data/animals.txt"
  animal_df = as.character(read.delim(animal_list_filename, header=FALSE, sep = "\n")[,1])
  animal_df = tolower(animal_df)
  description_vector = tolower(description_vector)
  title_vector = tolower(title_vector)
  word_list_description = strsplit(description_vector, split = " ")
  word_list_title = strsplit(title_vector, split = " ")
  animal_feature = rep(0, length(description_vector))
  for(i in 1:length(animal_feature))
  {
    if(sum(as.numeric(is.element(animal_df, as.list(word_list_description[[i]])))) >= 1)
    {
      animal_feature[i] = 1
    }
    if(sum(as.numeric(is.element(animal_df, as.list(word_list_title[[i]])))) >= 1)
    {
      animal_feature[i] = 1
    }
    
  }
  
  return(animal_feature)
}
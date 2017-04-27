setwd("./") # Set working directory to doc folder
library(dplyr)
library(glm)

data = read.csv("../data/data_full.csv")
data$ID = as.character(data$ID)
id_split = strsplit(data$ID, "_")
auction_id_vector = rep(NA, length(id_split))
item_id_vector = rep(NA, length(id_split))
for(i in 1:length(id_split))
{
  auction_id_vector[i] = id_split[[i]][1]
  item_id_vector[i] = id_split[[i]][2]
}
data$auction_id = auction_id_vector
data$item_id = item_id_vector


auction_data = read.csv("../data/auctionItems.csv")
auction_data$auction_id = auction_data$auctionId
auction_data$item_id = auction_data$id
auction_data = select(auction_data, auction_id, item_id, salePrice)

merged_data = merge(data, auction_data)
merged_data = select(merged_data, -auction_id, -item_id, -ID)

test_rows = sample(1:nrow(merged_data), .25*nrow(merged_data))
test_data = merged_data[test_rows,]
train_data = merged_data[-test_rows,]
linear_model = lm(salePrice ~ ., train_data)
linear_predict = predict(linear_model, test_data)


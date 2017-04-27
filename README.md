# ADS Project 5: 

Term: Spring 2017

+ Team 8
+ Projec title: Auction Prices Prediction
+ Team members
	+ team member 1 : Vikas Arun (va2298)
	+ team member 2 : Xuehan Liu (xl2615)
	+ team member 3 : Sean Reddy (sr3336)
	+ team member 4 : Boxuan Zhao (bz2290)
+ Project summary: In this project, our team tried to predict the price of rare items that are auctioned through the well known website Sotheby's. Inspiration for the project comes from the realization that the predicted sales price posted on the Sotheby's website is not particularly acurate. We decided to focus our efforts on predicting the price for painting items. We scraped alll the relevant information from the Sotheby's website (no API is provided unforunately), and we  performed basic NLP on the description, title, and other text information provided as well as some basic image analysis the picture of the painting. We also attempted to use the VGG pretrained model from the tensor flow library, but didn't use this in the end due to the large running time for VGG to produce features. We initially used basic linear regression to predict the price, but the variance was incredibly high, so we used lasso regression to only select ~15 features that were good predictors, and the resulting mean residual error was about $6000. We think this is quite good as our goal was not to predict the exact price, but rather give a price that is in the appropriate range for bidders to decide whether they would like to bid on an item.
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.

Hello!

This is my personal machine learning classifier tester function in R. Right now, it only carries the functions for conditional trees ("ctree"), conditional tree random forests ("cforest"), random forests("randomForest"), and untuned Support Vector Machines ("svm"). Feeding "tune=TRUE" into the parameters while using the "cforest" and "randomForest" treatments will run the "tuneRF" function. 

Because R maneuvers off of physical memory, there is an upperbound limit to the size of the data frames that it can run through. For now, it can work well with some of the data sets in the dataset package in R.

I definitely recommend using RStudio to work with this function.

-Dean

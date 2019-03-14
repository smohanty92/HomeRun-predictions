#import e1071 library
library("e1071")

#Calculate RMSE function
rmse <- function(error) {
    sqrt(mean(error^2))
}

#Read our data in
baseball.df <- read.csv("baseball.csv")

#Number of iterations for bootstrapping
b <- 200

#Initialize error array of size b
err <- array(dim=b)

for (i in 1:b) {

	#Sample baseball.df with replacement of same length |baseball.df|
	sampleD = sample(nrow(baseball.df), replace=TRUE)
	B = baseball.df[sampleD,]
	
	#Hold-Out Method
	#Perform an 80/20 split of sampleD into a training and testing set
	split = sample(nrow(B),(nrow(B)*0.8))
	train = B[split,]
	test = B[-split,]
	
	#create svm model
	model <- svm(baseball.df$HR ~ .,
				data=baseball.df,
				type="eps-regression",
				kernel="radial",
           		scale=FALSE,
				cost=1000,
				gamma=1,
            	epsilon=0.1)
            	
    #train model on training set
    pred = predict(model, test)

	#Calculate error of model trained on 'train' tested on 'test'
	error <- baseball.df$HR - pred
	
	#Calculate the RMSE
	predictedRMSE <- rmse(error)

	#Store RMSE into err array
	err[i] = predictedRMSE
	
}

#Sort err in ascending fashion
err <- sort(err)

print(err)

#extract lower and upper bounds for a 95% confidence interval
lb <- err[5]
ub <- err[195]

cat("lower bound is ", lb, "\n")
cat("upper bound is ", ub, "\n")

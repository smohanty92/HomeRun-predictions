#This is my grid search code for the polynomial kernel.
#The code is the same for the other kernels (except with some parameters removed in the embedded for loops)

#import e1071 library
library("e1071")

#Calculate RMSE function
rmse <- function(error) {
    sqrt(mean(error^2))
}

#load baseball csv into df
baseball.df <- read.csv("baseball.csv")

#Initialize rmses vector with variable that is an obvious outlier for future populated rmses
rmses <- 500

#loop through every cost (I'm using powers of 10 from 0.01 - 1000. Anything lower is moot and the svm library prints a warning for anything higher)
for (cost in c(0.01, 1, 10, 100, 1000)) {
	
		#Epsilon ranges will be from 0.1-0.9 (step of 0.1) as i've seen this in examples. Anything 1 or higher results in error
		for (epsilon in (1:9)/10) {
		
			#Gamma ranges will be from 0.1-1 (step of 0.1) as this is what I've seen in examples
			for (gamma in (1:10)/10) {
				
				#Coefficient is from 1:5 because 5 is the amount of features we have
				for (coefficient in 1:5) {
					
					#Degree is from 1:6 because there are 6 total data columns and thus 6 dimensions can exist
					for (degree in 1:6) {
						
						#create svm model
						model <- svm(baseball.df$HR ~ .,
									data=baseball.df,
									type="eps-regression",
									kernel="radial",
           							scale=FALSE,
									cost=cost,
									gamma=gamma,
									coef0= coefficient,
									degree=degree,
            						epsilon=epsilon)

						#Make a prediction for each attribute
						prediction <- predict(model, baseball.df)

						error <- baseball.df$HR - prediction
						predictedRMSE <- rmse(error)
	
						rmses <- c(rmses, predictedRMSE)
	
						cat("cost is ", cost, " and epsilon is ", epsilon, " and gamma is ", gamma, " and coefficient is ", coefficient, " and degree is ", degree, " and PredictedRMSE is ", predictedRMSE, " \n")

					}
					
					
				}
				
			}
				
		}

}

rmses <- sort(rmses)
rmses <- rmses[1:2]
cat("predictedRMSEs are ", rmses, " \n")

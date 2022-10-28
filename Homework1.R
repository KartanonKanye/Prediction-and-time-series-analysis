#import the tobacco data
smoking <- read.table("text_data/tobacco.txt", header = TRUE, sep = "\t",
                      row.names=1)

#creating a linear regression model with the
#dependent/response variable being ILL and the independent/explanatory variable 
#being CONSUMPTION
linear_model <- lm(ILL~CONSUMPTION, data = smoking)

#Estimate the regression coefficients with lsfit
least_squares_smoking <- lsfit(select(smoking, CONSUMPTION), select(smoking, ILL), intercept = TRUE)
least_squares_smoking$coefficients

#coefficient of determination
summary(linear_model)$r.squared

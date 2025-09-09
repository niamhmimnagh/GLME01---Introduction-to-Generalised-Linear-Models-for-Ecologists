url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/animal_movement.txt'
dat<-read.table(url, header=TRUE)
head(dat)

# magnitude: look plausible, habitat areas all positive, none are zero or negative. 
# no missing values visible in the first few rows. 

colSums(is.na(dat)) # this tells us how many missing values (coded as NA) are in our dataset

summary(dat)

# habitat area ranged from 67 to 476 - if we want to predict sites outside this range, we can't rely on those predictions
# when you're collecting data, collect the data you're interested in.
# movement: mean > median - slight right skew. Our data is approximately normal 
# gamma model
# want to check residual plots when the model is fitted to ensure model is fitting properly.


# glm()

m1<-lm(Movement~HabitatArea+CanopyCover, data=dat)

summary(m1)

# intercept - estimate of the movement when predictors are 0, but in our data
# we dont have predictors of zero. intercept is an extrapolation. 

# beta1 (habitat area estimate) - if habitat area increased by 1ha, you would expect
# a mean change of 0.017 in animal movement
# t-value is the test statistic - used to estimate the p-value
# p-value tells us whether the predictor is significant in terms of animal movement
# Ho: predictor does not have a significant effect on the response
# Ha: predictor does have a significant effect on the response
# p-value tells you whether or not to reject Ho
# p-value that is very small tells us if Ho is true, theres a very small probability of seeing these results
# p-value (>0.05) tells us we can reject Ho
# we reject Ho, and conclude habitat area has a significant effect on animal movement

# estimate for canopy cover (beta2) = -0.009
# p-value of 0.7
# p-value >0.05 - we fail to reject Ho - we dont have evidence that canopy ccover impacts
# animal movement significantly.

# r2/r squared tells us how much variation in the response is explained by the predictors
# adjusted r squared - penalisation of the r squared term

# F statistic - hypothesis test
# Ho: intercept-only model is a better fit
# Ha: the fitted model (with 2 predictors) provides a better fit than the intercept-only

# if the intercept-only model provided a better fit, the p-value gives the probability
# of seeing an F-statistic as large or larger than the one we see
# commonly used cutoff is p=0.05
# we reject Ho, and conclude that the fitted model gives a better fit than the intercept-only model

# confidence intervals 
confint(m1)

new_site<-data.frame(HabitatArea=300, CanopyCover=70)
pred<-predict(m1, newdata=new_site, interval='prediction')
pred

# Fitting the same model using glm
m2<-glm(Movement~HabitatArea+CanopyCover, data=dat, family=gaussian(link='identity'))
coef(m1)
coef(m2)

summary(m2)
plot(m2)

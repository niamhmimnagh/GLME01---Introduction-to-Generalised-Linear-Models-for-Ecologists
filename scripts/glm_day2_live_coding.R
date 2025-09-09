url<-c('https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/animal_disease.txt')
dat<-read.table(url, header=TRUE)
head(dat)

# how do we create age_c? we subtracted 2 from all observations
# mean centering - subtracting mean value
mean(dat$Age)

# if you use Age column as your predictor, then the intercept (beta0) represents
# the log odds of disease presence for an animal thats zero years old
# by centering at 2 - the intercept now represents the log odds of disease
# for a 2 year old animal

# centering doesnt change the slope
# beta1 is stil the change in log odds for each additional year

str(dat)
# this is to look at the structure of your data, to check if everything looks right

summary(dat)
# for numeric variables it gives numeric summaries: mean, median, quartiles
# for factors/characters it gives frequencies

colSums(is.na(dat))

# how many positive versus negative (disease status) animals do we have?
table(dat$Disease) # 385 negative and 115 positive
# if you had a lot of zeros and very few 1s then you've got imbalanced data

# Fitting the logistic regression using glm function

m_age<-glm(Disease~Age_c, data=dat, family=binomial(link='logit')) # probit/cloglog

summary(m_age)


# Intercept: -1.72 : log-odds of disease presence when the predictor=0
# we're using the centered age variable, so when Age_c = 0, Age=2
# intercept: log odds of disease presence when Age=2

log_odds<--1.72 # the log odds of disease presence for a 2 year old animal are -1.72
odds<-exp(log_odds) # the odds of disease presence for a 2 year old animal are 0.179
# the odds of disease presence in a 2 year old animal are smaller than the odds of not having the disease
probability<-odds/(1+odds) # the probability of a 2 year old animal having the disease is roughly 15%

# SLOPE: the effect of age on disease presence
log_odds<-0.235 #  a one-unit increase in x (a 1-year increase in age) increases the log-odds of 
# disease presence by 0.235
odds_ratio<-exp(log_odds) # each 1 year increase in age will multiply the odds of disease by 1.26
# or a 26% increase in the odds
# Ho: coefficient=0 (age_c has no significant effect on disease presence)
# Ha: coefficient !=0 (age_c does have a significant effect on disease presence)
# very small p-values are evidence against the null hypothesis - 
# we reject the null hypothesis - age is a significant predictor of disease presence

# deviance measures how well the model fits the data
# lower deviance is better (in terms of fit)

# null model: the model that just has the intercept
# null deviance: deviance for the null model

# residual deviance: deviance for the model that includes age
# the residual deviance is lower than the null, suggesting a better fit
# this tells us age is useful for explaining disease status.

# AIC: measure of model 
# AIC is used for comparison
# Lower AIC values incidcate better fit

# Fit a model that includes exposure as a predictor
m_age_exp<-glm(Disease~Age_c + Exposure_c, data=dat, family=binomial(link='logit'))
summary(m_age_exp)
# Ho: coefficient for exposure = 0 (exposure has no significant effect on disease)
# Ha: exposure does have an effect on disease
# p-value is very small - so we can reject the null hypothesis and conclude that exposure is a significant 
# predictor of disease

# Likelihood ratio test:
anova(m_age, m_age_exp, test='LRT')

# Ho: the simpler model is sufficient, adding exposure does not significantly improve model fit
# Ha: the more complex model fits the data significantly better

# residual df: residual degrees of freedom: number of observations - number of parameters estimated
# residual deviance: lower residual deviance indicates a better fit - the model with exposure included
# as a predictor has a lower residual deviance, suggesting a better fit

# p-value: Pr(>Chi)
# p-value provides evidence against the null hypothesis
# because our p-value is very small (cutoff of 0.05) we reject the null hypothesis, 
# and we conclude that adding exposure provides a signficant improvement in model fit.



url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/zero_inflated_fish.txt'
dat<-read.table(url)
head(dat)

library(pscl)
library(MASS)

sum(dat$Count==0)/316

# Poisson GLM

m_pois<-glm(Count~Depth+offset(log(Effort)),
            data=dat, family=poisson(link='log'))

summary(m_pois)

# rather than counts, our offset means we're modelling the rate per hour

# Coefficients Interpretation
# Intercept: -0.010518: the log of the expected rate per hour when depth=0
# min(dat$Depth): 3.7
# because our dataset minimum value is not 0, this intercept is an extrapolation
# exp(-0.010518):  0.9895371 - the expected rate per hour when depth is 0 is 0.99 per hour
# 

# Depth: -0.035780: for every every 1m increase in depth we subtract 0.035780 from
# the expected log(rate)
# (-0.035780): 0.9648525: for every 1m increase in depth, the expected catch rate is 
# multiplied by 0.9648525
# approx. 3.5% decrease in expected catch rate per hour for each 1m of additional depth
# very small p-value: lake depth is a significant predictor of fish caught per hour

# deviance gives a measure for how well the model fits the data
# dispersion: 
1025.5/314 # 3.26
# a dispersion parameter approx. 1 suggest no overdispersion
# the dispersion in our data is approx. 3.26 times the expected dispersion for a poisson distribution

# ZIP-Model
# uses pscl package
m_zip<-zeroinfl(Count~Depth+offset(log(Effort))|Depth,
                data=dat, dist='poisson')
summary(m_zip)

# Count Model Coefficients:
# intercept: at predictor = 0 (depth=0) the expected log rate (per hour) of fish caught
# is 0.506543
# exp(0.506543): 1.659544 - the expected rate per hour is 1.659544
# this is an extrapolation 

# depth: -0.037275: for each 1m increase in depth the log rate changes by -0.037275
# (-0.037275):  0.9634112: for each 1m increase in depth the rate is multiplied by 0.9634112
# p-value: depth is a significant predictor of rate of fish caught per hour
# Ho: coefficient=0 (depth is not a significant predictor of catch rate)

# Zero-Process: probability of being a structural zero
# intercept: -0.3525892 
# interpret in terms of log-odds and odds
# -0.3525892 is the log-odds of being in the structural group when depth=0
# exp(-0.3525892):  0.7028659 is the odds of being in the structural group when depth=0

# depth: 0.0008706
# for each 1m increase in depth, the log-odds of being in the structural zero group, change
# by 0.0008706
# exp(0.0008706): 1.000871 - for each 1m increase in depth, odds of being in the structural
# zero group are multiplied by 1] 1.000871
# very large p-value: fail to reject the null hypothesis
# null hypothesis: coefficient = 0 (depth doesnt have a significant effect on probability
# of being a structural zero)
# we don't have the evidence to conclude depth has a significant effect on probability
# of being a structural zero

m_zinb<-zeroinfl(Count~Depth+offset(log(Effort))|Depth,
                 data=dat, dist='negbin')
summary(m_zinb)

# Count model:
# intercept: 0.35374 - log of the expected catch rate when depth=0
 # (0.35374) - expected catch rate when depth=-0

# depth: -0.05307 -  for every 1m increase in depth the log of the expected catch rate
# changes by -0.05307
# exp(-0.05307) - for every 1m increase in depth this gives us the change in the expected
# catch rate
# small p-value: depth is a significant predictor of catch rate

# log(theta): var(Y)=lambda + lambda2/theta
# the dispersion parameter = theta
exp(-0.35081) # theta =  0.7041
# smaller values of theta mean higher levels of overdispersion in the counts
# theta of 0.7041 suggests overdispersion in the counts beyond what Poisson would allow for
# suggests slight evidence for using the negative binomial over the poisson

# Zero process: probability of being a structural zero

# DHARMa residuals
# in order to examine the DHARMa residuals, we need to refit using glmmTMB

library(glmmTMB)
library(DHARMa)

# poisson
m_pois<-glmmTMB(Count~Depth+offset(log(Effort)),
                data=dat, family=poisson)
summary(m_pois)

# zip
m_zip<-glmmTMB(Count~Depth+offset(log(Effort)),
               ziformula = ~Depth,
               data=dat, family=poisson)
summary(m_zip)

# zinb:
m_zinb<-glmmTMB(Count~Depth+offset(log(Effort)),
                ziformula = ~Depth,
                data=dat, family=nbinom2)

# How do we use DHARMa residuals?
# 1. simulate them
# 2. plot

# Poisson
res_pois<-simulateResiduals(m_pois, n=1000)
plot(res_pois)
# the black points strongly move away from the 45 degree line
# the lower part of the curve lies far below the line: too many very small residuals
# what does that mean? indicates too many observed zero counts relative to what the poisson
# expected
# the KS test reports p=0, and deviation is significant
# dispersion test p=0: indicates overdispersion relative to what the poisson expects

# the existence of too many zero counts pulls the lines at 0.25 and 0.5 downwards
# this indicates too many zeros relative to what the poisson distribution expects

testDispersion(res_pois)
# histogram that shows the distribution of a dispersion statistic computed
# from many simulated datasets under the fitted poisson model
# the red line is the dispersion statistic computed from our data

testZeroInflation(res_pois)
# histogram of the distribution of the number of zeros you would expect if the fitted
# poisson model were true
# red line is the number of zeros in our dataset

# ZIP
res_zip<-simulateResiduals(m_zip, n=1000)
plot(res_zip)
# ZIP model fixes the excess zero problem in the mean structure
# the plots indicate lingering overdispersion in the positive counts

testZeroInflation(res_zip)
# histogram: number of zeros youd expect if the fitted model were true  (zero-inflated poisson)

# ZINB:
res_zinb<-simulateResiduals(m_zinb, n=1000)
plot(res_zinb)

testDispersion(res_zinb) # no more extra dispersion than our model would expect
testZeroInflation(res_zinb)

# Hurdle/ZAP Model:
m_zap<-glmmTMB(Count~Depth+offset(log(Effort))65,
               ziformula = ~Depth,
               data=dat, 
               family=truncated_poisson)
summary(m_zap)


# Coefficients:
# intercept: log expected rate when depth=0 (0.497132)
# exp(0.497132): 1.64: the expected catch rate when depth=0 is 1.64

# depth: for each 1m increase in depth the log of the expected catch rate changes by
# -0.035852
# exp(-0.035852):  0.9647831: for each 1m increase in depth the expected catch rate
# is multiplied by  0.9647831
# small-p value: indicates that depth is a significant predictor of catch rate

# Zero Process: 
# intercept: the log odds of a zero-count when depth = 0 
# exp(-0.29603):  0.7437651 - the odds of a zero when depth=0

# depth: for each 1m increase in depth, the log odds of a zero count increase by 0.01734
# exp(0.01734): 1.017491 for each 1m increase in depth, the odds of a zero count are 
# multiplied by 1.017491
# large p-value: we don't have the evidence to conclude depth is a significant predictor

# DHARMa Residuals
res_zap<-simulateResiduals(m_zap, n=1000)
plot(res_zap)

# We are accounting for excess zeros, but the QQ plot suggests that there is still extra over
# dispersion in our data, coming from the count process
# implementing a zero-altered negative binomial would likely help with this.
# replacing the truncated poisson with a truncated negative binomial 


testZeroInflation(res_zap)
# the number of zero counts in our dataset lines up with model expectations

testDispersion(res_zap)
# the level of overdispersion in our data is beyond what the zap model expects
# we would do well to try the ZANB model instead.

# 

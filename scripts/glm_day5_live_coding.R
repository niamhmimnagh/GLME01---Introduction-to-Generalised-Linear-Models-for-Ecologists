url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/pollinator.txt'
dat<-read.table(url)

head(dat)


# 
# Visits

hist(dat$visits, breaks=20)
# histogram of the polinator visits
# most patches/plots saw few visits, but there are a couple that have far more visits
# the right-skew and the presence of very large values compared to where the bulk
# of the dataset lies suggests overdispersion


# response vs predictors
# visits and flower density scatterplot
plot(dat$flower_density, dat$visits)
# as flower density increases, the number of visits is also increasing
# the spread of the data increases with the flower density which suggest overdispersion

# visits and patch size
plot(dat$patch_size, dat$visits)
# very slight positive relationship, but not as obvious as for flower density
# the spread of the observations indicates that other predictors are likely necessary
# but not being taken into account

# visits and distance to the hedgerow
plot(dat$distance_hedge, dat$visits)
# no strong trend visible: possibly distance to the hedgerow is not a significant predictor
# for the number of pollinator visits

# Fit a Poisson GLM

m_pois<-glm(visits~ flower_density+patch_size+distance_hedge,
            data=dat, family=poisson(link='log'))

summary(m_pois)

# intercept: the expected number of visits when predictors=0 (or their baseline level)
exp(1.1886623) # 3.28 is the expected number of visits when all predictors are zero
summary(dat)

# none of our predictors have 0 values: this is an extrapolation

# flower density: each additional flower per m2 increases the log(visits) by 0.1153232
exp(0.1153232) # 1.12: each additional flower per m2 multiplies the expected number of visits
# by 1.12  - 12% increase in the expected number of visits for each additional flower per m2
# very small p-value suggests that flower density is a significant predictor of number of visits

# patch size: each additional hectare increases the log of the visits by 0.0586077
exp(0.0586077)#  1.06: each additional hectare multiplies the expected number of visits
# by 1.06: approximately a 6% increase in the expected number of visits per hectare
# very small p-value: patch size is a significant predictor of the number of visits

# distance from the hedgerow: each additional metre from the hedgerow decreases the log(visits) by 0.0008405
exp(-0.0008405) # 0.999: about a 0.08% decrease in the expected number of visits for each
# metre moved away from the hedgerow
# p-value (small): distance from hedgerow is a significant predictor of the number of 
# pollinator visits

# Poisson assumes no overdispersion (equidispersion)

# deviance is a measure of model fit
# null deviance: measure of how the intercept-only model fits
# residual deviance: measure of how our model fits

# Dispersion statistic
1118.9 # 4.74 - the dispersion in our data is 4.74 times what the Poisson expects
  
## Quasi-Poisson Model
m_quasi<-glm(visits~flower_density+patch_size+distance_hedge,
             data=dat, family=quasipoisson(link="log"))

summary(m_quasi)

# quasi-poisson and poisson give you the same estimates for your coefficients
# flower density and patch size are still identified as being significant predictors
# distance to the hedgerow is now no longer a significant predictor of visit count


library(MASS)
m_nb<-glm.nb(visits ~ flower_density + patch_size + distance_hedge,
             data = dat, link=log)


summary(m_nb)

# Interpreting coefficients
# intercept: exp(1.1654595) - 3.21 is the expected number of visits when predictors are 0
# 1.1654595 expected log(visits) when predictors are 0

# flower density:
# exp(0.12) approximately 1.13 - for each additional flower per m2 we multiply the
# expected number of visits by 1.13 - approximately 13% increase in visits
# p-value: flower density is a significant predictor of number of pollinator visits

# patch size:
# exp(0.0593820): 1.06: each additional hectare of area multiplies the expected number
# of visits by 1.06: 6% increase in visits per hectare
# patch size p-value indicates it is a significant predictor of visits

# distance from the hedgerow:
exp(-0.0010320) # 0.998 for each additional metre from the hedgerow expected number 
# of visits by 0.2%
# p-value indicates slight significance of distance from the hedgerow on the expected
# number of visits
# standard errors in the NB are slightly smaller than in the quasi-poisson model

# theta controls how the variance scales with the mean
# large values for theta indicate smaller amounts of overdispersion
# moderate levels of overdispersion

# null vs residual deviance compares our model with the intercept-only model:
# drop in deviance from the null to the residual deviance indicates a better fit than
# the intercept only model

# poisson: underestimated SE - distance from the hedgerow looked highly significant
# quasi poisson corrected SE  by scaling with the dispersion - 
# from our data we could not conclude that the distance from the hedgerow was significant

# negative binomial: models overdispersion using the theta: SE lie between poisson and quasi poisson
# distance to the hedgerow regained some of its significance

AIC(m_pois)
AIC(m_nb)

# lower AIC value for the negative binomial model indicates that its providing a better fit
# to our data than the Poisson model did

# BINOMIAL EXAMPLE

url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/overdispersed_germination.txt'
dat<-read.table(url)
head(dat)

summary(dat)

# Visualise our data
hist(dat$prop, breaks=15)
# the standard binomial might expect less variability, more proportions clustering around the mean
# this histogram suggests there is more variability in our data than the binomial distribution would expect

dat$ngerm<-dat$n-dat$germ
head(dat)

# start by looking at the standard binomial model
m_binom<-glm(cbind(germ, ngerm)~canopy+soil_moisture+management,
             data=dat, family=binomial(link='logit'))

summary(m_binom)

# intercept: -0.008043 log-odds scale
exp(-0.008043) # 0.99 is baseline odds of germination when canopy cover  = 0, soil moisture
# is 0 and management is  (baseline/reference)
# 0.99:1 odds of germination - germination is slightly less likely than nongermination
# because is an extrapolation (values outside of observed range) the intercept
# is not directly interpretable

# canopy: -0.024509 - for a 1% increase in canopy, the log odds of seed germination
# decrease by 0.024509 
exp(-0.024509 ) #  0.9757889
# a 1% increase in canopy cover multiplies the odds of seed germination by 0.9757889
# approximately a 2.4% decrease in the odds of germination for every 1% increase in canopy covver
# small p-values: reject Ho canopy cover is a significant predictor of seed germination

# soil moisture:  0.030674  - for a 1% increase in soil moisture, the log-odds of seed
#  germination increase by  0.030674 
exp( 0.030674 ) # 1.031149 
# a 1% increase in soil moisture multiplies the odds of germination by 1.031149
# alternatively 1% increase in soil moisture increases the odds of germination by 3%

# Management: unmown (-0.262288) - 
exp(-0.262288)
#  0.7692894: in an unmown site the odds of germination are  0.7692894*odds of germination
# in a mown site
# alternatively: a 23% lower odds of germination in an unmown site vs a mown site
# small p-value: management is a significant predictor of seed germination

# overdispersion
m_binom$deviance/m_binom$df.residual
811/146 # 5.55 dispersion in the data is approx. 5.55 times what the binomial expects

# QUASI-BINOMIAL
m_quasi<-glm(cbind(germ, ngerm)~canopy+soil_moisture+management,
             data=dat, family=quasibinomial(link='logit'))

summary(m_quasi)
# estimates match what the binomial told us
# the p-value for management is greater than for the binomial model: 
# the overdispersion we are accounting for had led to management being less significant of a predictor

# BETA BINOMIAL
library(glmmTMB)

m_bb<-glmmTMB(cbind(germ, ngerm)~canopy+soil_moisture+management,
              data=dat, 
              family=betabinomial(link='logit'))

summary(m_bb)

# intercept: log odds of seed germination when predictors are 0 (or at their baseline)
#  0.020589
exp( 0.020589) # the odds of germination at the baseline level is 1.02
# 1.02:1 - germination is slightly more likely than non germination
# this an extrapolation

# canopy -  for a 1% increase in canopy the log odds of germination decrease by 0.023789
exp(-0.023789 ) # for every 1% increase in canopy cover, the odds of germination are 
# multiplied by 0.976

# soil moisture: for a 1% increase in soil moisture the log odds of germination
# increase by 0.029474
exp(0.029474) # multiplying the odds by 1.02 - 2% increase

# management: 
exp(-0.273329) # odds of germination in an unmown site are the odds of germination in 
# a mown site multiplied by 0/76
# unmown sites have 24% lower odds of germination than mown sites


AIC(m_binom)
AIC(m_bb)
# decrease in AIC values indicates the beta binomial model provides a better fit


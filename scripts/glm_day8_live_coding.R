mobile<-c(floor(runif(10, 0, 15)))
non_mobile<-15-mobile
total<-15
cbind(mobile, non_mobile, 'total'=15)

fit<-glm(cbind(successes, failures)~x, family=binomial)


####################################


url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/bird_counts.txt'
dat<-read.table(url)
head(dat)
range(dat$Area) # for moddel convergence reasons, we will create a new variable that contains centered(area)

dat$Area_c<-scale(dat$Area,center=TRUE, scale=FALSE)

head(dat)

# Frequentist
m_pois<-glm(Y~Area_c, data=dat, 
            family=poisson(link='log'))


summary(m_pois)

# Area_c: 0.041949
# the estimated log count of bird species increases by 0.041949 for each 1 unit increase in area
# area is a significant predictor of species count

# Intercept: 1.987919
# the expected log count of bird species at average area (when area_c=0, area=average (25))


# Bayesian Framework

# 1. Decide on Priors.

prior_inf<-c(
  prior(normal(log(10), 0.2), class='Intercept'),
  prior(normal(0, 0.2),  class='b', coef='Area_c')
)

# centered at log(10) (2.3) - on the count scale, exp(log(10)) = 10
# before we see any data, we believe the expected count (for a site with average area) is around 10
# standard deviation of 0.2 on the log scale (tight): 
# on the count scale, 95% of the prior mass is between 6.7 and 14.9
#  the prior constrains the intercept to 7-15

# Area: 
# centered at 0 - on average we assume no effect of area
# standard deviation of 0.2 is quite narrow.
# we expect very small changes in species count due to area
# 

# this is a very strong informative prior - 

prior_weak<-c(
  prior(normal(log(10), 1.5), class='Intercept'),
  prior(normal(0, 1.0),  class='b', coef='Area_c')
)

# the intercept is stil centered at 10, but the distribution is allowed to be much wider
# this prior is very flat
# area_c: centered at 0 (possibly no effect)
# this slope will allow counts to decrease or increase dramatically

# Prior Predictive Checks
# 
fit_prior_inf<-brm(
  Y~Area_c, data=dat,
  family=poisson(), prior=prior_inf, sample_prior='only',
  chains = 4,
  iter=8000,
  warm=4000,
  seed=2025,
  refresh=0
)

# nice starting point 2000 iterations per chain, with 1000 warmup
# if you see ESS values/Rhat that look bad then increase chains and warmup

fit_prior_weak<-brm(
  Y~Area_c, data=dat,
  family=poisson(), prior=prior_weak, sample_prior='only',
  chains = 4,
  iter=8000,
  warm=4000,
  seed=2025,
  refresh=0
)

# Plot our prior predictive checks
pp_check(fit_prior_inf, type='bars')

# prior predictive mass is centered roughly where the data lives, very low counts and higher counts are 
# predicted to be rare
# there is the tiny amount of mass beyond the observed maximum

pp_check(fit_prior_weak, type='bars')

# the bars at 0-1 are huge, and we're predicting values in the 500s (way above the observed maximum)
# the prior is too wide (diffuse) - it allows a wide range of implausible values.

# FIT THE MODEL

fit_inf<-brm(
  Y~Area_c, data=dat,
  family=poisson(), 
  prior=prior_inf, 
  chains = 4,
  iter=8000,
  warm=4000,
  seed=2025,
  refresh=0
)

summary(fit_inf)
# poisson regression with a log link - we're working on the log scale
# the expected count is found by taking the exponential

# Intercept: 2.01
# on the log scale, when area_c=0 (or when area=average value), the expected log-count is 2.01
# the expected count at an average area is 7.46 - the baseline level of the outcome

# Area_c: 0.58
# a 1 unit increase in area_c increases the log of the expected count by 0.58
# a 1 unit increase in area_c multiplies the expected count by 1.78 (78% increase)

# Est.Error: how much uncertainty remains about the parameter after combining the prior with the data

# 95% Credible Interval
# A 95% credible interval of (1.94, 2.07) means we are 95% confident the true log-intercept lies within this range
# We are 95% confident that the true intercept lies within the range (6.95, 7.92)

# Rhat: diagnostic for convergence
# a value of 1 indicates the chains have mixed well and are sampling from the same posterior
# values above 1.01 would be cause for concern

# ESS: Effective Sample Size
# Bulk ESS: how well the chains can estimate statistic that describe the bulk of the data (mean)
# Tail ESS: how well the chains can estimate the extreme quantiles (intervals)
# Both Bulk and tail ess values are in the 10s of thousands, far above the recommended 100, so both
# the estimates and the intervals are reliable

# In frequentist setting the estimates are MLEs, but in the Bayesian setting the estimates are the 
# posterior means

posterior_inf<-as_draws_df # the draws are on the log scale

rr_inf<-exp(posterior_inf$b_Area_c)

# if rr_inf=0 # the increase of area does not change the expected counts 
# >1 means expected counts increase

median(rr_inf)
# typically each additional hectare of area multiples expected counts by 1.79

mean(rr_inf>1)
# the probability that expected counts increase with area is 100%
# (this gives us the proportion of posterior draws where rr_inf is greater than 1)

# we want to know what the probability that expected counts increase by at least 70% per hectare
mean(rr_inf>1.7)
# the probability that expected counts increase by at least 70% per hectare is about 93

# POSTERIOR Predictive Checks
pp_check(fit_inf, type='bars')

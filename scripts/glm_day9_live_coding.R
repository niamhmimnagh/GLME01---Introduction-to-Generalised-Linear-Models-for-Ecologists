url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/pine_martens.txt'
dat<-read.table(url)
head(dat)

# we are fitting an occupancy model, taking into account what we know: probability of detection is not perfect
# and needs to be taken into account so we don't model the probability of detectection rather than
# the probability of occupancy

library(unmarked)
# can fit this model using brms - specify the distribution is 'zeroinflatedbinomial'

# site level variables: siteCovs
# observation level variables: obsCovs
# all your factors should be factors, or unmarked will change them for you
umf<-unmarkedFrameOccu(
  y = dat[, 7:18],
  siteCovs=data.frame(habitat=dat$habitat, elev_s=dat$elev_s, log_effort=dat$log_effort)
)
#y~habitat+elevation
f_occ<-occu(
  formula = ~ log_effort + habitat + elev_s # detection stage
  ~ habitat + elev_s, # occupancy
  data=umf
)


summary(f_occ)



# occupancy: log-odds of a site being occupied
# intercept: the baseline log odds of a site being occupied at average elevation, in the forest

# habitatGrassland:-0.7837
# for grassland vs forest, the log-odds of occupancy decreases by 0.784
# the odds are multiplied by exp(-0.784) = 0.456

# elevation: each 1 unit increase in elevation decreases the log odds of occupancy by 0.3228

# all p-values are quite large, so none of the coefficients we've looked at are significant drivers of
# probability of occupancy

# Detection section - log odds of detection rather than occupancy

# AIC is not a standalone metric, but its useful for comparing across models (fitted on the same dataset)

###############



# N-MIXTURE MODELS

url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/n_mixture.txt'
dat<-read.table(url)
head(dat, 10)


# 120 observations: 120 sites, each of them surveyed four times
# habitat - grassland, forest, wetland
# elevation_scaled
# effort per visit  - the number of minutes spent at each site varied between visits
# J is the number of visits: 4
# n_sites=number of sites

# Checks
# how many missing values are in our counts?
# no missing values
sum(is.na(dat[,1:4]))


# Range of our counts
# our data is not very wide - a poisson is probably an ok choice
range(dat[,1:4])

# How many zero counts are in our observed counts?
# about 13% of our counts are zeros
# if we had a lot of zeros, then we'd be looking at zero inflated models instead of the poisson
sum(dat[,1:4]==0)/480

# 120 sites and 4 visits:

# how many times were each habitat surveyed
table(dat$habitat)

# if we had only 5 or 6 visits to wetland, you'd hesitate before using wetland as the reference
# but this is fine

library(unmarked)

umf<-unmarkedFramePCount(
  y=dat[,1:4],
  siteCovs=data.frame(habitat=dat$habitat, elev_s=dat$elev_s),
  obsCovs= list(effort = as.matrix(dat[,7:10]))
)

# convert habitat to factors (if we want to control the reference level)

f_nm<-pcount(
  formula = ~ log(effort) + habitat # detection
      ~ habitat + elev_s, # abundance
  data=umf
)

summary(f_nm)

# we have two coefficient tables, one for abundance and one for detection
# the abundance table is on the log-scale (poisson with the log link)
# detection table is on the logit scale (binomial with the logit link)

# Abundance
# intercept: is the log expected count (true underlying count) in the forest, at mean elevation
# exp(1.402): 4.06 is the expected count in the forest at baseline elevation
# no longer talking about expected observed count

# habitatGrassland: -0.971
# for grassland vs forest, the log expected abundance dereases by 0.971
# for grassland vs forest, the expected abundance is multiplied by 0.37
# the expected abundance in grassland is just over 1/3 of that in the forest

# habitatWetland: -0.706
# for wetland vs forest, the log expected abundance decreases by 0.706
# the expected abundance is multiplied by 0.49
# the expected abundancce in the wetland is approximately half the expected abundance in the forest

# elevation: -0.443 (centered and scaled - sd)
# for a 1 SD increase in elevation, the log expected abundance decreases by 0.443
# the expected abundance is multiplied by 0.642
# using standard deviation scale is nice for comparing the effect of different numeric covariates

# Ho: the coefficient is 0, Ha: coefficient is not 0
# so for each of our predictors the we reject Ho, and conclude that they are significant predictors of abundance

# DETECTION - on the logit scale
# intercept: the log odds of detection in the forest at log-effort=0 are -1.421
# the odds of detection are exp(-1.421)

# log(effort) - 1.173
# for a 1 unit increase in the log of the effort, the log-odds of detection increase by 1.173
# for a 1 unit increase in the log of the effor, the odds are multiplied by exp(1.173)

# habitatGrassland: -0.507
# for grassland vs forest, the log odds of detection decrease by 0.507
# the odds of detection are multiplied by exp(-0.507) = 0.6
# we have lower odds of detection in the grassland vs the forest




#### SPECIES DISTRIBUTION MODELS
library(terra)
env<-rast('https://github.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/blob/main/data/env.tif?raw=1')

env

# if you were importing lots of rasters separately, you want to check if the resolution
# extent and coordinate reference system are matching up


names(env) # what rasters are in here? temperature, elevation and forest

# plot the rasters
plot(env)

# plotting the rasters lets us see any gradients (temperature is higher in the south vs the north,
# and elevation is higher in the north vs the south)


# summary statistics per covariate
summary(env)

# resolution or the extent 
res(env)
ext(env)

# TRAINING DATA


url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/sdm_occupancy.txt'
dat<-read.table(url)
head(dat)
# y is the observed occupancy

# GLM

m_occ<-glm(y~temp+elev+forest, 
           data=dat, family=binomial(link = 'logit'))
summary(m_occ)
# Ho: coefficient=0, Ha: coefficient!=0
# elevation has a large p-value so we fail to reject the null hypothesis
# we dont have the evidence to conclude that elevation has a significant effect on species occupancy

# PROJECT TO THE MAP
# use terra::predict() (use the predict function that comes from terra)
# if you dont, you could run into issues with other versions of the predict() function
# type=response - plot the predicted probability of occupancy
pred_occ<- terra::predict(env, m_occ, type='response', na.rm=TRUE)

# PLOTTING THE PROBABILITY OF OCCUPANCY
plot(pred_occ)
# dark - low probability (maybe unsuitable)
# light - high probability

# given the environmental conditions at each location, whats the probability the species will occur there

# probability of occurrence is highest in the south east and declines as we move north west (higher elevation, lower temperature)
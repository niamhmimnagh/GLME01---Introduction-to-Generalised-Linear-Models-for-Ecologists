url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/seed_germination.txt'
dat<-read.table(url)
head(dat)

str(dat)

summary(dat)
# we have 90  pots in our experiment
# every pot has 20 seeds in it
# minimum number of germinated seeds is 1, and maximum is 16

# fitting the binomial glm using the (successes, failures) set-up

m1<-glm(cbind(Germinated, NotGerminated)~Light, data=dat, family=binomial(link='logit'))

summary(m1)
#  deviance is a measure of fit
# null deviance: deviance of the model without any predictors (only has the intercept)
# residual deviance: deviance of our model.
# lower deviance indicates a better fit:
# residual deviance is significantly lower than null deviance: adding light as a predictor
# provides a better fit

# AIC values mean little in isolation - they are used for model comparison

# coefficients: 
# intercept is the log-odds of germination for the baseline group
# if our predictor was numeric, the intercept would be the log-odds of germination at x=0
# our predictor light is a factor with 3 levels (full, partial and shade)
# by default r picks the level that comes first alphabetically (in our case: full)
# the intercept is the log-odds of germination under full light
# coefficients (partial and shade) are differences in log-odds of germination
# relative to full light

# intercept: 0.12 - in full light the log-odds of a single seed germinating is 0.12
# LightPartial (partial light): -1.61 - the log odds of a single seed germinating in
# partial light is 1.61 lower than full light
# LightShade: -0.72 - log odds in shade are 0.72 lower than in full

# intercept
log_odds<-0.1201
odds<-exp(log_odds) #1.13 - the odds of a single seed germinating in full light is approximately 1.13
# 1.13:1 odds - odds of a seed germinating are greater than the odds of a seed not germinating
probability<-odds/(1+odds) # the probability of a seed germinating under full light is 53%

# partial light
log_odds<--1.6141
odds_ratio<-exp(log_odds) # the odds of germination in partial light are 20% the odds of
# germination in full light

# logit(pi)=log odds(pi) = beta0+beta1*lightPartial + beta2*lightShade
# log odds = 0.1201 - 1.6141*(1) - 0.7246*(0) = -1.494 
log_odds<--1.494
odds<-exp(log_odds)
probability<-odds/(1+odds) # the probability of germination in partial light is around 18%


# Shade
# logit(pi)=log odds(pi) = beta0+beta1*lightPartial + beta2*lightShade
# log odds = 0.1201 - 1.6141*(0) - 0.7246*(1) = -0.6045
log_odds<--0.6045
odds<-exp(log_odds)
probability<-odds/(1+odds) # the probability of germination in shade is around 35%

##########################
# MULTINOMIAL
url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/habitat_selection.txt'
dat<-read.table(url)
head(dat)

# check structure first - habitat might not be a factor
str(dat)

table(dat$habitat, dat$age_class)

library(dplyr)
dat$habitat<-as.factor(dat$habitat)
dat<- dat %>% mutate(habitat=relevel(habitat, ref='Mudflat'))

# fit the model
library(nnet)
fit<-multinom(habitat ~ body_mass + age_class + prey_density, data=dat)

# examine model output
summary(fit)

# create a new table
# 1) One tidy table with Relative Risk Ratios (RRR) and 95% CIs
tab_RRR <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::rename(RRR = estimate)

# 2) Wald p-values from summary(fit), then merge into the same table
s  <- summary(fit)
p  <- 2 * (1 - pnorm(abs(s$coefficients / s$standard.errors)))
pv <- as.data.frame(as.table(p));
names(pv) <- c("y.level","term","p.value")

final_tab <- dplyr::left_join(tab_RRR, pv, by = c("y.level","term")) |>
  dplyr::select(y.level, term, RRR,  p.value.x) |>
  dplyr::arrange(y.level, term)


final_tab

# y.level: which of the habitat categories are being compared against mudflat
# Rocky means we're looking at rocky vs. mudflat

# term: intercept or the predictor variable for each of our 3 equations
# intercept: log relative risk of an individual choosing that habitat vs the mudflat
# when the predictors are zero/at the baselines


# RRR: Relative risk ratio: exp(estimate) (the model estimated log relative risks)
# p-value provides evidence against the null hypothesis
# Ho: coefficient=0 (predictor doesnt have a significant effect on habitat selection)
# Ha: coefficient !=0 
# small p values mean we reject the null hypothesis (cutoff = 0.05)
# rows 2-8 have large p values: we fail to reject Ho
# the effect of the predictors (age, bodymass, prey) on choosing a rocky habitat vs the mudflat
# are not significant


# Row 9: intercept for seagrass: 
# at baseline conditions, the relative risk of an individual choosing seagrass vs
# mudflat is about 0.33
# risk of choosing seagrass is less likely than choosing the mudflat
# for an adult with body mass 0 and prey density 0, the risk of choosing seagrass is approx
# 1/3 that of choosing the mudflat

# Row 12:
# the effect of prey-density on choosing seagrass instead of mudflats
# each 1 unit increase in prey density multiplies the relative risk of choosing the seagrass
# by 1.05
# increases the relative risk of seagrass vs mudflats by approximately 5%

# Row 10:
# the p-value being large indicates no significant statistial evidence of a difference
# between adults and juveniles in their relative risk of seagrass vs mudflats


# ROW 11
# relative risk ratio (RRR) compares the probability of being in one outcome category 
# (here: Seagrass) to the probability of being in the baseline category (Mudflats), 
# per unit increase in the predictor.
# 
# For every unit increase in body mass, the probability of an individual being in
# Seagrass (rather than Mudflats) increase slightly.
# heavier individuals are slightly more likely to be found in Seagrass habitats
# than in Mudflats

exp(0.0005187456)

# ORDINAL DATA
url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/habitat_suitability.txt'
dat<-read.table(url)
head(dat)


dat$Suitability<-factor(dat$Suitability, levels=c("Poor", "Fair", "Good", "Excellent"),
                        ordered=TRUE)
levels(dat$Suitability)

library(ordinal)
fit<-clm(Suitability~VegCover_sc+DistRoad_logkm+Management, data=dat, link='logit')
summary(fit)
# a positive beta value (estimate) increases the odds of being in a higher category

# effect of scaled vegetation cover: 0.7883
# for a 1 standard deviation in vegetation cover, the odds of being in a
# higher suitability category are multiplied by exp(0.7883) = 
# very small p-value indicates vegetation cover is a significant predictor of suitability

a<-scale(dat$VegCover)
head(a)
head(dat$VegCover_sc)
sigma<-attr(a, 'scaled:scale') # standard deviation used
beta<-coef(fit)['VegCover_sc']/sigma
beta
# each one percent icnrease in vegetation cover increases the log odds of being
# in a higher category by 0.046
exp(beta) # change in the odds


# Distance from the road
# large p value: we dont have enough evidence to conclude that distance from the road
# affects habitat suitability
# a 1 log-km increase in distance from the road changes the odds by a multiplicative factor
# of exp(0.2620)

# Management
# ManagementUnprotected means we're comparing the effect of being in an unprotected site
# to a protected site
# exp(-0.4250) = 0.65
# unprotected sites have a 35% lower cumulative odds of being higher suitability
# compared to protected sites

# threshold coefficients are not ecological effects: generally dont interpret their sizes



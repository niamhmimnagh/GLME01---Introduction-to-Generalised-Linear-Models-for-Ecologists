url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/bird_counts.txt'
dat<-read.table(url)

head(dat)
summary(dat)

# Fitting the model
m_pois<-glm(Y~Area, data=dat, family=poisson(link='log'))

# summary
summary(m_pois)

# log(lambda)=beta0 + beta*area

# intercept:
# on the log scale, when area=0 hectares, the log of the expected number of bird species
# is 0.917
exp(0.917) # the expected number of bird species when area is 0 is 2.5
# this is an extrapolation: we are estimating the number of bird species for an area
# the model hasnt seen
# you could center the area (subtract the mean from all observations)
# then the exp(intercept) would be the expected number of bird species at mean(area)

# the effect of area (slope/beta1)
# each additional hectare in area increases the log of the expected number of bird
# species by 0.041949   
exp(0.041949)
# on the response scale, the expected number of bird species is multiplied by 1.04
# for each additional hectare of area
# roughly a 4.3% increase in expected bird species count per hectare

# Ho: coefficient=0 (area doesnt have a significant effect on bird count)
# Ha: coefficient!=0
# p-value provides evidence against the null hypothesis Ho
# very small p-value: reject Ho and conclude that area does have a significant effect on bird count

# deviance is a measure of model fit: lower deviance indicates better fit
# null deviance: deviance from the intercept-only model
# residual deviance: deviance for the model we fitted (area as a predictor)
# model with area as a predictor provides a better fit

url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/animal_density.txt'
dat<-read.table(url)
head(dat)

summary(dat)

library(ggplot2)
ggplot(dat, aes(count))+geom_histogram(bins=30)


# exposure
dat$exposure<-dat$area_ha*dat$effort_hr
head(dat)

# distribution of the exposure
ggplot(dat, aes(exposure))+geom_histogram(bins=30)
# most surveys were conducted at relatively low exposures
# a small number of surveys are much more exposed
# differing exposure levels means we should account for it


ggplot(dat, aes(exposure, count))+geom_point()
# the plot shows a relationship between exposure and count
# longer surveys or greater area surveyed does have a relationship with the count

# Fitting the Poisson model with exposure terms

m_pois2<-glm(count~habitat_quality+offset(log(area_ha))+offset(log(effort_hr)),
             data=dat, family=poisson(link='log'))

summary(m_pois2)

# interccept: when habitat quality=0, the expected density (per hectare per hour)
# is exp(0.69039) or approximately 2.
# when habitat quality is 0, the expected density is about 2 animals per hectare 
# per hour

# slope for habitat quality (beta1)
# for every one unit increase in habitat quality, the expected density (animals per hectare
# per hour) will multiply by exp(0.57616) or 1.78
# for each +1 unit in habitat quality, the expected density multiplies by about 1.78
# approximately 78% increase in density

# Ho: coefficient is 0, habitat quality doesnt have a significant effect on density
# Ha: coefficeint is not 0
# small p-value means we reject Ho: habitat quality has a significant effect on density

m_pois3<-glm(count~habitat_quality,
             data=dat, family=poisson(link='log'))
summary(m_pois3)


# overdispersion parameter = residual deviance/residual degrees of freedom
288.03/298
# overdispersion parameter approx. 1 - means overdispersion is likely not an issue

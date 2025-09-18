url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/long_data.txt'
long_dat<-read.table(url)
head(long_dat)

# swapping between long and wide format
library(tidyr)
library(dplyr)

wide_dat<- long_dat |> pivot_wider(names_from=species,
                          values_from=count,
                        names_prefix='count_')

# use the values in species to create column names
# fill those columns with the corresponding count values
# adds 'count_' to each new species column name
# normally NA indicates that there is no  observation related to those levels 
# in the original dataset
head(wide_dat)

wide_dat<-long_dat %>%
  pivot_wider(
    id_cols = c(site, year, habitat), # use these columns to define each row
    names_from = species, # create one unique column for each species
    values_from = count, # fill the new columns with values from 'count'
    names_prefix = 'count_', # for each new column name is 'count_'
    values_fn=sum, # if multiple rows map to the same species/key sym their counts
    values_fill=0 # if there is some combination of key missing, fill it with 0
    )
head(wide_dat)

# WIDE -> LONG
long_again<-wide_dat %>% 
  pivot_longer(
    cols=starts_with('count_'), # select all columns that start with 'count_'
    names_to = 'species', # create a new column called species
    names_prefix = 'count_', # strip the 'count_' prefix
    values_to = 'count' # put all the numbers from the wide columns into a new 'count' column
  )

head(long_again)

#####
# MISSING DATA
url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/mar_data.txt'
mar_dat<-read.table(url)
head(mar_dat)

# this is MAR data
# for MAR data complete cases analysis is not appropriate

fit_cc<-lm(blood_pressure ~ age+bmi+sex, data=mar_dat)
summary(fit_cc)
# normal linear model (glm with a normal response)
# 111 observations deleted due to missingness - perform complete cases analysis
# if data is missing completely at random, then complete case analysis is fine
# however, for mar data, complete case analysis can introduce bias

# interpreting results
# interccept: expected blood pressure for the reference category (female) when age and bmi are 0
# age and bmi of zero are not realistic values, 
# this intercept is a statistical anchor

# age: 0.46262
# for each 1 unit increase in age, blood pressure increases by about 0.46 units
# p-value is very small so age is a significant predictor of blood pressure

# bmi: 0.79794
# for each 1 unit increase in bmi, blood pressure increases by 0.79
# p-value: indicates bmi is a significant predictor of blood pressure

# sexMale: (sex is coded as a factor (male or female),  and R chose female as the reference)
# (female is first alphabetically)
# estimate: -5.17660
# males have an average of 5.2 units lower blood pressure than females, holding age and bmi
# constant

# by default, lm() uses na.action=na.omit,
# the model is fitting on 889 out of 1000 observations
# the remaining cases the model is fitted to is not a random sample anymore,
# the coefficients we see reflect only the subset of individuals that had no missing data

nobs(fit_cc)
nrow(mar_dat)


# MULTIPLE IMPUTATION (mice - Multiple Imputation by Chained Equations)
library(mice)

# create imputed datasets
imp<-mice(mar_dat, m=20, printFlag=F, seed=99)
# setting the seed ensures reproducibility
imp$loggedEvents
# why this might happen: 
# sex might actually only be female or male
# in one of the subsets, the sex variable just happened to be only male or only female

# Fit the analysis on each of our 20  datasets
fit_mi<-with(imp, lm(blood_pressure~age+bmi+sex))

# Pool the results
pool_mi<-pool(fit_mi)
summary(pool_mi)

# this gives you a coefficient table:
# this is a normal glm (normal linear model)
# the estimates are interpreted on the original scale

# intercept: the expected blood pressure for females at age=0 and bmi=0
# age: for each 1 unit increase in age, the expected blood pressure increases by 0.46

# this multiple imputation is used for analysing MAR data
# complete cases can reduce sample size and statistical power  by dropping observations
# multiple imputation generally gives less biased estimates than complete cases analysis
# more precise estimates (smaller standard errors)


# INTERACTION TERMS:

url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/interactions_data.txt'
int_dat<-read.table(url)
head(int_dat)

# movement ~ habitat
fit_1<-glm(movement~habitat, data=int_dat)
summary(fit_1)

# intercept: average movement for the reference category (forest)
# when the habitat is forest, the average predicted movement is 7.52

# habitatGrassland: the difference is mean movement between grassland and forest
# grassland has an average of 2.18m lower movement than forest
# the p-value (small) indicates that the difference between grassland and forest in terms
# of animal movement is significant

# habitatWetland: the difference in the mean movement between wetland and forest
# wetland has an average of 3.43m higher movement than forest
# again the p-value tells us the difference is significant

# forest: average movement 7.52
# grassland = 7.52 - 2.19 = 5.33 (average movement in the grassland)
# wetland = 7.52 + 3.43 = 10.95 (average movement in the wetland)

fit_2<-glm(movement~area*habitat, data=int_dat)
# area + habitat + area*habitat
summary(fit_2)

# intercept: the average movement for the reference (forest) when area=0

# area: the slope of movement with respect to area (forest)
# for every 1 unit increase in area, movement increases by approx 0.54 in the forest

# habitatGrassland: difference in the intercept between grassland and forest (when area=0)
# the movement in grassland is about 0.72 units lower than in forest (when area=0)

# habitatWetland: difference between wetland and forest (when area=0)
# the movement in wetland is about 1.97 units higher than in forest when area=0

# area:habitatGrassland:-0.267
# this is the difference in the effect of area between grassland and forest
# the effect of area in the grassland: 0.54-0.267 = 0.273
# for every 1 unit increase in area, movement increases by approx. 0.273 in the grassland

# area:habitatWetland: 0.31
# the difference in the effect of area between wetland and forest
# the effect of area in wetland: 0.54 + 0.31 = 0.85
# for every 1 unit increase in area, movement increases by 0.85 in the wetland

AIC(fit_1)
AIC(fit_2)
#  out of the two models we've fitted, model 2 provides a better fit
# AIC doesnt actually tell you if models are good
# its just a comparison

# CENTERING AND SCALING

head(int_dat)
# centering subtracts the mean
# scaling centers the data and then divides by the standard deviation
area_c<-scale(int_dat$area, center=TRUE, scale=FALSE)
area_mean<-attr(area_c, 'scaled:center' )
area_sd<-attr(area_c, 'scaled:scale' )

head(area_c)
head(int_dat$area)

int_dat$area_c<-area_c
head(int_dat)

# RE-FIT THE MODELS USING CENTERED AREA:
fit_3<-glm(movement~area_c*habitat, data=int_dat)
# area + habitat + area_c*habitat
summary(fit_3)

# intercept: the average movement for the reference (forest) when area is at its average value

# habitatGrassland: difference in the intercept between grassland and forest (when at average area (=5))
# the movement in grassland is about 0.72 units lower than in forest (when at average area)



# QUADRATIC MODEL

fit_4<-glm(movement~area+I(area^2)+habitat, data=int_dat)
summary(fit_4)

# coefficients
# intercept: the expected movement when area=0, and when habitat=forest

# area: 0.546
# the linear effect of area on movement in the forest
# for each 1 unit increase in area, movement increases by about 0.55 units in the forest

# area^2: -0.0008
# the quadratic effect of area in the forest
# if this coefficient were negative: it indicates a concave-down relationship
# inverted u-shape
# movement increases with area initially, but then the rate slows and could eventually
# turn negative as area becomes larger

# if this coefficient were positive, it would indicate a concave-up relationship
# u-shape
# as area increases, movement decreases at first, but then levels of  begins to increase
# again as area becomes larger

# because area squared is not a significant predictor it can be safely removed
# if it were significant, but area (linear effect) was not, area would have to remain
# in the model


############################################################
# Example: Animal Movement — Normal Linear Model in R
# Goal: Show lm() and glm() (Gaussian/identity) are equivalent,
#       then run  diagnostics.
############################################################

set.seed(71)  

#### 1) IMPORT DATA ####
url<-'https://raw.githubusercontent.com/niamhmimnagh/GLME01---Introduction-to-Generalised-Linear-Models-for-Ecologists/refs/heads/main/data/animal_movement.txt'
d<-read.table(url, header=TRUE)
head(d)

#### FIT WITH lm() (the classical normal linear model) ####
m_lm <- lm(Movement ~ HabitatArea + CanopyCover, data = d)
summary(m_lm)

# Interpreting Coefficients:
# Intercept: at habitat area=0 and canopy cover=0, the average movement is 4.43
# HOWEVER, area and canopy cover are not 0 anywhere in our dataset, so this is an extrapolation
# and basically the model is estimating for a scenario it has never seen, so is more or less
# making it up

# Habitat Area: 
# for every 1 unit increase in habitat area, movement increases by 0.017.
# Null hypothesis (Ho): coefficient=0 (habitat area is not a significant predictor of movement)
# Alternative hypothesis (Ha): coefficient !=0
# the p-value tells us the probability of obtaining a t-value at least as large as the one we did, 
# if the null hypothesis were true.
# so small p-values provide evidence against the null hypothesis
# a general cutoff point is 0.05 for the p-value
# because the p-value here is much lower than 0.05 (4.18e-11), we reject the null hypothesis
# and conclude that habitat area is a significant predictor of animal movement

# Canopy cover:
# for every 1 unit increase in canopy cover, movement decreases by 0.009
# The null and alternative hypothesis are the same as for habitat area, i.e. we are testing whether
# we believe the coefficient to be 0 or not
# here the p-value is very large, which means if the null hypothesis were true, we would have a 70%
# probability of obtaining a t-value at least as large as the one we observe for canopy cover
# because p-value > 0.05, we fail to reject the null hypothesis - we do not have evidence to conclude
# that canopy cover is a significant predictor of animal movement
# important to note the way we phrase this - it is possible that if more data were collected, the 
# significance level of canopy cover would change, so we cannot say that canopy cover is definitely
# not a significant predictor of movement, just that we do not currently have evidence to conclude 
# it significant



#### FIT WITH glm() USING GAUSSIAN IDENTITY (equivalent to lm) ####
# For a normal linear model, glm(family = gaussian(link="identity")) gives the same MLEs.
m_glm <- glm(Movement ~ HabitatArea + CanopyCover, data = d,
             family = gaussian(link = "identity"))
summary(m_glm)

####  
# Why are they identical? For Gaussian errors with identity link, both lm() and glm()
# maximize the same likelihood and solve the same normal equations.


#### DIAGNOSTICS FOR THE GLM FIT ####
# We will inspect:
#   - Raw residuals
#   - Residuals vs Fitted (check linearity & equal variance)
#   - Normal Q–Q (check normality of residuals)
#   - Residuals vs Leverage (influence & Cook's distance contours)
#   - Cook's distance explicitly
#   - Influence measures summary
#   - Collinearity diagnostics and VIF

# A) Get residuals and basic summaries
res <- resid(m_glm)           # raw residuals
stud_res <- rstudent(m_lm)    # studentised residuals (from lm; fine here because they’re identical)

print(summary(res))

# B) Diagnostic plots
op <- par(no.readonly = TRUE)  # save old graphical settings
par(mfrow = c(2, 3), mar = c(4.2, 4.2, 2, 1))

# 1) Histogram of residuals (shape should look roughly symmetric/normal)
hist(res, breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")
rug(res)

# 2) Residuals vs Fitted (look for random scatter, no pattern/funnel)
plot(fitted(m_glm), res,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, lty = 2)

# 3) Normal Q–Q plot (points near the line suggest normality)
qqnorm(res, main = "Normal Q-Q Plot of Residuals")
qqline(res, lwd = 2)

# 4) Scale-Location (Spread-Location) — constant variance check
plot(fitted(m_glm), sqrt(abs(res)),
     xlab = "Fitted values", ylab = "Sqrt(|Residuals|)",
     main = "Scale-Location")
lo <- lowess(fitted(m_glm), sqrt(abs(res)))
lines(lo, col = "gray40", lwd = 2)

# 5) Residuals vs Leverage (with Cook's distance contours)
plot(m_glm, which = 5)  # base R built-in plot for leverage vs standardized residuals + Cook's lines

par(op)  # restore graphics

# C) Influence measures (hat/leverage, studentised residuals, Cook's D, DFBETAs)
cat("\n=== Influence measures (top potential influencers) ===\n")
inf <- influence.measures(m_glm)
print(head(which(apply(inf$is.inf, 1, any), TRUE)))  # row indices flagged by any measure


# D) Collinearity and VIF
  v <- car::vif(m_glm)
# VIF approx. 1 means no collinearity, 2-5 is moderate, >10 is a serious concern

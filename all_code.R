knitr::opts_chunk$set(echo = TRUE)

sample(1:100, 10, replace=FALSE)
#1:10000 = numbers to chose among 
#number of random numbers you wish to generate
#to replace or not (in other words do you wish for the same number to be selected multiple times)

#create vector of heights (cm) of one population of A. subverticulata
sedonapopulation <- c(3, 3, 3, 3, 7, 8, 9)
#take the mean
mean(sedonapopulation)
#calculate variance
var(sedonapopulation)
#calculate standard deviation
sd(sedonapopulation)
#calculate standard error
#base r doesn't have this function
#so we have to write our own
std_error <- function(x) sd(x)/sqrt(length(x))
std_error(sedonapopulation)

sum = 3+3+3+3+7+8+9 #add all the numbers in the sample
n = length(sedonapopulation) #or you can just calculate the number of height measurements
mean = sum/n; mean #divide sum by number

#We determine how much each observation varies from the mean.
diffobs1 = mean - 3
diffobs2 = mean - 3
diffobs3 = mean - 3
diffobs4 = mean - 3
diffobs5 = mean - 7 
diffobs6 = mean - 8
diffobs7 = mean - 9 

#Then we square each of these. 
diffobj1_sq = diffobs1^2
diffobj2_sq = diffobs2^2
diffobj3_sq = diffobs3^2
diffobj4_sq = diffobs4^2
diffobj5_sq = diffobs5^2
diffobj6_sq = diffobs6^2
diffobj7_sq = diffobs7^2

#Then we add the differences up.
sumofsquares = sum(diffobj1_sq, diffobj2_sq, diffobj3_sq, diffobj4_sq, diffobj5_sq, diffobj6_sq, diffobj7_sq)
#Divide the sum of squares by n - 1.
variance = sumofsquares/(n-1); variance 

#an example of an unskewed population
sedona_unskewed <- c(1, 2, 3, 4, 5, 6, 7)
mean(sedona_unskewed)
median(sedona_unskewed)

#previous sedona population; skewed
sedonapopulation <- c(3, 3, 3, 3, 7, 8, 9)
mean(sedonapopulation)
median(sedonapopulation)

sedona_unskewed <- c(7, 2, 2, 3, 3, 3, 3, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 0.5)
mean(sedona_unskewed)
median(sedona_unskewed)
#I'm renaming sedonapopulation, sedona_skewed for this example
sedona_skewed <- c(3, 3, 3, 3, 7, 3, 4, 5, 6, 3, 3, 3, 4, 4, 6, 7, 8, 9, 3, 4, 5, 2)
mean(sedona_skewed)
median(sedona_skewed)

hist(sedona_unskewed, main = "Mostly Unskewed", xlab = "Plant height (cm)", breaks=5)

h <- hist(sedona_unskewed, main = "Mostly Unskewed", xlab = "Plant height (cm)", breaks=5)

xfit <- seq(min(sedona_unskewed), max(sedona_unskewed), length = 40) 
yfit <- dnorm(xfit, mean = mean(sedona_unskewed), sd = sd(sedona_unskewed)) 
yfit <- yfit * diff(h$mids[1:2]) * length(sedona_unskewed) 

lines(xfit, yfit, col = "black", lwd = 2)

hist(sedona_skewed, main = "Skewed", xlab = "Plant height (cm)", breaks = 5)

boxplot(sedona_skewed, main="Skewed", ylab="Plant height (cm)")

#Let's add a plant height of 20.
sedona_skewed <- c(3, 3, 3, 3, 7, 3, 4, 5, 6, 3, 3, 3, 4, 4, 6, 7, 8, 9, 3, 4, 5, 2, 20)
boxplot(sedona_skewed, main="Skewed", ylab="Plant height (cm)")

library(readr)

url <- "https://drive.google.com/uc?export=download&id=1KDvCzj9-YzN1zDHdLutcpruYx5mpcEWH"
example1 <- read_csv(url)

knitr::kable(example1, caption="Sample Space", full_width = F, html_font = "Arial")

answer = 1/36; answer

#factorial of 5 or 5!
factorial <- 5*4*3*2*1; factorial
factorialR <- factorial(5); factorialR
#remember that 
factorial(0)

permutations = (factorial(4))/factorial(4-3); permutations

combinations = (factorial(5))/(factorial(2)*factorial(5-2)); combinations

#The sample space is all possible rolls
samplespace <- c(1, 2, 3, 4, 5, 6); samplespace
#We will use Boolean operators in R. They will return TRUE / FALSE statements.
#Below we tell R to look for odd numbers OR (indicated by line) 
#numbers less than 4.
unionevent <- (samplespace%%2==1) | (samplespace < 4); unionevent
#If the conditions are satisfied, TRUE will be returned.
#If conditions are not met, FALSE will be returned.

#Now, let's calculate the probability using techniques that you 
#have already seen above.
probunion <- 4/6; probunion

#The sample space is all possible rolls
samplespace <- c(1, 2, 3, 4, 5, 6); samplespace
#We will use Boolean operators in R. They will return TRUE / FALSE statements.
#Below we tell R to look for odd numbers OR (indicated by line) 
#numbers less than 4.
intersectionevent <- (samplespace%%2==1) & (samplespace < 4); intersectionevent
#If the conditions are satisfied, TRUE will be returned.
#If conditions are not met, FALSE will be returned.

#Now, let's calculate the probability using techniques that you 
#have already seen above.
probintersection <- 2/6; probintersection

#Just for fun
#The other common Boolean operator used in computer code is 'not'
#We want to roll any number except 2
nottwo <- samplespace != 2; nottwo
probNOTtwo <- 5/6; probNOTtwo


conditional <- (1/6)/(1/2); conditional 
#the likelihood of guessing the correct number, if the number is odd is 1/3.

heads <- (1/2)*(1/2); heads

fivesix <- (1/6)+(1/6); fivesix

#The sample space is all possible rolls
samplespace <- c(1, 2, 3, 4, 5, 6); samplespace
#We will use Boolean operators in R. They will return TRUE / FALSE statements.
#Below we tell R to look for odd numbers OR (indicated by line) 
#numbers less than 4.
unionevent <- (samplespace%%2==1) | (samplespace < 4); unionevent
#If the conditions are satisfied, TRUE will be returned.
#If conditions are not met, FALSE will be returned.

#Now, let's calculate the probability using techniques that you 
#have already seen above.
probunion <- 4/6; probunion

probA <- 3/6
probB <- 3/6
intersection <- 1/3

finalprob <- probA + probB - intersection; finalprob

paws <- 0:18
plot(paws, dbinom(paws, 18, 0.5), type='h', ylab="Probability", xlab="Number Right-pawed Cats")

probability <- dbinom(paws, 18, 0.5)
N <- 0:18
pawtable <- cbind(N, probability)
pawtableF <- as.data.frame(pawtable); pawtableF

pvalue <- (0.0117 + 0.0031 + 0.0006 + 0.00007 + 0.000004)*2; pvalue

library(pwr)
library(tidyverse)
library(simr)
library(simglm)
library(Superpower)
#load data
data('oatvar', package='faraway')

pwr.anova.test(k=3, f=0.3, sig.level=0.05, power=0.8) 

cohen.ES(test = 'r', size = 'small')

ggplot(oatvar, aes(y=yield, x=block, color=variety)) + 
    geom_point() +
    geom_line(aes(x=as.integer(block)))

library(simr)
head(cbpp)
cbpp$obs <- 1:nrow(cbpp)

library(tidyverse)
data <- tribble(~Population, ~SLA, 
        "Flagstaff", 2, "Flagstaff", 1, "Flagstaff", 2,
        "Sedona", 5, "Sedona", 6, "Sedona", 7,
        "CampV", 10, "CampV", 11, "CampV", 10)

pop <- group_by(data, Population)
SLAtable <- summarise(pop, meanSLA = mean(SLA)); SLAtable

#ANOVA
model1 <- aov(SLA ~ Population, data=data)
summary(model1)

#ANOVA run as a linear model
model2 <- lm(SLA ~ Population, data=data)
summary(model2)

#in this simple model, 
#notice that the Intercept is the mean SLA of the 
#reference group
#then population Flagstaff Coefficient
#is 10.333 - 8.667 = 1.67 (mean of the Flagstaff pop)
#Sedona population =  10.3333 - 4.3333
#6 = mean SLA of the Sedona population

data(iris)
#Is petal width related to petal length?

#model <- glm(y-value ~ x-values, family = specifylinkingfunctions, data = dataset)
iris_model <- glm(Petal.Length ~ Petal.Width, family = gaussian(), data = iris) #y data is continuous, use gaussian
summary(iris_model)

#for comparison run a linear regression for comparison
iris_modelR <- lm(Petal.Length ~ Petal.Width, data = iris) #y data is continuous, use gaussian
summary(iris_modelR)



library(car)
#glm
#iris_model <- glm(Petal.Length ~ Petal.Width, family = gaussian(), data = iris) #y data is continuous, use gaussian
Anova(iris_model)

#for comparison run a linear regression for comparison
#iris_modelR <- lm(Petal.Length ~ Petal.Width, data = iris) #y data is continuous, use gaussian
Anova(iris_modelR)


#Look at residuals
res_irismodel <- rstandard(iris_model)
plot(iris_model$fitted.values, res_irismodel, pch=20, ylab = "Standarized residuals", xlab = "fitted values")


iris_model <- glm(Petal.Length ~ Petal.Width, family = Gamma (), data = iris) #y data is continuous, use gaussian
summary(iris_model)
res_irismodel <- rstandard(iris_model)
plot(iris_model$fitted.values, res_irismodel, pch=20, ylab = "Standarized residuals", xlab = "fitted values")

iris_model <- glm(Petal.Length ~ Petal.Width, family = gaussian (link="log"), data = iris) #y data is continuous, use gaussian
summary(iris_model)
res_irismodel <- rstandard(iris_model)
plot(iris_model$fitted.values, res_irismodel, pch=20, ylab = "Standarized residuals", xlab = "fitted values")


iris_model <- glm(Petal.Length ~ Petal.Width, family = gaussian (link=power(0.25)), data = iris) #y data is continuous, use gaussian
summary(iris_model)
res_irismodel <- rstandard(iris_model)
plot(iris_model$fitted.values, res_irismodel, pch=20, ylab = "Standarized residuals", xlab = "fitted values")


iris_model <- glm(Petal.Length ~ Petal.Width, family = gaussian(), data = iris)


#load libraries and datasets
suppressWarnings(library('faraway'))
suppressWarnings(library('lmerTest'))
suppressWarnings(library('lme4'))
suppressWarnings(library('tidyverse'))
suppressWarnings(library('emmeans'))
suppressWarnings(library('multcomp'))
data('oatvar', package='faraway')

ggplot(oatvar, aes(y=yield, x=block, color=variety)) + 
    geom_point() +
    geom_line(aes(x=as.integer(block)))

oatmodel.1 <- lmer( yield ~ variety + (1|block), data=oatvar)
anova(oatmodel.1)


oatmodel.1 <- lmerTest::lmer( yield ~ variety + (1|block), data=oatvar)
dispairwise1 <- emmeans::emmeans(oatmodel.1, ~variety, type = "response"); dispairwise1
dis_glht <- glht(oatmodel.1, mcp(variety = "Tukey"))

#need to fix the below line
#LetterResults <- multcomp::cld(dis_glht, alpha=0.05, Letters = LETTERS)

#why can't i use the oat dataset from MASS?
#model <- lmer(yield ~ variety + Fertilizer + (1|plot/subplot), data=oats)


library(car)
library(dplyr)
library(purrr)
library(tibble)


# Load directly from Google Drive (readr-style)
library(readr)

data <- read_csv("https://drive.google.com/uc?export=download&id=1Yap0vVnrf-5nJemsFQ8sSIH3RNLxU5WF")

# Select only numeric predictors and drop rows with missing values
numeric_predictors <- data %>%
  select(where(is.numeric))

# Visualize
library(corrplot)
cor_matrix <- cor(numeric_predictors, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.7)

climatechange_df <- read_csv("climate_change_allvariables.csv")

# Select only numeric predictors and drop rows with missing values
numeric_predictors <- climatechange_df %>%
  select(where(is.numeric))

# Remove identifier column
predictor_data <- resilience_clean %>% select(-Plot_ID)

# Loop over each variable and calculate max VIF
vif_results <- map_dfr(names(predictor_data), function(var) {
  # Build formula: var ~ all other variables
  formula <- as.formula(paste(var, "~ ."))
  # Create modeling dataset: use current var as response, others as predictors
  model_data <- predictor_data %>%
    select(-all_of(var)) %>%
    mutate(response = predictor_data[[var]])
  # Fit the model
  model <- lm(response ~ ., data = model_data)
  # Get VIFs
  vif_vals <- vif(model)
  tibble(response_var = var, max_vif = max(vif_vals, na.rm = TRUE))
})

print(vif_results)







library(MASS)

#full_model <- glm(response ~ ., data = data_reduced, family = binomial)
#stepwise_model <- stepAIC(full_model, direction = "both")
#summary(stepwise_model)



library(glmnet)

#x <- model.matrix(response ~ ., data_reduced)[, -1]
#y <- data_reduced$response

#lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")
#best_lambda <- lasso_model$lambda.min
#best_model <- glmnet(x, y, alpha = 1, family = "binomial", lambda = best_lambda)
#coef(best_model)


library(leaps)

#best_subset <- regsubsets(response ~ ., data = data_reduced, nbest = 1, really.big = TRUE)
#summary(best_subset)



library(MuMIn)
options(na.action = "na.fail")
#global_model <- glm(response ~ ., data = data_reduced, family = binomial)
#dredged_models <- dredge(global_model)
#averaged_model <- model.avg(dredged_models, subset = delta < 2)
#summary(averaged_model)



library(spsurvey)

# Define the study area and number of sample points
#study_area <- as.spatialPolygons(my_shapefile)
#n_samples <- 100

# Generate spatially balanced sample points
#sample_points <- grts(study_area, n = n_samples)

# Plot the sample points
#plot(study_area)
#points(sample_points, col = "red")



#test1 <- lmer(responsevariable ~ GrazingTreatment+Time+GrazingTreatment:Time, random~1|Plot/Time, data=yourdata)


#Note that you have to go to the CRAN website to install IPMpack for the first time:
#install.packages("IPMpack", repos = "http://R-Forge.R-project.org", type = "source")
#devtools::install_github("CRAN/IPMpack")

#Load packages
#library(IPMpack)
library(fields)
library(car)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggthemes) 

#remember in your own work that is it really convenient to set up a working directory! Here is an example:
#setwd("/Users/sks379/Desktop/pectisanalyses/FinalPectisAnalyses") #laptop 



#read in data (it needs to be imported as a dataframe names pectisdata)
#pectisdata <- read.csv("/Users/sks379/Desktop/pectisanalyses/FinalPectisAnalyses/FinalTransitionMatrices/exampleIPMendangeredplant.csv", header=TRUE)
url <- "https://drive.google.com/uc?export=download&id=10vtDeB4yXtic7lLa8oTpCDQft10vtYDM"
pectisdata <- read.csv(url)

#exclude poor quality data
#pectisipm <- dplyr::filter(pectisdata, Exclude == "0")

#prepare the dataset
pectis <- as.data.frame(pectisdata)
pectis$size <- as.numeric(as.character(pectis$size))
pectis$sizeNext <- as.numeric(as.character(pectis$sizeNext))

head(pectis)


#evaluating data for IPM construction
growth <- lm(sizeNext ~ size, na.action = na.omit, data=pectis)
resid <- residuals(growth)
shapiro.test(resid)
plot(fitted(growth), residuals(growth))
abline(lm(residuals(growth)~fitted(growth)))


id.n = length(pectis$id)
qqPlot(growth, distribution = "norm", id.method="y", id.cex = 0.6, id.n=id.n, id.col = "blue", id.location = "ab")


row_index <- 161
value <- pectis[row_index,]
print(value)

#or you can generate the id and use that to subset (just looking at the id number from above)
target_id <- 68
row_data_index <- pectis[pectis$ID == target_id, ]
print(row_data_index)

#check out the other outlier
row_index <- 84
value <- pectis[row_index,]
print(value) #note id number is 627

pectis <- pectis[-c(161),] 

growth <- lm(sizeNext ~ size, na.action = na.omit, data=pectis)

par(mfrow=c(1,2),mar=c(4,4,2,1))

id.n = length(pectis$id)
plot(sizeNext ~ size, xlab="Height (cm) year 1", ylab="Height (cm) year 2", data=pectis)
qqPlot(growth, distribution = "norm", id.method="y", id.cex = 0.6, id.n=id.n, id.col = "blue", id.location = "ab")
abline(lm(residuals(growth)~fitted(growth)))



pectis <- pectis[-c(82, 84),] 

growth <- lm(sizeNext ~ size, na.action = na.omit, data=pectis)
resid <- residuals(growth)
shapiro.test(resid)

par(mfrow=c(1,2),mar=c(4,4,2,1))

id.n = length(pectis$id)
plot(sizeNext ~ size, xlab="Height (cm) year 1", ylab="Height (cm) year 2", data=pectis)
qqPlot(growth, distribution = "norm", id.method="y", id.cex = 0.6, id.n=id.n, id.col = "blue", id.location = "ab")
abline(lm(residuals(growth)~fitted(growth)))


minimumsize1 <- min(pectis$size, na.rm=T); minimumsize1
minimumsize2 <- min(pectis$sizeNext, na.rm=T); minimumsize2
maximumsize1 <- max(pectis$size, na.rm=T); maximumsize1
maximumsize2 <- max(pectis$sizeNext, na.rm=T); maximumsize2
standarddev <- sd(pectis$size, na.rm=T); standarddev

minSize <- minimumsize2; minSize #true minimum
maxSize <- maximumsize2 + standarddev; maxSize #1 standard deviation all adult plants year 1
nBigMatrix = 100 #dictates bin size
x<-seq(from=0,to=135,length=1001) #Seems fairly standard to create a matrix with these dimensions
x0<-data.frame(size=x,size2=x*x)


#create a glm to predict survival
survival <- glm(surv ~ size, na.action = na.omit, family = "binomial", data=pectis)

# Install and load the DHARMa package
#install.packages("DHARMa",  dependencies = TRUE)
library(DHARMa)

residuals <- residuals(survival, type = "response")
test <- testResiduals(survival)

#if you need to identify outliers
object1 <- simulateResiduals(fittedModel = survival)
outliers(object1, lowerQuantile = 0, upperQuantile = 1,
  return = c("index", "logical"))

#you can do this for flowering, but this year everything flowered so you will get an error
flowering <- glm(fec1 ~ size, na.action = na.omit, family = "binomial", data = pectis)
residuals <- residuals(flowering, type = "response")
#test <- testResiduals(flowering)



reproduction <- filter(pectis, fec1 == "1")

seeds <- glm(fec2 ~ size, na.action = na.omit, family = "poisson", data = reproduction)
residuals <- residuals(seeds, type = "response")
qqnorm(residuals)
qqline(residuals)

test_seeds <- testResiduals(seeds)

object2 <- simulateResiduals(fittedModel = seeds)
outliers(object2, lowerQuantile = 0, upperQuantile = 1,
  return = c("index", "logical"))


#explore different regression models for relating growth, survival and reproduction to state variable
#=============================================
#explore vital rate data
par(mfrow=c(3,2),mar=c(4,4,2,1))
plot(pectis$size,jitter(pectis$surv),xlab="Size (t)", ylab="Survival to t+1")
plot(pectis$size,pectis$sizeNext,xlab="Size (t)",ylab="Size (t+1)") 
plot(pectis$size,jitter(pectis$fec1),xlab="Size (t)", ylab="Flowering probability")
plot(pectis$size,pectis$fec2,xlab="Size (t)",ylab="Flower Head Number") 
hist(pectis$sizeNext[is.na(pectis$size)], xlab="Recruit Size", main="")


#install.packages("IPMpack", repos = "http://R-Forge.R-project.org", type = "source")
#devtools::install_github("CRAN/IPMpack")
#see https://levisc8.github.io/ipmr/articles/ipmr-introduction.html

#Load packages
library(IPMpack)

#growth model constuction
growthModelComp(dataf = pectis, makePlot = TRUE, legendPos = "bottomright", mainTitle = "Growth")

#select best model - model with lowest values
#go <-makeGrowthObj(dataf = pectis, Formula = sizeNext~size + size2, regType = "constantVar", Family="gaussian")

#pectis_clean <- na.omit(pectis_surv)

#survModelComp(dataf = pectis_clean, makePlot = TRUE, legendPos = "bottomright", mainTitle = "Survival")
#survModelComp(dataf = pectis, makePlot = TRUE, legendPos = "bottomright", mainTitle = "Survival")

#select best model - model with lowest values
#so <- makeSurvObj(dataf = pectis_clean, Formula = surv~size+size2)



#Test various fertility models to select best model 
#Seed production of reproductive plants






#create dataframe to store results


library(iNEXT)
library(tidyverse)
#or
#library(devtools)
#install_github('AnneChao/iNEXT')

#First let's check out two abundance datasets

#Individual‐based abundance data (datatype="abundance"): Input data for each assemblage/site include species abundances in an empirical sample of n individuals (“reference sample”). When there are N assemblages, input data consist of an S by N abundance matrix, or N lists of species abundances.

#Check out spider
data("spider"); spider
#The spider dataset consists of abundance data from two canopy manipulation treatments (“Girdled” and “Logged”) of hemlock. Counts of individuals for species are provided for girdled and logged trees.

#Go ahead and estimate diversity
out <- iNEXT(spider, q=c(0, 1, 2), datatype="abundance", endpoint=500)
ggiNEXT(out, type=1, facet.var="Assemblage")

#Check out the other abundance dataset, called 'bird'
data("bird"); bird

#calculate diversity metrics for the bird dataset
birdtest <- iNEXT(bird, q=c(0,1,2), datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=50)

birdtest$AsyEst

#plot your bird data
ggiNEXT(birdtest, type=1, se=TRUE, facet.var="None", color.var="Assemblage", grey=FALSE)


#incidence dataset
data(ant)
str(ant)

data(ciliates)
str(ciliates)

#Run the following commands to get the output as shown below.
out.raw <- iNEXT(ciliates, q = 0, datatype="incidence_raw", endpoint=150)

#The following command returns the species diversity with a specified level of sample coverage of 98.5% for the ant data. For some assemblages, this coverage value corresponds to rarefaction (i.e., less than the coverage of the reference sample), while for the others it corresponds to extrapolation (i.e., greater than the coverage of the reference sample), as indicated under the method column of the output.

estimateD(ant, datatype="incidence_freq",
 base="coverage", level=0.985, conf=0.95)

library(readr)

# Download and read the first dataset
url1 <- "https://drive.google.com/uc?export=download&id=1c1guQTQwGNfA5Kx-xNPKRDsSJrBR-HxW"
env_matrix <- read_csv(url1)

# Download and read the second dataset
url2 <- "https://drive.google.com/uc?export=download&id=1jCqfJPEuEWo4uNZfxW43VkSvEDDtayF5"
species_matrix <- read_csv(url2)


# Load necessary libraries
library(tidyverse)

print(colnames(species_matrix))
print(colnames(env_matrix))

# combine the datasets, by hand or with code!
combined_data <- inner_join(species_matrix, env_matrix, by = c("...1" = "...1"))

# Split the data into different groups
control_pre <- combined_data %>% 
  filter(Treatment == "Control" & Treatment_status == "PreTreatment")

#remove unnecessary columns
colnames(control_pre)
control_pre <- dplyr::select(control_pre, -c(Year, PlotN, Treatment, Treatment_status, ...1))
control_pret <- t(control_pre)

control_post <- combined_data %>% 
  filter(Treatment == "Control" & Treatment_status == "PostTreatment")
colnames(control_post)
control_post <- dplyr::select(control_post, -c(Year, PlotN, Treatment, Treatment_status, ...1))
control_postt <- t(control_post)

treatment_pre <- combined_data %>% 
  filter(Treatment == "Treatment" & Treatment_status == "PreTreatment")
colnames(treatment_pre)
treatment_pre <- dplyr::select(treatment_pre, -c(Year, PlotN, Treatment, Treatment_status, ...1))
treatment_pret <- t(treatment_pre)

treatment_post <- combined_data %>% 
  filter(Treatment == "Treatment" & Treatment_status == "PostTreatment")
colnames(treatment_post)
treatment_post <- dplyr::select(treatment_post, -c(Year, PlotN, Treatment, Treatment_status, ...1))
treatment_postt <- t(treatment_post)

#combine matrices, control_pre, control_post, treatment_pre, treatment_post, in a matrix of N lists
combined_list <- list(control_pre = control_pre,
                      control_post = control_post,
                      treatment_pre = treatment_pre,
                      treatment_post = treatment_post)

lapply(combined_list, head)

combined_list <- lapply(combined_list, function(x) {
  if(is.data.frame(x)) as.matrix(x) else x
})

str(combined_list)


library(iNEXT)

# Example of running iNEXT on incidence_raw data
results <- suppressWarnings({iNEXT(combined_list, q=0, datatype="incidence_raw")})

# Plot the results
plot(results)

#you have to adjust the code to port in this datasets!
library(readr)

url3 <- "https://drive.google.com/uc?export=download&id=1bpLmtDDEPAdSaTPFPNg33hyj6y0viqxk"
arcticlocs <- read_csv(url3)

#time periods
arcticlocsTP1 <- filter(arcticlocs, year >= 1939 & year < 1979)
arcticlocsTP2 <- filter(arcticlocs, year >= 1980 & year < 2021)

obsTP1 <- length(arcticlocsTP1$genus)
obsTP2 <- length(arcticlocsTP2$genus)

lengthsofobs <- c(obsTP1, obsTP2)

#Time period 1
arcticlocsTP1sum <- plyr::count(arcticlocsTP1, 'genus')
arcticlocsTP1unitnum <- length(arcticlocsTP1$genus)
arcticlocsTP1V <- c(arcticlocsTP1unitnum, arcticlocsTP1sum$freq)
arcticlocsTP1Vn <- as.numeric(arcticlocsTP1V)

#Time period 2
arcticlocsTP2sum <- plyr::count(arcticlocsTP2, 'genus')
arcticlocsTP2unitnum <- length(arcticlocsTP2$genus)
arcticlocsTP2V <- c(arcticlocsTP2unitnum, arcticlocsTP2sum$freq)
arcticlocsTP2Vn <- as.numeric(arcticlocsTP2V)

arctic <- list("TimePeriod1" = arcticlocsTP1Vn,
               "TimePeriod2" = arcticlocsTP2Vn)

str(arctic)

#establish the max extrapolation number (two times the largest sample) and the knots manually
maxrun <- (max(obsTP1, obsTP2)*2)
t <- seq(1, maxrun, by=1)

out.inc <- iNEXT(arctic,  q=c(0,1,2), datatype="incidence_freq", size=t, nboot=200)

arcticfigure <- ggiNEXT(out.inc, facet.var="Order.q", se =TRUE, grey = TRUE) + ylab("Generic diversity") + ggtitle("Arctic cordillera") + theme_bw() + 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)); arcticfigure

library(dagitty)
library(ggdag)

dag <- dagify(
  SoilMoisture ~ CanopyCover,
  CanopyCover ~ Fire,
  labels = c(Fire = "Fire Frequency", CanopyCover = "Canopy Cover", SoilMoisture = "Soil Moisture"),
  exposure = "Fire", outcome = "SoilMoisture"
)
ggdag(dag, text = FALSE, use_labels = "label") + theme_dag()

dag <- dagify(
  PlantGrowth ~ SoilNitrogen + PathogenLoad,
  labels = c(SoilNitrogen = "Soil N", PathogenLoad = "Pathogens", PlantGrowth = "Plant Growth"),
  exposure = "SoilNitrogen", outcome = "PathogenLoad"
)
ggdag(dag, text = FALSE, use_labels = "label") + theme_dag()

dag <- dagify(
  InvasiveCover ~ SoilType,
  NativeRichness ~ SoilType,
  labels = c(SoilType = "Soil Type", InvasiveCover = "Invasive Cover", NativeRichness = "Native Richness"),
  exposure = "InvasiveCover", outcome = "NativeRichness"
)
ggdag(dag, text = FALSE, use_labels = "label") + theme_dag()

library(dagitty)
library(ggdag)
library(tidyverse)


# Define the DAG
ivm_dag <- dagitty("
dag {
  Treatment
  Soil_Substrate
  Cattle
  Plant_Richness
  Plant_Cover
  Plant_Height
  Ceanothus
  Woody_Debris
  Pollinator_Richness
  Pollinator_Abundance

  Treatment -> Plant_Richness
  Treatment -> Plant_Cover
  Treatment -> Plant_Height
  Treatment -> Ceanothus
  Treatment -> Woody_Debris

  Soil_Substrate -> Treatment
  Soil_Substrate -> Plant_Richness
  Soil_Substrate -> Plant_Cover
  Soil_Substrate -> Woody_Debris

  Cattle -> Plant_Richness
  Cattle -> Plant_Cover
  Cattle -> Pollinator_Abundance
  Cattle -> Pollinator_Richness

  Plant_Richness -> Pollinator_Richness
  Plant_Richness -> Pollinator_Abundance
  Plant_Cover -> Pollinator_Richness
  Plant_Cover -> Pollinator_Abundance
  Plant_Height -> Pollinator_Richness
  Plant_Height -> Pollinator_Abundance
  Ceanothus -> Pollinator_Richness
  Ceanothus -> Pollinator_Abundance
  Woody_Debris -> Pollinator_Richness
  Woody_Debris -> Pollinator_Abundance
}
")

node_roles <- tibble(
  name = c(
    "Treatment",
    "Soil_Substrate",
    "Cattle",
    "Plant_Richness",
    "Plant_Cover",
    "Plant_Height",
    "Ceanothus",
    "Woody_Debris",
    "Pollinator_Richness",
    "Pollinator_Abundance"
  ),
  role = c(
    "Treatment",
    "Context",
    "Context",
    "Plant",
    "Plant",
    "Plant",
    "Plant",
    "Plant",
    "Pollinator",
    "Pollinator"
  )
)

# Get layout and merge roles
dag_df <- tidy_dagitty(ivm_dag, layout = "nicely") %>%
  left_join(node_roles, by = "name")

# Plot using ggplot2 with corrected edge structure
ggplot() +
  geom_segment(
    data = dag_df %>% filter(!is.na(xend)),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.02, "npc")),
    color = "grey40"
  ) +
  geom_point(
    data = dag_df %>% filter(!is.na(x)), 
    aes(x = x, y = y, color = role), 
    size = 8, alpha = 0.85
  ) +
  geom_text(
    data = dag_df %>% filter(!is.na(x)), 
    aes(x = x, y = y, label = name), 
    color = "black", size = 4
  ) +
  scale_color_manual(values = c(
    "Treatment" = "#FB7E21FF",
    "Context" = "#A91601FF",
    "Plant" = "#18DDC2FF",
    "Pollinator" = "#00468BFF"
  )) +
  labs(
    title = "Hypothesized DAG for Pollinator Response to IVM Treatment",
    color = "Variable Type"
  ) +
  theme_void()

adjustmentSets(ivm_dag, exposure = "Treatment", outcome = "Pollinator_Richness")
adjustmentSets(ivm_dag, exposure = "Treatment", outcome = "Pollinator_Abundance")

library(lavaan)
library(mvtnorm)
library(mvnormtest)
library(psych)
library(Matrix)

# Load necessary package
if (!requireNamespace("mvnormtest", quietly = TRUE)) {
  install.packages("mvnormtest")
}
library(mvnormtest)

# Simulate multivariate normal data
set.seed(123)
data <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100)
)

# Apply Mardia-Shapiro-Wilk test (requires transpose)
mshapiro.test(t(data))

# Check skew and kurtosis
psych::describe(data)

# Check for missing data
summary(is.na(data))  # Should be all FALSE


# Load the lavaan package
if (!requireNamespace("lavaan", quietly = TRUE)) {
  install.packages("lavaan")
}
library(lavaan)

# Define a simple SEM model
model <- '
  y1 ~ x1 + x2
  y2 ~ y1
'

# Simulate data
set.seed(123)
data <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y1 = rnorm(100),
  y2 = rnorm(100)
)

# Fit the model
fit <- sem(model, data = data)

# Print summary
summary(fit)

# Count parameters
n_params <- lavInspect(fit, "npar")  # Number of free parameters
n_obs <- nrow(data)

# Portnoy's rule
portnoy_value <- n_params^(3/2) / n_obs
portnoy_value

# Number of unique observed covariances
p <- ncol(data)
n_cov <- p * (p + 1) / 2

# Compare to number of free parameters
n_cov
lavInspect(fit, "npar")  # Should be ≤ n_cov

library(lavaan)

# define model as a lavaan-style string
model <- '
  cover ~ age + elev
  firesev ~ age + cover
'

fit <- sem(model, data = keeley)
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea"))

# Load dataset
# Example: keeley <- read.csv("path/to/your/keeley_data.csv")

# Fit SEM
#model1 <- 'cover ~ age'
#fit1 <- sem(model1, data = keeley)
#summary(fit1, standardized = TRUE, rsquare = TRUE)

#fit1_mean <- sem(model1, data = keeley, meanstructure = TRUE)
#summary(fit1_mean)

#lavaanPlot(model = fit1, coefs = TRUE, stand = TRUE)

#model2 <- '
#  firesev ~ age
#  cover ~ firesev + age
#'
#fit2 <- sem(model2, data = keeley)
#summary(fit2, standardized = TRUE, rsquare = TRUE)

#model3 <- '
#  firesev ~ af*age
#  cover ~ fc*firesev + ac*age

  # Derived
# indirect := af * fc
#  total := ac + (af * fc)
#'
#fit3 <- sem(model3, data = keeley)
#standardizedSolution(fit3)

#varTable(fit3)

#lavaanPlot(
#  model = fit3,
#  coefs = TRUE,
#  stand = TRUE,
#  sig = 0.05,
#  graph_options = list(layout = "circo")
#)

#model_final <- '
#  rich ~ distance + abiotic + hetero
#  hetero ~ distance
#  abiotic ~ distance
#  abiotic ~~ hetero
#'
#fit_final <- sem(model_final, data = keeley)
#summary(fit_final, standardized = TRUE)

library(lavaan)
library(mvnormtest)
library(MVN)

#model_full <- '
#  firesev ~ age
#  cover ~ firesev
#'

#fit_full <- sem(model_full, data = keeley, meanstructure = TRUE)
#summary(fit_full, fit.measures = TRUE)

#residuals(fit_full, type = "cor")  # residual correlations
#modificationIndices(fit_full, standardized = FALSE, sort. = TRUE)

library(MVN)

# Step 1: Get residuals from lavaan model
#resids <- lavPredict(fit_full, type = "ov")  # residuals for observed variables

# Optional: check univariate normality visually
#apply(resids[, 1:2], 2, function(x) {
#  qqnorm(x); qqline(x)
#})

# Step 2: Multivariate Shapiro-Wilk (for n ≤ 50)
#mshapiro.test(t(resids[, 1:2]))

# Step 3: Mardia's test for multivariate normality
#MVN::mvn(data = resids[, 1:2], mvn_test = "mardia")

# Step 1: Get residuals from lavaan model
#tryCatch({
#  resids <- lavPredict(fit_full, type = "ov")
  
  # Make sure residuals are numeric and have no missing values
#  if (!is.null(resids) && is.matrix(resids) && all(is.finite(resids[, 1:2]))) {
    # Step 2: Multivariate Shapiro-Wilk (for n ≤ 50)
#    print(mshapiro.test(t(resids[, 1:2])))

    # Step 3: Mardia's test
#    print(MVN::mvn(data = resids[, 1:2], mvn_test = "mardia"))
#  } else {
#    message("Residuals are missing, not numeric, or contain NA/Inf values.")
#  }
#}, error = function(e) {
#  message("MVN test failed: ", conditionMessage(e))
#})

#fit_sb <- sem(model_full, data = keeley, test = "Satorra.Bentler")
#summary(fit_sb)

#fit_bs <- sem(model_full, data = keeley, test = "bollen.stine", se = "boot", bootstrap = 1000)
#summary(fit_bs)

# Load required libraries
library(lavaan)
library(AICcmodavg)

# Fully Mediated Model
fullMedModel <- '
  firesev ~ age
  cover ~ firesev
'
fullMedSEM <- sem(fullMedModel, data = keeley)

# Partially Mediated Model
partialMedModel <- '
  firesev ~ age
  cover ~ firesev + age
'
partialMedSEM <- sem(partialMedModel, data = keeley)

# Likelihood Ratio Test (nested models)
anova(partialMedSEM, fullMedSEM)

# AICc model comparison
aictab(
  cand.set = list(fullMedSEM, partialMedSEM),
  modnames = c("Full", "Partial")
)

# Sample covariance matrix from Santos & Cannatella (2011)
# santosCov <- read.table("https://raw.githubusercontent.com/username/santosCov.txt", na.strings = #".")
#santosCov <- as.matrix(santosCov)

# Create covariance matrix manually (example values)
santosCov <- matrix(c(
  1.00,  0.45, 0.38,
  0.45,  1.00, 0.50,
  0.38,  0.50, 1.00
), nrow = 3, byrow = TRUE)

# Add row and column names (must match your CFA model exactly)
colnames(santosCov) <- rownames(santosCov) <- c("Alkaloid.quantity", "Alkaloid.diversity", "Conspicuous.coloration")

# Specify CFA model
santosCFA1 <- '
  Aposematism =~ Alkaloid.quantity + Alkaloid.diversity + Conspicuous.coloration
'

# Fit the model
santosFit1 <- sem(santosCFA1, sample.cov = santosCov, sample.nobs = 21)
summary(santosFit1, standardized = TRUE)

#santosSize <- '
#  Size =~ Log.Mass + Log.RMR + Log.Scope
#'

#santosSizeFit <- sem(santosSize, sample.cov = santosCov, sample.nobs = 21)
#summary(santosSizeFit, standardized = TRUE)

#santosCFA2 <- '
#  Aposematism =~ Alkaloid.quantity + Alkaloid.diversity + Conspicuous.coloration + #Ant.Mite.Specialization + log.Prey
#  Scale =~ Log.Mass + Log.RMR + Log.Scope + Conspicuous.coloration
#'

#santosFit2 <- sem(santosCFA2, sample.cov = santosCov, sample.nobs = 21)
#summary(santosFit2, standardized = TRUE)

library(lavaan)
library(semPlot)

# Define the SEM
model <- '
  # Measurement model
  Performance =~ perf1 + perf2 + perf3

  # Structural model
  Performance ~ Treatment
'

# Simulate data
set.seed(123)
n <- 200
Treatment <- rnorm(n)
perf1 <- 0.6*Treatment + rnorm(n, sd = 1)
perf2 <- 0.6*Treatment + rnorm(n, sd = 1)
perf3 <- 0.6*Treatment + rnorm(n, sd = 1)
data <- data.frame(Treatment, perf1, perf2, perf3)

# Fit the SEM
fit <- sem(model, data = data)
summary(fit, standardized = TRUE)

semPaths(fit, "std", layout = "tree", whatLabels = "std")

# Define DAG
g <- dagitty("dag {
  x -> y2 -> y1
  x -> y1
}")

# View conditional independencies implied by the DAG
impliedConditionalIndependencies(g)

library(piecewiseSEM)

# Simulate example data
set.seed(123)
n <- 100
x <- rnorm(n)
y2 <- 0.5 * x + rnorm(n)
y1 <- 0.6 * x + 0.4 * y2 + rnorm(n)
example_data <- data.frame(x, y1, y2)

# Fit piecewise SEM
mod_list <- psem(
  lm(y2 ~ x, data = example_data),
  lm(y1 ~ x + y2, data = example_data)
)

# ✅ Get model fit statistics (replaces sem.fit)
fisherC(mod_list)

# ✅ Get standardized path coefficients (replaces sem.coefs)
stdCoefs(mod_list)

# ✅ Optional: View DAG
plot(getDAG(mod_list))

# Load libraries
library(piecewiseSEM)
library(visreg)
library(DiagrammeR)

data(keeley)  # from piecewiseSEM and work through this information 

# Fit individual models
mod1 <- lm(abiotic ~ distance, data = keeley)
mod2 <- lm(hetero ~ distance, data = keeley)
mod3 <- lm(rich ~ abiotic + hetero, data = keeley)

# Combine into a psem object
keeley_sem <- psem(mod1, mod2, mod3)

# Directed separation test
#dSep(keeley_sem)

# Fisher's C statistic
#fisherC(keeley_sem)

# Coefficients
coefs(keeley_sem)

# R-squared values
rsquared(keeley_sem)


keeley_sem <- psem(
  lm(firesev ~ age + cover, data = keeley),
  lm(cover ~ age + elev + firesev, data = keeley),
  data = keeley
)

plot(keeley_sem)

#Option B: Refined Graph with DiagrammeR

# Optional customization
plot(keeley_sem,
     node_attrs = list(
       x = c(2.5, 2.5, 4, 1),
       y = c(3, 1, 2, 2),
       shape = "rectangle",
       fillcolor = "white"
     ))

mod_firesev  <- lm(firesev ~ age, data = keeley)
mod_cover    <- lm(cover ~ firesev, data = keeley)

firesev_model <- psem(mod_firesev, mod_cover)

summary(firesev_model)
dSep(firesev_model)
rsquared(firesev_model)

# Visualize fire severity's effect on cover
visreg(firesev_model[[2]], xvar = "firesev")

# Download data
url <- "https://drive.google.com/uc?export=download&id=1oHBul4_JcqlPFZgYsH3WOIZJRQRw1O4F"
cardinale <- read.csv(url)

# Check it loaded
head(cardinale)

# Log-transform variables
cardinale$logN <- log10(cardinale$N + 1e-6)
cardinale$logN2 <- cardinale$logN^2
cardinale$logChl <- log10(cardinale$Chl)

#Fit SEM with piecewiseSEM

model1 <- psem(
  lm(SA ~ logN + logN2 + SR, data = cardinale),
  lm(logChl ~ SA + logN + logN2, data = cardinale),
  logN %~~% logN2,
  data = cardinale
)

summary(model1)

# Center predictors
cardinale$logN.cen <- scale(cardinale$logN, scale = FALSE)
cardinale$logN2.cen <- cardinale$logN.cen^2

# Check correlation
cor(cardinale$logN.cen, cardinale$logN2.cen)

model2 <- psem(
  lm(SA ~ logN.cen + logN2.cen + SR, data = cardinale),
  lm(logChl ~ SA + logN.cen + logN2.cen, data = cardinale),
  logN.cen %~~% logN2.cen,
  data = cardinale
)

summary(model2)


url2 <- "https://drive.google.com/uc?export=download&id=1YTsFP1T__Hn13hTvj9TVOK-wbGDxLd01"

# Try to read the CSV directly
keeley <- read.csv(url2)

keeley$age_cent <- scale(keeley$age, scale = FALSE)
keeley$fire_cent <- scale(keeley$firesev, scale = FALSE)
keeley$int_term <- keeley$age_cent * keeley$fire_cent

keeley_int <- psem(
  lm(cover ~ age_cent * fire_cent, data = keeley),
  lm(fire_cent ~ age_cent, data = keeley),
  data = keeley
)

summary(keeley_int)

# Simulated structure to mimic Anderson et al.
set.seed(42)
anderson <- data.frame(
  biomass.kg = rnorm(100, mean = 10, sd = 2),
  leafN = rnorm(100, mean = 3, sd = 0.5),
  landscape = sample(0:1, 100, replace = TRUE),
  hotspotYN = rbinom(100, 1, 0.4)
)

library(piecewiseSEM)

anderson.sem <- psem(
  lm(leafN ~ biomass.kg, data = anderson),
  glm(hotspotYN ~ leafN + biomass.kg + landscape, family = "binomial", data = anderson)
)

summary(anderson.sem)

# Add the 'conserve = TRUE' argument to be conservative in tests
summary(anderson.sem, conserve = TRUE)

dSep(anderson.sem, direction = c("hotspotYN <- leafN"))

anderson.sem2 <- update(anderson.sem, hotspotYN %~~% leafN)
dSep(anderson.sem2)

anderson.glm <- anderson.sem[[2]]
Betas <- coefs(anderson.sem)[2:4, 3]  # GLM coefficients
preds <- predict(anderson.glm, type = "link")

sd.y.LT <- sqrt(var(preds) + pi^2/3)
sd.x <- sapply(anderson[, c("leafN", "biomass.kg", "landscape")], sd)

Betas.LT <- Betas * sd.x / sd.y.LT
Betas.LT

preds_response <- predict(anderson.glm, type = "response")
R <- cor(anderson$hotspotYN, preds_response)

sd.y.OE <- sqrt(var(preds_response)) / R
Betas.OE <- Betas * sd.x / sd.y.OE
Betas.OE

# Indirect effect: leafN → hotspotYN (through biomass.kg)
Beta.leafN <- coefs(anderson.sem)$Std.Estimate[1]
indirect_LT <- Beta.leafN * Betas.LT[1]
indirect_OE <- Beta.leafN * Betas.OE[1]

c(LT = indirect_LT, OE = indirect_OE)

library(lme4)
library(piecewiseSEM)
library(emmeans)
library(lavaan)


library(multcompView)

# Simulated structure: Genotype nested within Phragmites status (e.g., native, invasive)
set.seed(1)
n <- 90
bowen <- data.frame(
  status = factor(rep(c("native", "invasive", "introduced"), each = 30)),
  Genotype = rep(paste0("G", 1:9), each = 10),
  observed_otus = rnorm(n, mean = 2500, sd = 100),
  RNA.DNA = rnorm(n, 0.7, 0.05),
  below.C = rnorm(n, 43, 1),
  abovebiomass_g = rnorm(n, 2, 0.5)
)

# Mixed models for each component
div_mod <- lmer(observed_otus ~ status + (1 | Genotype), data = bowen)
activity_mod <- lmer(RNA.DNA ~ status + observed_otus + (1 | Genotype), data = bowen)
carbon_mod <- lmer(below.C ~ observed_otus + status + (1 | Genotype), data = bowen)
biomass_mod <- lmer(abovebiomass_g ~ RNA.DNA + observed_otus + below.C + status + (1 | Genotype), data = bowen)

# Build piecewise SEM
bowen_mod <- psem(div_mod, activity_mod, carbon_mod, biomass_mod, data = bowen)
summary(bowen_mod)

lapply(bowen_mod[-length(bowen_mod)], emmeans, specs = ~status)

# Post-hoc Tests (Tukey)

generic_tukey <- function(x) emmeans(x, list(pairwise ~ status))
lapply(bowen_mod[-length(bowen_mod)], generic_tukey)

# Create simulated dataset
group_df <- data.frame(
  site = rep(c("A", "B"), each = 50),
  x = rnorm(100),
  m = rnorm(100),
  y = rnorm(100)
)

# Define a simple SEM model
sem_model <- '
  m ~ a*x
  y ~ b*m + c*x
'

# Fit multi-group SEM
fit_multi <- lavaan::sem(sem_model, data = group_df, group = "site")

# View summary
summary(fit_multi, fit.measures = TRUE, standardized = TRUE)

library(lavaan)

# Simulate data
set.seed(123)
n <- 100
group <- rep(c("A", "B"), each = n)
x <- rnorm(2 * n)
m <- 0.5 * x + rnorm(2 * n)
y <- 0.6 * m + 0.3 * x + rnorm(2 * n)
data <- data.frame(group, x, m, y)

model <- '
  m ~ a*x
  y ~ b*m + c*x
'

fit_multi <- sem(model, data = data, group = "group")
summary(fit_multi, fit.measures = TRUE, standardized = TRUE)

# Constrain 'a' and 'b' to be equal across groups
model_constrained <- '
  m ~ c(a, a)*x
  y ~ c(b, b)*m + c*x
'

fit_constrained <- sem(model_constrained, data = data, group = "group")
anova(fit_multi, fit_constrained)  # Chi-square test for invariance

library(semPlot)
semPaths(fit_multi, "std", layout = "tree", whatLabels = "std", edge.label.cex = 1.2)

# Log-transform predictors
cardinale$logN <- log10(cardinale$N + 1e-6)
cardinale$logN2 <- cardinale$logN^2
cardinale$logChl <- log10(cardinale$Chl)

# Centering predictors to reduce multicollinearity
cardinale$logN.cen <- scale(cardinale$logN, scale = FALSE)
cardinale$logN2.cen <- scale(cardinale$logN^2, scale = FALSE)

cardinale.sem <- psem(
  lm(SA ~ logN.cen + logN2.cen + SR, data = cardinale),
  lm(logChl ~ SA + logN.cen + logN2.cen, data = cardinale),
  logN.cen %~~% logN2.cen,
  data = cardinale
)

summary(cardinale.sem)

#cardinale.mixed <- psem(
#  lme(SA ~ logN.cen + logN2.cen + SR, random = ~1 | Stream, data = cardinale),
#  lme(logChl ~ SA + logN.cen + logN2.cen, random = ~1 | Stream, data = cardinale),
#  logN.cen %~~% logN2.cen,
#  data = cardinale
#)

#summary(cardinale.mixed)

# Compare R-squared
#rsquared(cardinale.sem)
#rsquared(cardinale.mixed)

# Example if "site_mean" were available
# cardinale$stream_mean_logN <- ave(cardinale$logN, cardinale$Stream)
# cardinale$deviation_logN <- cardinale$logN - cardinale$stream_mean_logN

#fisherC(cardinale.mixed)

library(readr)

# Read CSV directly into R
powerline_plants <- read.csv("https://drive.google.com/uc?export=download&id=1MVlaEshEn7M2g8rOiJ8fWPPozn3WgNck")


library(dagitty)
library(ggdag)
library(tidyverse)


# Define the DAG
ivm_dag <- dagitty("
dag {
  Treatment
  Soil_Substrate
  Cattle
  Plant_Richness
  Plant_Cover
  Plant_Height
  Ceanothus
  Woody_Debris
  Pollinator_Richness
  Pollinator_Abundance

  Treatment -> Plant_Richness
  Treatment -> Plant_Cover
  Treatment -> Plant_Height
  Treatment -> Ceanothus
  Treatment -> Woody_Debris

  Soil_Substrate -> Treatment
  Soil_Substrate -> Plant_Richness
  Soil_Substrate -> Plant_Cover
  Soil_Substrate -> Woody_Debris

  Cattle -> Plant_Richness
  Cattle -> Plant_Cover
  Cattle -> Pollinator_Abundance
  Cattle -> Pollinator_Richness

  Plant_Richness -> Pollinator_Richness
  Plant_Richness -> Pollinator_Abundance
  Plant_Cover -> Pollinator_Richness
  Plant_Cover -> Pollinator_Abundance
  Plant_Height -> Pollinator_Richness
  Plant_Height -> Pollinator_Abundance
  Ceanothus -> Pollinator_Richness
  Ceanothus -> Pollinator_Abundance
  Woody_Debris -> Pollinator_Richness
  Woody_Debris -> Pollinator_Abundance
}
")

node_roles <- tibble(
  name = c(
    "Treatment",
    "Soil_Substrate",
    "Cattle",
    "Plant_Richness",
    "Plant_Cover",
    "Plant_Height",
    "Ceanothus",
    "Woody_Debris",
    "Pollinator_Richness",
    "Pollinator_Abundance"
  ),
  role = c(
    "Treatment",
    "Context",
    "Context",
    "Plant",
    "Plant",
    "Plant",
    "Plant",
    "Plant",
    "Pollinator",
    "Pollinator"
  )
)

# Get layout and merge roles
dag_df <- tidy_dagitty(ivm_dag, layout = "nicely") %>%
  left_join(node_roles, by = "name")

# Plot using ggplot2 with corrected edge structure
ggplot() +
  geom_segment(
    data = dag_df %>% filter(!is.na(xend)),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.02, "npc")),
    color = "grey40"
  ) +
  geom_point(
    data = dag_df %>% filter(!is.na(x)), 
    aes(x = x, y = y, color = role), 
    size = 8, alpha = 0.85
  ) +
  geom_text(
    data = dag_df %>% filter(!is.na(x)), 
    aes(x = x, y = y, label = name), 
    color = "black", size = 4
  ) +
  scale_color_manual(values = c(
    "Treatment" = "#FB7E21FF",
    "Context" = "#A91601FF",
    "Plant" = "#18DDC2FF",
    "Pollinator" = "#00468BFF"
  )) +
  labs(
    title = "Hypothesized DAG for Pollinator Response to IVM Treatment",
    color = "Variable Type"
  ) +
  theme_void()

library(piecewiseSEM)

# List all variables used across all models
sem_vars <- c(
  "Species_Richness", "Treatment", "Q_Substrate_BareSoil",
  "Plot_DungCount", "Q_Substrate_PerennialVeg", "Height_cm",
  "Q_Substrate_WoodyDebris"
)

# Remove all rows with NA in any of the SEM variables
sem_data <- powerline_plants %>%
  dplyr::select(all_of(sem_vars)) %>%
  na.omit()

# Ensure Treatment is a factor
sem_data$Treatment <- as.factor(sem_data$Treatment)

# Check structure to confirm
str(sem_data)

sem_data$Treatment <- droplevels(as.factor(sem_data$Treatment))

# Fit models using this cleaned dataset
mod1 <- lm(Species_Richness ~ Treatment + Q_Substrate_BareSoil + Plot_DungCount, data = sem_data)
mod2 <- lm(Q_Substrate_PerennialVeg ~ Species_Richness + Treatment + Q_Substrate_BareSoil + Plot_DungCount, data = sem_data)
mod3 <- lm(Height_cm ~ Q_Substrate_PerennialVeg + Treatment, data = sem_data)
mod4 <- lm(Q_Substrate_WoodyDebris ~ Height_cm + Treatment + Q_Substrate_BareSoil, data = sem_data)
# You can now also safely fit mod6 and mod7 using the same sem_data:
# mod6 <- lm(Pollinator_Richness ~ Species_Richness + Q_Substrate_PerennialVeg + Height_cm + Ceanothus + Q_Substrate_WoodyDebris + Plot_DungCount, data = sem_data)
# mod7 <- lm(Pollinator_Abundance ~ Species_Richness + Q_Substrate_PerennialVeg + Height_cm + Ceanothus + Q_Substrate_WoodyDebris + Plot_DungCount, data = sem_data)

# Assemble SEM
mods <- list(mod1, mod2, mod3, mod4)
lapply(mods, function(m) summary(m)$coefficients)

# Rebuild the SEM with standardized estimates
sem_model <- psem(mod1, mod2, mod3, mod4)

# Summary with standardization explicitly requested
summary(sem_model, standardize = "scale")


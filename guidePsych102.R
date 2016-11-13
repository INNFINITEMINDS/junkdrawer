############################# THE MEGA FINAL GUIDE!!!!!!!!!!!!!!!!!!!!


#Tutorials 1 - 3
###########  Commands and Basics###############################################

#Commands that help
help(log)
?log
??log
apropos("log")
help.search("log")

#Library Commands
library(car) #
library (effects)
library(splines)

#Commands that  Explain
summary(Davis) 
blah= lm(weight~repwt, data=Davis)
summary(blah)
objects(Davis) #puts in 
class()
levels()
length() #It needs a list: Davis$sex 
sum()
names(Davis) #puts in actual order
head(Davis) #shows data
tail(Davis)
nrow(Davis) #needs a data set
print()
some(Davis) #shows data
xtabs(~ group, data=Baumann) # tells you the number of different factors in a category                    
model.matrix (~ type + education + education:type,data=Prestige)[select, ]   

# Commands that help you list.
<- array(0,10)
<- seq(1,10)
<- rep(0,10)
= c(1,2,3,4)

# Sequence operator
1:4   
4:1
-1:2
seq(1,4)
seq(2, 8, by=2)
seq(0, 1, by=.1)
m+seq(0, 1, length=11)

#Combine Function Arithmetic
c(1,2,3,4)/2    
c(1,2,3,4)/c(4,3,2,1)
log(c(0.1,1,10,100), 10)
c(1,2,3,4) + c(4,3)
c(1,2,3,4) + c(4,3,2)

#Basic Indexing. Run this: x= 1:100
x[21]  
x[11:20]
x[-(11:100)]
(z <- x[1:10])
z < -0.5
z > 5
z < -0.5 | z > 0.5   # | is vectored "or", & is "and"
abs(z) > 5
z[abs(z) > 5] # indexing by a logical vector

# User-Defined Functions
my.mean <- function(x) sum(x)/length(x)
my.mean(x)
my.mean(y)
my.mean(1:100)
###########  Editing#########################################################

#Editing Models #subset
(davis.mod.2 <- update(davis.mod, subset=-12)) #creates subset that removes the outlier 12
scatterplot(davis.mod.2)

Duki1= subset (Prestige$prestige, Prestige$prestige > 60)
Duki2= subset (Prestige, Prestige$prestige> 60)
(Duki1)
plot(Duki2)
abline(Duki2)

#Selecting Data                                           
select <- c(1, 2, 35, 36, 61, 62) # a few rows
Prestige$type[select]  # a few values

#rearrangiing factors
Moore$fcategory <- factor(Moore$fcategory,
                          levels=c("low", "medium", "high"))
Moore$partner.status <- relevel(Moore$partner.status, ref="low")

Prestige$type <- with(Prestige, factor(type,levels=c("bc", "wc", "prof")))

Salaries$rank <- relevel(Salaries$rank, ref="AsstProf")
###########  Factors#############################################
#Contrast Coding
contrasts #(Prestige$type)

#Contrast Example
(z <- factor(rep(c("a", "b", "c", "d"), c(3, 2, 4, 1))))
model.matrix(~ z)
contrasts(z)                                            

#Rearranging Factor levels
Prestige$type <- with(Prestige, factor(type,levels=c("bc", "wc", "prof")))

#Factors transformed into numbers or characters
(type.number <- as.numeric(Prestige$type))
type.number[select]
class(type.number)
type.character <- as.character(Prestige$type)
type.character[select]
class(type.character)
type.factor <- factor(type.character, levels=c("bc", "wc", "prof"))
type.factor[select]
###########  Equations and Regressions############################################### 

#Equation Commands and Equations
mean() #sum(x)/length(x)
mean(Davis$repwt, na.rm=T) #How to get the Mean from a data set with NA in it.
sd() 

sampMeans[1:100] <- sapply( 1:100, function(x) mean(rnorm(100,mean=100,sd=10)))

sd(sampMeans)
10/sqrt(length(sampMeans)-1)

attach(Davis)
idx <- !is.na(repwt) & !is.na(weight) #These are the equations for the coefficients with the way to
summary (idx)                         # avoid the NA incorperated in it.
b <- sum((repwt[idx]-mean(repwt[idx]))*(weight[idx]-mean(weight[idx])))/
  sum((repwt[idx]-mean(repwt[idx]))^2)
a <- mw - b*mrw             #mean weight - ((b) (mean repwt))
b
confint(davis.mod) #Gets you the confidence intervals of Model

#Regressions
lm() #(weight ~ repwt, data=Davis)   <- Bivariate
lm #(prestige ~ type, data=Prestige) <- Bivariate with a factor variable


#Making Random Variables
xSamp <- rnorm(100,mean=100, sd=10)
meanSamp <- mean(xSamp)
###########  Graphing Stuff########################################################

#Commands that Graph
plot()       
hist()
scatterplot(weight ~ repwt, data=Davis, smooth=FALSE, id.n=1)
scatterplotMatrix #(~weight+height+repht+repwt, data=Davis) #More complicated scatterplot matrix vvvvvvv
scatterplotMatrix(~ prestige + log2(income) + education + women | type, data=Prestige, by.group=TRUE, id.n=0,smooth=FALSE, col=gray(c(0,0.5,0.7)))

?scatterplotMatrix

residualPlot #(prestige.mod)
leveragePlots #(prestige.mod)                                     
plot(fitted(prestige.mod),Prestige$prestige) #fitted makes the fitted values for the model happen                        

#Labeling
plot(post.test.3 ~ group, data=Baumann, xlab="Group",ylab="Reading Score") #Shows how to label x and y axis

#Making Lines/arrows/curves
abline(v=meanSamp, lty="dashed")
abline(a=0,b=1)

arrows(mean(sampMeans)-sem, 20, mean(sampMeans)+sem, 20, code=3)

plot(sampN,sampMeans,xlim=c(-30, 1030), ylim=c(95.5, 105.5),type='b')
upMeans <- 100 + 2*(10/sqrt(sampN)) 
lowMeans <- 100 - 2*(10/sqrt(sampN))
abline(h=100)
lines(sampN,upMeans,lty='dashed')
lines(sampN,lowMeans,lty='dashed')     #this makes a curve

plot(effect("income:education", prestige.mod.2)) # effects command used to visual section of interaction

library(splines)
prestige.mod.3 <- lm(prestige ~ ns(income, df=4), data=Prestige)
summary(prestige.mod.3)
plot(effect("ns(income, df=4)", prestige.mod.3)) #graph showing income platauing 

shapiro.test(log(Prestige$income))

prestige.mod.4a <- lm(prestige ~ education*type + log2(income)*type, data=Prestige)
summary(prestige.mod.4a)
plot(effect ("education:type", prestige.mod.4a))   #ANACOVA with interaction using the plot command and the effect command for education*type in model 4a.      

########### (Tutorial 2, Exercise 8) Plotted sample means as a function of sample size with three curves representing th confindence intervals. #######
sampN <- c(10,20,50,100,200,500,1000)
sampMeans <- array(0,7)
sampMeans[1:7] <- sapply(sampN,function(x) mean(rnorm(x,mean=100,sd=10)))

plot(sampN,sampMeans,xlim=c(-30, 1030), ylim=c(95.5, 105.5),type='b')
upMeans <- 100 + 2*(10/sqrt(sampN)) 
lowMeans <- 100 - 2*(10/sqrt(sampN))
abline(h=100)
lines(sampN,upMeans,lty='dashed')
lines(sampN,lowMeans,lty='dashed')
########### (Tutorial 2, Exercise 2) Table comparing Multivariate coefficients with Bivariate coefficients ##########
library(cars)
head (Prestige)
summary(presM)
(presM <- lm(prestige ~ education + log(income) + women + type, data = Prestige))
presM.edu <- lm(prestige ~ education, data = Prestige)
presM.inc <- lm(prestige ~ log(income), data = Prestige)
presM.sex <- lm(prestige ~ women, data = Prestige)
presM.typ <- lm(prestige ~ type, data = Prestige)
presM.cen <- lm(prestige ~ census, data = Prestige)

coef(presM) # brings up the coefficients of the model.
coef(presM)[2] # brings up the 2nd coefficient in that model.
coef(presM)[2:4] # brings up the 2nd - 4th coefficients in that model.
round(coef(presM)[2:4], 2) # a way to round to just two digits.

Full.Model = round(coef(presM)[2:4], 2) # I'll save this output to a variable.

mod.compare <- cbind(Full.Model = round(coef(presM)[2:4],2), 
                     Bivariate.Model = c(round(coef(presM.edu)[2],2), 
                                         round(coef(presM.inc)[2],2),
                                         round(coef(presM.sex)[2],2)))
mod.compare
########### (Tutorial 2, Exercise 5) Linear regression with type as a factor with verified answer ##############
summary(mod <- lm(prestige ~ type, data=Prestige))

means[1] - coef(mod)[1] ## The intercept is equivalent to the mean of blue collar jobs
means[2] - (coef(mod)[2] + coef(mod)[1]) ## The coefficient for prof is equivalent to the 
## difference between bc and prof prestige.
means[3] - (coef(mod)[3] + coef(mod)[1]) ## The coefficient for wc is the difference between
## wc and bc prestige.
########### (Tutorial 3, Exercise 2) Plotted predictions that one would have obtained ############## 
#for each group without taking the proffession type into account. 
summary(prestige.mod.1)

m.bc=coef(prestige.mod.1)[1]
m.prof= coef(prestige.mod.1)[1] + coef(prestige.mod.1)[4]
m.wc= coef (prestige.mod.1)[1] + coef(prestige.mod.1)[5]
m.type= mean(c(m.bc,m.prof, m.wc))
m.type

new.fit= prestige.mod.1$fitted.values

fit.n= length(new.fit)

for (i in 1:fit.n) {
  if (prestige.mod.1$model$type[i] == "bc") {
    new.fit[i] = new.fit[i] - (m.bc) + m.type
  }
  else if (prestige.mod.1$model$type[i] == "prof" ) {
    new.fit[i] = new.fit[i] - (m.prof) + m.type
  }
  else {
    new.fit[i] = new.fit[i] - (m.wc) + m.type
  }
}

scatterplot(prestige.mod.1$model$prestige,new.fit, groups=prestige.mod.1$model$type,smooth=FALSE)
abline(a=0,b=1)

scatterplot(prestige.mod.1$model$prestige, prestige.mod.1$fitted.values, groups=prestige.mod.1$model$type,smooth=FALSE)
abline(a=0,b=1)

# Specific Examples

########### For Loop Examples#########
for (i in 1:10) {
  x[i] <- i*3
}

sampMeans <- array(0,100)
for (i in 1:100) {
  sampMeans[i] <- mean(rnorm(100,mean=100, sd=10))
}

sampN <- c(10,20,50,100,200,500,1000)
sampMeans <- array(0,7)
for (i in 1:7) {
  sampMeans[i] <- mean(rnorm(sampN[i],mean=100, sd=10))
}

for (i in 1:fit.n) {
  if (prestige.mod.1$model$type[i] == "bc") {
    new.fit[i] = new.fit[i] - (m.bc) + m.type
  }
  else if (prestige.mod.1$model$type[i] == "prof" ) {
    new.fit[i] = new.fit[i] - (m.prof) + m.type
  }
  else {
    new.fit[i] = new.fit[i] - (m.wc) + m.type
  }
########### apply Function examples #############
  sampMeans <- rep(0,100)
  sampMeans[1:100] <- sapply( 1:100, function(x) mean(rnorm(100,mean=100,sd=10)))
  
  Newloop= c(10,20,50,100,200,500,1000)
  Newloop.array= array(Newloop)
  Newloop.array [1:7]= sapply (1:7, function (x) mean(rnorm(Newloop,mean=100, sd=10)))
  plot(Newloop.array)
  plot(Newloop, Newloop.array)
  
  means <- with(Prestige, tapply(prestige, type, mean)) #Gets the mean from all factors within type
########### cbind Examples ###############
  mod.compare <- cbind(Full.Model = round(coef(presM)[2:4],2), 
                       Bivariate.Model = c(round(coef(presM.edu)[2],2), 
                                           round(coef(presM.inc)[2],2),
                                           round(coef(presM.sex)[2],2)))
  davis.mod <- lm(weight ~ repwt, data=Davis)
  davis.mod.2 <- update(davis.mod, subset=-12)
  cbind(Original=coef(davis.mod), NoCase12=coef(davis.mod.2)) #cbind comparing two models one with outlier and one withour
########### With Command Examples###########
  with(Davis, mean(repwt, na.rm=T)) #Gets the mean from data set with NA in it.
  
  means <- with(Prestige, tapply(prestige, type, mean)) #Gets the mean from all factors within type
  
  Prestige$type <- with(Prestige, factor(type,levels=c("bc", "wc", "prof"))) #rearranges factors
  
  with(Moore, tapply(conformity,list(Authoritarianism=fcategory,"Partner's Status"=partner.status),mean))
  

#Tutorial 4: Modeling Interactions.########################################################################################

#################  model comparison #########
# Before looking at interactions, we are understanding how linear regression can be used to fit non-linear terms. 
#This is done by adding "regressors".  K "predictors"  can be manipulated to generate many more (>K) "regressors".
# We can compare the model that predicts prestige with income and education as additive predictor variables 
#with a model that also includes a multiplicative term.

prestige.mod.1 <- lm(prestige ~ education + income, data=Prestige)
prestige.mod.2 <- lm(prestige ~ income*education, data=Prestige)
confint(prestige.mod.1)
summary(prestige.mod.2)

#confidence interval: very unlikely that your model is significant if confidence contains 0
anova(prestige.mod.1, prestige.mod.2)
#################  Graphing interactions####################

library(effects)
plot(effect("income:education", prestige.mod.2))

library(splines)
prestige.mod.3 <- lm(prestige ~ ns(income, df=4), data=Prestige)

plot(effect("ns(income, df=4)", prestige.mod.3))

with(Moore, {
  interaction.plot(fcategory, partner.status, conformity, type="b",
                   pch=c(1, 16), cex=2, ylim=range(conformity), leg.bty="o")
  points(jitter(as.numeric(fcategory), factor=0.5), conformity,
         pch=ifelse(partner.status == "low", "L", "H"))
})

# Leverage plots are generalizations of Added value plots.
# In added value plots, the residuals of the pretictor variables 
# given all other variables are used to predict the residual of the predicted variable.
# advariable allows you to see the contribution of each on your model. 

prestige.mod.4a <- lm(prestige ~ education*type + log2(income)*type, data=Prestige)
summary(prestige.mod.4a)
avPlots(prestige.mod.4a)
leveragePlots( prestige.mod.4a)
residualPlots( prestige.mod.4a)
#################  Exercise 1 +2: DF and parameters, predicting means off of that table #############################

# Exercise 1. Questions: 1) How many parameters? 2) What are the df?
# 3) Using the table on p. 169. check the results of the coefficients.

mod.moore.1 <- lm(conformity ~ fcategory*partner.status, data=Moore)
summary(mod.moore.1)

summary(mod.moore.1) 
#1)There are 6 parameters. 
#2) Df1= 5, Df2= 39
length(mod.moore.1$mod$conformity)
#3)
lowlow= mod.moore.1$coefficients[1]
lowhigh= mod.moore.1$coefficients[1]+mod.moore.1$coefficients[4]
mediumlow= mod.moore.1$coefficients[1]+mod.moore.1$coefficients[2]
mediumhigh=mod.moore.1$coefficients [1]+mod.moore.1$coefficients[2]+mod.moore.1$coefficients[4]+mod.moore.1$coefficients[5]
highlow=mod.moore.1$coefficients[1]+ mod.moore.1$coefficients[3]
highhigh= mod.moore.1$coefficients[1]+mod.moore.1$coefficients[3]+ mod.moore.1$coefficients[4]+mod.moore.1$coefficients[6]

# Then without
mod.moore.2 <- update(mod.moore.1, . ~ . - fcategory:partner.status)
summary(mod.moore.2)

#  Exercise 2 Questions: 1) How many parameters? 2) What are the df?# 3) Using the table on p. 170. 
#Check to see if you can still 
#predict the mean of each cell.  Why not? 

summary(mod.moore.2)
#1)There are 4 parameters
#2)There are df1= 3 , ddf2= 41
#3)
(lowlow2= mod.moore.2$coefficients[1])
(lowhigh2=mod.moore.2$coefficients[1]+mod.moore.2$coefficients[4])
(mediumlow2=mod.moore.2$coefficients[1]+mod.moore.2$coefficients[2])
(mediumhigh2=mod.moore.2$coefficients[1]+mod.moore.2$coefficients[2]+mod.moore.2$coefficients[4])
(highlow2= mod.moore.2$coefficients[1]+mod.moore.2$coefficients[3])
(highhigh2=mod.moore.2$coefficients[1]+mod.moore.2$coefficients[3]+mod.moore.2$coefficients[4])
#################  Exercise 4 R2 and F stat equations#########################
# Exercise 3.  For the prestige.mod.4a (the ANOCOVA with interactions) 
#calculate SSerror, SStotal and from those R2 and F. 
#Hint take a look at the objects in the model
y=prestige.mod.4a$mod$prestige

(ss2= sum(prestige.mod.4a$residuals^2))

length (Prestige$prestige)

(ss1= sum((y-mean(y))^2)) 

(R2= ((ss1-ss2)/ss1))

(R2= ((ss1-ss2)/ss1))

Fstat= ((ss1-ss2)/8)/(ss2/89)

pf(Fstat, 8, 89, lower.tail = FALSE)

#This shows that the probabaility of finding values greater or
#equal to the F-Ratio is very small, so model 2 is a greater
#predictor than model 1. 
# Also check your results with the sequential anova table using 
#the anova() command.
anova(prestige.mod.4a)
Anova(prestige.mod.4a)

#Tutorial5: statitistics for linear models using the F-statistic ###########################################################

#################  R2 equations again #############
sserror <- sum(prestige.mod$residual^2)
mean.prestige <- mean(prestige.mod$model$prestige)
sstotal <- sum((prestige.mod$model$prestige - mean.prestige)^2)

(rsquare <- (sstotal-sserror)/sstotal)

k1 <- 1;
k2 <- length(prestige.mod$coefficients)
n <- length(prestige.mod$fitted.values)
(dfmodel <- k2-k1)
(dferror <- n-k2)
(fvalue <- ((sstotal-sserror)/dfmodel)/(sserror/dferror))

# This will return the probability
pf(fvalue, dfmodel, dferror, lower.tail = FALSE)
#################  Exercise 1+ 2: F-value ###################

prestige.mod.1 <- lm(prestige ~ education + log2(income) + type,
                     data=na.omit(Prestige)) # full model
prestige.mod.0 <- update(prestige.mod.1, . ~ 1) # intercept only
anova(prestige.mod.0, prestige.mod.1) # compare models

# This F value is the same as the overall F test that you get from the summary of the complete model
summary(prestige.mod.1)

# One can also test the effect of one additional parameter
prestige.mod.0inc <- update(prestige.mod.1, . ~ . - log2(income))
anova(prestige.mod.0inc, prestige.mod.1) # compare models


# Exercise 2.  What does this F value correspond 2?  Give two other ways of obtaining it.
#You can get this F value by the Anova() function and looking at the entry for log2(income) and 
#it is also the square of the t value in the Wald Test.
Anova(prestige.mod.1)
(prestige.mod.1.sum <- summary(prestige.mod.1))

(prestige.mod.1.sum$coefficients[3,3])^2

# More generally you can use the anova command to compare two embedded models
prestige.mod.1 <- lm(prestige ~ education + log2(income) + type,
                     data=na.omit(Prestige)) # full model
prestige.mod.ed  <- lm(prestige ~ education, data=na.omit(Prestige))

anova(prestige.mod.1, prestige.mod.ed)

#Tutorial 6: Resampling (Bootstrap) and Cross-validation. ##################################################################

#################  Tables and Graphs###########################

ftable(x1 <- xtabs(~ discipline + rank + sex, data=Salaries))
round(100*ftable(prop.table(x1, margin=c(1, 2))), 1) # % m and f

library(lattice)
xyplot(salary ~ yrs.since.phd | discipline:rank, group=sex,
       data=Salaries, type=c("g", "p", "r"), auto.key=TRUE)

bwplot(salary ~ discipline:sex | rank, data=Salaries,
       scales=list(rot=90), layout=c(3, 1))
#################  Bootstrapping ############################# 

# Generate a model for Males only
fselector <- Salaries$sex == "Female" # TRUE for females
salmod <- lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
             subset=!fselector) # regression for males
female.salmod= lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
                  subset=fselector) #
# predictions for females: If include the men  we are overpredicting for women. 
femalePreds <- predict(salmod, newdata=Salaries[fselector, ])
(meanDiff <- mean(Salaries$salary[fselector] - femalePreds))
(mean(Salaries$salary))

malePreds <- predict(female.salmod, newdata=Salaries[-fselector, ])
(men.meanDiff <- mean(Salaries$salary[-fselector] - malePreds))


# Let's do a bootstrap to see how often we could get this result
set.seed(8141976) # for reproducibility
fnumber <- sum(fselector) # number of females
n <- length(fselector) # number of observations
B <- 999 # number of replications
simDiff <- numeric(B) # initialize vector with B entries

for (j in 1:B){
  sel <- sample(1:n, fnumber) # random sample of nominated 'females'
  m2 <- update(salmod, subset=-sel) # refit regression model
  simDiff[j] <- mean(Salaries$salary[sel] - predict(m2, newdata=Salaries[sel, ])) # compute mean diff.
}

# Calculate the p-value    
(frac <- round(sum(meanDiff > simDiff)/(1 + B), 3))

# Plot the histogram
hist(simDiff, main=paste("Histogram of Simulated Mean Differences\np-value =",frac),
     xlab="Dollars")
abline(v=meanDiff, lty="dashed")
#################  predict timbre and keep track of R-square values as a function of the number of parameters##############



head(Prestige)
nrow(Prestige)
blah= nrow(Prestige)
sample( blah, 30)


setwd("~/Timbre")
Timbre <- read.table('TimbreData.txt')
(n.inst <- nrow(Timbre))
head(Timbre)

Rvals <- numeric(40)
Rvals.adj  <- numeric(40)
timbre.mod <- lm(timbre ~ sound.1, data=Timbre)
(timbre.sum <- summary(timbre.mod))
Rvals[1] <- timbre.sum$r.squared
sserror <- sum(timbre.mod$residual^2)
mean.timbre <- mean(timbre.mod$model$timbre)
sstotal <- sum((timbre.mod$model$timbre - mean.timbre)^2)
dferror <- timbre.mod$df.residual  # n - k -1
dftotal  <- length(timbre.mod$fitted.values)-1 # n - 1
Rvals.adj[1]  <- 1- ((sserror/dferror)/(sstotal/dftotal))

i <- 2
timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", i))
timbre.sum <- summary(timbre.mod)
Rvals[i] <- timbre.sum$r.squared
sserror <- sum(timbre.mod$residual^2)
dferror <- timbre.mod$df.residual  # n - k -1
Rvals.adj[i]  <- 1- ((sserror/dferror)/(sstotal/dftotal))
#################  Exercise 2: plot the R2 as a function of the number of sound parameters ###############
for (j in 1:40){
  timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", j))
  timbre.sum <- summary(timbre.mod)
  Rvals[j] <- timbre.sum$r.squared
  sserror <- sum(timbre.mod$residual^2)
  dferror <- timbre.mod$df.residual  # n - k -1
  Rvals.adj[j]  <- 1- ((sserror/dferror)/(sstotal/dftotal))
}
plot(Rvals)
lines(Rvals.adj, col="red")
#################  Exercise 3: Calculate by hand the R-square #####################

timbre.mod.2= lm(timbre~ sound.1+ sound.2+ sound.3, data=Timbre)
summary(timbre.mod.2)

t.sserror <- sum(timbre.mod.2$residual^2)
mean.t <- mean(timbre.mod.2$model$timbre)
t.sstotal <- sum((timbre.mod.2$model$timbre - mean.t)^2)

(rsquare.t <- (t.sstotal-t.sserror)/t.sstotal)
#################  Exercise 4:  Cross val#########################
#divide the sound instruments 
#into a fitting and validation data set, use the model fitted on the 
#fitting data set to get predictions for the validation data set and 
#calculate the R-square for that validation data.  Do this for models 
#of increasing dimension (from 1 to 40).  Plot this validation, R-square on the same graph.
# Suggestion: use 10 data ponts for a validation data set but note that you will then have n-10 rows in your data set for fitting model parameters.
?sample
parameters = ncol(Timbre) - 1
Rvals.Fit <- numeric(parameters)
Rval.Val= numeric(parameters)
fit_index = sample(n.inst, n.inst-10)
fitted = Timbre[fit_index,]
validated = Timbre[-fit_index,]
for (j in 1:40){
  if (j == 1) {
    timbre.mod.Fit<- lm(timbre ~ sound.1, data=fitted)
  } else {
    timbre.mod.Fit <- update(timbre.mod.Fit, sprintf(". ~ . + sound.%d", j))
  }
  timbre.fit.sum = summary(timbre.mod.Fit)
  ss2 <- sum((validated$timbre - predict(timbre.mod.Fit, newdata=validated))^2)
  ss1 <- sum((validated$timbre- mean(validated$timbre))^2)
  Rvals.Fit[j] <-  timbre.fit.sum$r.squared
  Rval.Val[j] = (ss1-ss2)/ss1
}

param = 1:40
plot(param, Rvals.Fit, type='l')
lines(param, Rval.Val, type="l", col="blue")

#Tutorial 7: Logistic Regression ############################################################################################

#################  Exercise 1: replicating function graphs  ##################
#Try to make figure 5.1 in R.  Notice that figure 5.1 are inverse link functions or mean kernel functions.  
#Hint: you will have to use the help menu to find functions to calculate the probit
#-generate an x vector. maybe sequence command, going to create an x/y plot. going from -3 to 3. then plot x/y.

x=seq(-5,5, 0.1)
logit= 1/(1+exp(-x))
cloglog= 1-exp(-exp(x))
probit= pnorm(x/sqrt(3))  
plot(x,logit, type="l")
lines(x,cloglog, lty= 2)
lines(x,probit, lty= 3)
legend( x="topleft", lty=c(1,2,3), c("logit", "cloglog", "probit"))
#################  Exercise 2: Glm command, exp conversion, and explaiantion  ##################
# We are now fitting the generalized linear model.  The logit link is the default link function
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
                family=binomial(link=logit), data=Mroz)
summary(mroz.mod)
plot(mroz.mod)

# We are calculating the exponents of the coefficients 
round(exp(cbind(Estimate=coef(mroz.mod), confint(mroz.mod))), 2)

# Exercise 2.  What is the effect of having one additional child who is less than 6 years old?

#it is going to decrease the odds of working by 1-0.23 = 77%
#################  Comparing two glm models ###################
# We are now going to compare two models - with and without the participation of children
mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
anova(mroz.mod.2, mroz.mod, test="Chisq")

# The Anova command in car performs a type II test
Anova(mroz.mod)

# As in the case of lm() we can also look at the predictions.
head(predict(mroz.mod)) # first 6 values
head(predict(mroz.mod, type="response"))
#################  Exercise 3:  setting a threshold at 50% calculate the number of correct predictions and Plot ###################
nrow(Mroz)
head(Mroz)
objects(mroz.mod)
mroz.mod$fitted.values
(x= (predict (mroz.mod, type="response") >=0.5)) #always do response
sum(x)
threshold= subset (x, subset = x>0.50)
length(threshold) #these are the number of predictions that will work

y=(Mroz$lfp == "yes")
sum(y)
z= y==x # the == says, if both of these are true, then count them.
sum(z)


# Now we make a nice plot to explain logistic regression
x11()
sel = sample(length(mroz.mod$linear.predictors), 200);
scatterplot(mroz.mod$linear.predictors[sel],jitter(mroz.mod$fitted.values[sel],factor=1000),group=z[sel],smooth=FALSE, reg.line=FALSE, cex=0.5)

points(mroz.mod$linear.predictors[sel], rep(.5683931,200), cex=0.5)

#Tutorial 8: GLM statistical ignificance#####################################################################################

#################  Fitting and comparing GLM models  #########################33
# We are now fitting the generalized linear model.  The logit link is the default link function
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
                family=binomial(link=logit), data=Mroz)
summary(mroz.mod)
mroz.mod.5 <- glm(lfp ~age,
                  family=binomial(link=logit), data=Mroz)
summary(mroz.mod.5)

round(exp(cbind(Estimate=coef(mroz.mod), confint(mroz.mod))), 2)
round(exp(cbind(Estimate=coef(mroz.mod.5), confint(mroz.mod.5))), 2)
#base line is equal to 1

# We are now going to compare two models - with and without the participation of children
mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)

anova(mroz.mod.2, mroz.mod, test="Chisq")

anova(mroz.mod.2, mroz.mod)

# The Anova command in car performs a type II test
Anova(mroz.mod)

names(mroz.mod$model)

#################  Exercise 1:  Calculating residual deviance with equation (with forloop)#####################

# Exercise 1. Calculate the deviance for the full mroz model (similar to R2 error). 
#Note that the log likelyhood of the saturated model is equal to zero.  
#In my solution, I used: the data yes variable, a for loop to add the contribution of 
#each entry and a if statement to look at yes vs no. 

array.yes= array(0,428)
array.no= array(0,325)
for (i in 1:428) {
  if (mroz.mod$model$lfp[i] == "yes") {
    array.yes[i]= (log(mroz.mod$fitted.values[i]))
  } 
}

for (i in 429:753){
  if (mroz.mod$model$lfp[i] =="no") {
    array.no[i]= (log(1-(mroz.mod$fitted.values[i])))
  }
}

sum(array.no[429:753])
LLm= sum(array.yes)+ sum(array.no[429:753])

(Deviance = LLm*-2)
#################  Exercise 2:  Calculating NULL deviance with equation (with forloop)##############

# Exercise 2. Now calculate the Null deviance.  
#The null deviance is the deviance obtained from the mean response. (similar to R2 total). 
#Compare your values to those obtained in the summary.


mroz.mod.null <- update(mroz.mod, . ~ . - k5 - k618 - age - wc - hc - lwg - inc +1)
array.yes.null= array(0,428)
array.no.null= array(0,325)
for (i in 1:428) {
  if (mroz.mod.null$model$lfp[i] == "yes") {
    array.yes.null[i]= (log(mroz.mod.null$fitted.values[i]))
  } 
}

for (i in 429:753){
  if (mroz.mod.null$model$lfp[i] =="no") {
    array.no.null[i]= (log(1-(mroz.mod.null$fitted.values[i])))
  }
}

sum(array.no.null[429:753])
LLm.null= sum(array.yes.null)+ sum(array.no.null[429:753])

(Deviance = LLm.null*-2)
#################  Exercise 3:  Calculate the change in deviance ##################
# Exercise 3. Now calculate the change in deviance of a full model, with and without age.  
#Obtain a p value from the chisquare  distribution. 
#Perform the Type II test to check your result (you can do this both with the anova and the 
#Anova function - do it both ways so that you understand it well).

mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,family=binomial(link=logit), data=Mroz)
mroz.mod.3= mroz.mod.2 <- update(mroz.mod, . ~ . - age) 
summary(mroz.mod)
summary(mroz.mod.3)
age.anova=anova(mroz.mod.3, mroz.mod, test ="Chisq")
Pvalue= age.anova[2,5]
age.Anova=Anova(mroz.mod)
Pvalue.2= age.Anova[3,3]
(Pvalue==Pvalue.2) #It's true
#################  Exercise 4.  Comparing Deviance to sum of square Errors##################

#linear models can also be called using glm.  The two following models are equivalent, but give different summaries:

prestige.mod.lm <- lm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.lm)

prestige.mod.glm <- glm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.glm)

# Compare the deviance in the glm model to the sum of square errors from the lm model?  
#Is the residual Deviance given by R unitless (normalized) or does it have units?

(sserror <- sum(prestige.mod.lm$residual^2))

# This ss errors is equal to the residual deviance in the model.  
#R uses a definition of deviance that has units.  This deviance 
#is only equal to the difference in log likelihoods for distributions 
#where the dispersion parameter is zero.  To get a unit-less deviance 
#(defined as the difference in log likelihoods), you would have to divide by the dispersion parameter. 

#Tutorial 9: LME versus LM on Math achievement ############

################ Organizing Math data, finding means, making plots ################

library(nlme)
library(lattice)

# All the studetns
data(MathAchieve)
# Look at first 10 rows
MathAchieve[1:10,]

#Now organized by school
data(MathAchSchool)
MathAchSchool[1:10,]

# Recalculate the mean of SES for each school
attach(MathAchieve)
mses <- tapply(SES, School, mean) 
mses[as.character(MathAchSchool$School[1:10])]
detach(MathAchieve)

# Now we are making a new data frame that has all the data.
Bryk <- as.data.frame(MathAchieve[, c("School", "SES", "MathAch")])
names(Bryk) <- c("school", "ses", "mathach")
sample20 <- sort(sample(7185, 20)) # 20 randomly sampled students
Bryk[sample20,] 

# Now we add the data that is the same for all schools
Bryk$meanses <- mses[as.character(Bryk$school)]
Bryk$cses <- Bryk$ses - Bryk$meanses
sector <- MathAchSchool$Sector
names(sector) <- row.names(MathAchSchool)
Bryk$sector <- sector[as.character(Bryk$school)]
Bryk[sample20,]

# Now we are going to look at the data
cat <- sample(unique(Bryk$school[Bryk$sector=='Catholic']), 20)
Cat.20 <- groupedData(mathach ~ ses | school, data=Bryk[is.element(Bryk$school, cat),])
pub <- sample(unique(Bryk$school[Bryk$sector=='Public']), 20)
Pub.20 <- groupedData(mathach ~ ses | school, data=Bryk[is.element(Bryk$school, pub),])


trellis.device(color=F)
xyplot(mathach ~ ses | school, data=Cat.20, main="Catholic",
       panel=function(x, y)	{
         panel.xyplot(x, y)
         panel.loess(x, y, span=1)
         panel.lmline(x, y, lty=2)
       }
)

xyplot(mathach ~ ses | school, data=Pub.20, main="Public",
       panel=function(x, y)	{
         panel.xyplot(x, y)
         panel.loess(x, y, span=1)
         panel.lmline(x, y, lty=2)
       }
)
################ Using LMList command to find SES coefficients by school, then plot #######

# lm list generates a list of linear models - in this case for each school
cat.list <- lmList(mathach ~ ses | school, subset = sector=='Catholic', data=Bryk)

pub.list <- lmList(mathach ~ ses | school, subset = sector=='Public', data=Bryk)

# Plots the intervals for the coefficients

plot(intervals(cat.list), main='Catholic')

plot(intervals(pub.list), main='Public')

# Make a box plot of coefficients.
# First store the coefficients in a new array
cat.coef <- coef(cat.list)
cat.coef[1:10,]
pub.coef <- coef(pub.list)
pub.coef[1:10,]
# Second, make the box plot
# dev.new()
old <- par(mfrow=c(1,2))
boxplot(cat.coef[,1], pub.coef[,1], main='Intercepts',
        names=c('Catholic', 'Public'))
boxplot(cat.coef[,2], pub.coef[,2], main='Slopes',
        names=c('Catholic', 'Public'))
par(old)
################ Running LME on Math data with cses as random effect ############

# Now that we understand the data we can fit a hierarchical linear model
bryk.lme.1 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ cses | school, data=Bryk)
summary(bryk.lme.1)

# Did the inclusion of the random effects worked? 
# Yes
# Let's compare to models without a different intercept for each school and without a different slope.
bryk.lme.2 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ cses | school, data=Bryk)

# First the model without the different slope
bryk.lme.2 <- update(bryk.lme.1, random = ~ 1 | school) 
anova(bryk.lme.1, bryk.lme.2)

# Next the model without the different intercept
bryk.lme.3 <- update(bryk.lme.1, random = ~ cses - 1 | school) 
anova(bryk.lme.1, bryk.lme.3)
################ Exercise 1: Compare LME to LM #################

# Exercise.  Compare to the lm model.

bryk.lm.mod=lm(mathach ~ meanses*cses + sector*cses, data=Bryk)
summary(bryk.lm.mod)

anova(bryk.lme.1, bryk.lm.mod)

#The lme model shows a significant decrease in AIC, BIC, and logLik compared to the lm model, which shows
#it to be a better model. 

#Tutorial 10: Within subjects ANOVA using the mixed linear model ############

################ Intro / upload ############

# First get the data.  Change the following line to fit your folder structure
setwd('/Users/ddbourgin/Save/advstatsfa2014/week10/')
load('ALLSUBJSphase2.Rdata')

# The data frame: data.ex5 has results on reaction times in a musical stroop test
head(data.ex5)

# We are going to take a look at the data per subject
library(lattice)
xyplot(TTime~Congruence:BinRefTone | Subj, data.ex5, groups=SubjType)
################ Exercise 1: LME model versus LM model part 1 ###############

# Exercise 1. Test the mixed linear model with random intercept for reaction
# time and compare the coefficients and the results to the one obtained without
# including the subject variability.

library(nlme)
miren.lme.1 <- lme( TTime ~ Congruence*BinRefTone*SubjType, random = ~ 1 | Subj, data=data.ex5)
summary(miren.lme.1)

miren.lm.1 <- lm( TTime ~ Congruence*BinRefTone*SubjType, data=data.ex5)
summary(miren.lm.1)

# In this case the within subject analysis allowed us to avoid a false positive!
# The linear model finds an effect between subjects that have AP vs RP but the
# mixed model tells us that that variability can be explained by within subject
# random effects.
################ Exercise 2: LME model versus LM model part 2 (with Log) ###############


# Exercise 2. Repeat the analysis with the log of the reaction time.
data.ex5$logRT <- log(data.ex5$TTime)

miren.lme.2 <- lme( logRT ~ Congruence*BinRefTone*SubjType, random = ~ 1 | Subj, data=data.ex5)
summary(miren.lme.2)

miren.lm.2 <- lm( logRT ~ Congruence*BinRefTone*SubjType, data=data.ex5)
summary(miren.lm.2)

# The log increased the t-values of the signficant values.

#Tutorial 11: PCA, ICA, and LDA ############

################ Create data for use on PCA and ICA###########
# In this example there are four light recorders and two light sources
recorders <- data.frame("X"=c(0,0,1,1), "Y" = c(0,1,1,0), row.names=c("A", "B","C","D"))
locs <- data.frame("X"=c(.3,.5),"Y"=c(.8,.2))
intensities <- data.frame("sine"=sin(0:99*(pi/10))+1.2,
                          "cosine"= .7*cos(0:99*(pi/15))+.9)

# Now, we are going to generate "recorded" data
dists <- matrix(nrow=dim(locs)[1], ncol=dim(recorders)[1],
                dimnames <- list(NULL, row.names(recorders)))
for (i in 1:dim(dists)[2]) {
  dists[,i]=sqrt((locs$X-recorders$X[i])^2 + (locs$Y-recorders$Y[i])^2)
}
recorded.,data <- data.frame(as.matrix(intensities)%*%
                              matrix(data=exp(-2*as.numeric(dists)), nrow=dim(locs)[1], ncol=dim(recorders)[1]))

recorded.data <- as.data.frame(apply(recorded.data,2,FUN=function(x)
{sdx=sd(x, na.rm=TRUE);
 noise=rnorm(length(x),0,sdx/10);
 return(x+noise)}
))

# Let's take a look at it.

plot(recorded.data)
round(cor(recorded.data),2)

# We can also plot is as a time-series
plot.ts(recorded.data)

################ Using the PCA #################3

# We can also calculate the principal componnents using R functions
pr=prcomp(recorded.data)
pr

# This plots the variance explained by each pc
plot(pr)  

# Let's plot the normalized standard deviation explained by each PC
barplot(pr$sdev/pr$sdev[1])

# You can also set a tolerance value to select PCA that explain a certain fraction of the variance.
pr2=prcomp(recorded.data, tol=.1)
plot.ts(pr2$x)  # x are the coefficients of the pca
plot.ts(intensities)    #how do they compare to the int?
plot.ts(recorded.data)  #how do they compare to the recorded data?

# We can also obtain reconstructions of the data using only some of the pca

# Here is a reconstruction using all PCs
od=pr$x %*% t(pr$rotation)

# Here is the reconstruction using the first two PCs
od2=pr2$x %*% t(pr2$rotation)

# Let's plot the reconstructions on different graphs
plot.ts(od)
plot.ts(od2)
################ Using ICA ################

# Let's try ICA
library(fastICA)
a <- fastICA(recorded.data, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)

plot.ts(a$S)
################ Using an LDA ##############

# We are now also going to look at linear discriminant analysis or MANOVA 
# The R function is called lda() and it is found in the MASS package
library(MASS)

# the iris data has information on the morphology of three types of irises.. Can you tell the species apart from this morphology?

Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))
train <- sample(1:150, 75)
table(Iris$Sp[train])

# Let's perform a discriminant function analysis
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
z

# we can plot the results
plot(z)

# And get predictions for the data that we did not use
predict(z, Iris[-train, ])$class
Iris$Sp[-train]

# Exercise
# Look at the biopsy data which has morphological data on tumors.  THere are 9 measurements
# V1 clump thickness.
# V2 uniformity of cell size.
# V3 uniformity of cell shape.
# V4 marginal adhesion.
# V5 single epithelial cell size.
# V6 bare nuclei (16 values are missing).
# V7 bland chromatin.
# V8 normal nucleoli.
# V9 mitoses

head(biopsy)
################ LDA for Data with 80%  data for traing #############

# Perform a MANOVA (same as lda) on this data. Use 80% of the data for training. 
#How many discriminant functions do you find?  
#How well can you predict the class for the testing data? 
#(give a percent of correct detection and of false positives)
n <- nrow(biopsy)
nsel <- round(.8*n)
train <- sample(1:n, nsel)

biopsy.lda <- lda(biopsy[,2:10], biopsy[,11], subset = train, na.action = na.omit)
(biopsy.predict <- predict(biopsy.lda, biopsy[-train,2:10])$class)
biopsy.actual <- biopsy[-train,11]

(detection <- (biopsy.actual == 'malignant') * (biopsy.predict == 'malignant'))
sel <- !is.na(detection)

(p.detection <- sum(detection[sel])/sum(biopsy.actual[sel] == 'malignant'))

(fpositive <- (biopsy.actual == 'benign') * (biopsy.predict == 'malignant'))
sel <- !is.na(fpositive)

(p.fpositive <- sum(fpositive[sel])/sum(biopsy.actual[sel] == 'benign'))

# Use the ldahist function to obtain a histogram of the first dimension
ldahist(biopsy[,2:10],biopsy[,11])

# "By Hand" Calculations #####
############ Calculating R2 and F Stat #############
###### Turtorial 4:
y=prestige.mod.4a$mod$prestige

(ss2= sum(prestige.mod.4a$residuals^2))

(ss1= sum((y-mean(y))^2)) 

(R2= ((ss1-ss2)/ss1))

(R2= ((ss1-ss2)/ss1))

Fstat= ((ss1-ss2)/8)/(ss2/89)

pf(Fstat, 8, 89, lower.tail = FALSE)

###### Tutorial 5:
sserror <- sum(prestige.mod$residual^2)
mean.prestige <- mean(prestige.mod$model$prestige)
sstotal <- sum((prestige.mod$model$prestige - mean.prestige)^2)

(rsquare <- (sstotal-sserror)/sstotal)

k1 <- 1;
k2 <- length(prestige.mod$coefficients)
n <- length(prestige.mod$fitted.values)
(dfmodel <- k2-k1)
(dferror <- n-k2)
(fvalue <- ((sstotal-sserror)/dfmodel)/(sserror/dferror))

# This will return the probability
pf(fvalue, dfmodel, dferror, lower.tail = FALSE)

###### Tutorial 6: (Just R2)
timbre.mod.2= lm(timbre~ sound.1+ sound.2+ sound.3, data=Timbre)
summary(timbre.mod.2)

t.sserror <- sum(timbre.mod.2$residual^2)
mean.t <- mean(timbre.mod.2$model$timbre)
t.sstotal <- sum((timbre.mod.2$model$timbre - mean.t)^2)

(rsquare.t <- (t.sstotal-t.sserror)/t.sstotal)
############ Deviance ############
###### Tutorial 7: 

array.yes= array(0,428)
array.no= array(0,325)
for (i in 1:428) {
  if (mroz.mod$model$lfp[i] == "yes") {
    array.yes[i]= (log(mroz.mod$fitted.values[i]))
  } 
}

for (i in 429:753){
  if (mroz.mod$model$lfp[i] =="no") {
    array.no[i]= (log(1-(mroz.mod$fitted.values[i])))
  }
}

sum(array.no[429:753])
LLm= sum(array.yes)+ sum(array.no[429:753])

(Deviance = LLm*-2)

###### Tutorial 7: (Null Deviance)


#The null deviance is the deviance obtained from the mean response. (similar to R2 total). 
#Compare your values to those obtained in the summary.


mroz.mod.null <- update(mroz.mod, . ~ . - k5 - k618 - age - wc - hc - lwg - inc +1)
array.yes.null= array(0,428)
array.no.null= array(0,325)
for (i in 1:428) {
  if (mroz.mod.null$model$lfp[i] == "yes") {
    array.yes.null[i]= (log(mroz.mod.null$fitted.values[i]))
  } 
}

for (i in 429:753){
  if (mroz.mod.null$model$lfp[i] =="no") {
    array.no.null[i]= (log(1-(mroz.mod.null$fitted.values[i])))
  }
}

sum(array.no.null[429:753])
LLm.null= sum(array.yes.null)+ sum(array.no.null[429:753])

(Deviance = LLm.null*-2)

###### Deviance compared to sum of squares


prestige.mod.lm <- lm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.lm)

prestige.mod.glm <- glm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.glm)

# Compare the deviance in the glm model to the sum of square errors from the lm model?  
#Is the residual Deviance given by R unitless (normalized) or does it have units?

(sserror <- sum(prestige.mod.lm$residual^2))

# This ss errors is equal to the residual deviance in the model.  
#R uses a definition of deviance that has units.  This deviance 
#is only equal to the difference in log likelihoods for distributions 
#where the dispersion parameter is zero.  To get a unit-less deviance 
#(defined as the difference in log likelihoods), you would have to divide by the dispersion parameter. 
############ Bootstrap 1 (Kelly's Lovely Example) #######
female = sample(9,250,replace=TRUE)
f.factor = factor(rep("F", length(female)))
male = sample(10,150,replace=TRUE)
m.factor = factor(rep("M", length(male)))

f = data.frame(data=female, gender=f.factor)
m = data.frame(data=male, gender=m.factor)

x = merge(f,m, all=TRUE, sort=FALSE)
# happiness scores! for males and females

n = nrow(x) # total sample size
nF = sum(x$gender == "F") # sample size for females
nM = n - nF # sample size for males

mean.F = mean(x$data[x$gender == "F"]) # mean for females
mean.M = mean(x$data[x$gender == "M"]) # mean for males
mean.diff = mean.F - mean.M # difference between the female and male mean (actual)

## this is actually doing the bootstrapping the data with the for loop
bootstrap_times = 1000 # how many times you want to bootstrap the data
random.diff = rep(0,bootstrap_times) # initiazling the variable to be stored in
for (i in 1:bootstrap_times) {
  x.gF = sample(x$data,nF) 
  x.gM = sample(x$data,nM)
  random.diff[i] = mean(x.gF) - mean(x.gM)
}

hist(random.diff) # visualizing the boostrapped differences
abline(v=mean.diff, lty=2, col="red") # this would be the visual interpretation for a 
# one-tail test
p_one_tail = (sum(mean.diff >= random.diff))/ bootstrap_times # one-tailed p value

abline(v=abs(mean.diff), lty=2, col="blue") # this would be the visual interpretation of
# a two tailed test
p_two_tail = (sum(abs(mean.diff) <= abs(random.diff)))/ bootstrap_times
# how many times random value that were that as extreme as the real data, divded by 1000
# (bootstrap_times) to obtain the percentage, aka p value
############ Bootstrap 2 (Tutorial 6) #######
# Generate a model for Males only
fselector <- Salaries$sex == "Female" # TRUE for females
salmod <- lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
             subset=!fselector) # regression for males
female.salmod= lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
                  subset=fselector) #
# predictions for females: If include the men  we are overpredicting for women. 
femalePreds <- predict(salmod, newdata=Salaries[fselector, ])
(meanDiff <- mean(Salaries$salary[fselector] - femalePreds))
(mean(Salaries$salary))

malePreds <- predict(female.salmod, newdata=Salaries[-fselector, ])
(men.meanDiff <- mean(Salaries$salary[-fselector] - malePreds))


# Let's do a bootstrap to see how often we could get this result
set.seed(8141976) # for reproducibility
fnumber <- sum(fselector) # number of females
n <- length(fselector) # number of observations
B <- 999 # number of replications
simDiff <- numeric(B) # initialize vector with B entries

for (j in 1:B){
  sel <- sample(1:n, fnumber) # random sample of nominated 'females'
  m2 <- update(salmod, subset=-sel) # refit regression model
  simDiff[j] <- mean(Salaries$salary[sel] - predict(m2, newdata=Salaries[sel, ])) # compute mean diff.
}

# Calculate the p-value    
(frac <- round(sum(meanDiff > simDiff)/(1 + B), 3))

# Plot the histogram
hist(simDiff, main=paste("Histogram of Simulated Mean Differences\np-value =",frac),
     xlab="Dollars")
abline(v=meanDiff, lty="dashed")
############ Cross Validation (Tutorial 6) #############

?sample
parameters = ncol(Timbre) - 1
Rvals.Fit <- numeric(parameters)
Rval.Val= numeric(parameters)
fit_index = sample(n.inst, n.inst-10)
fitted = Timbre[fit_index,]
validated = Timbre[-fit_index,]
for (j in 1:40){
  if (j == 1) {
    timbre.mod.Fit<- lm(timbre ~ sound.1, data=fitted)
  } else {
    timbre.mod.Fit <- update(timbre.mod.Fit, sprintf(". ~ . + sound.%d", j))
  }
  timbre.fit.sum = summary(timbre.mod.Fit)
  ss2 <- sum((validated$timbre - predict(timbre.mod.Fit, newdata=validated))^2)
  ss1 <- sum((validated$timbre- mean(validated$timbre))^2)
  Rvals.Fit[j] <-  timbre.fit.sum$r.squared
  Rval.Val[j] = (ss1-ss2)/ss1
}

param = 1:40
plot(param, Rvals.Fit, type='l')
lines(param, Rval.Val, type="l", col="blue")
############ Bootstrap / Cross Val on Practice Final for LDA) ############
nsets  <-  10
pcorrect  <- array(0, dim=nsets)
n  <- nrow(wine)
nvalid  <- floor(n*0.1)

for (i in 1:nsets) {
  valid <- sample(n, nvalid)
  wine.mod.cv <- lda(Cultivar ~ ., data=wine, prior = c(1,1,1)/3, subset = -valid)
  predict.Cultivar  <- predict(wine.mod.cv, wine[valid, ])$class
  pcorrect[i]  <- sum(predict.Cultivar == wine$Cultivar[valid])/nvalid 
}

pcorrect.avg  <- mean(pcorrect)


# Midterm 1 Solutions####

# We are going to use the car package
library(car)

# For this exam we will look at a new data set - the 'Vocab' data on vocabulary,
# education and sex. 

# Variables in the Vocab dataset:
#     vocab:     Vocabulary test score (0 - 10)
#     education: Years of education
#     sex:       Female or Male
#     year:      Year of the survey

# Let's look at some of the data:
some(Vocab)

# Question 1 (1 pt).  What is the sample size for the ENTIRE dataset?

n <- nrow(Vocab)

# n is 21638


# Because the sample size is large, if you plot all the data points at once, the
# figure will be too busy
scatterplot(vocabulary ~ education | sex, data=Vocab)

# Instead, we are going to:
#    a. Restrict ourselves to a subset of the entire dataset
#    b. Jitter (add random noise to) the datapoints in this suset
#    c. Fit regression lines to the new datapoints

# We perform steps a and b for you below. Run the following 2 lines to generate a
# scatterplot displaying the jittered datapoints:
select = sample(nrow(Vocab),200)
scatterplot(jitter(vocabulary, factor=2) ~ jitter(education, factor=2) | sex, cex=0.5, data=Vocab, subset=select, smooth=FALSE, reg.line=FALSE)




# Question 2 (1 pt). Using the output of lm command, calculate the mean vocabulary score 
# for male and females: store this values in new variables.

vocab.mod.1  <- lm(vocabulary ~ sex, data=Vocab)
summary(vocab.mod.1)
(vocab.mean.Female  <- vocab.mod.1$coefficients[1])
(vocab.mean.Male  <- vocab.mean.Female + vocab.mod.1$coefficients[2])

# Question 3 (1 pt).  Verify your answer for the male and female means using the
# tapply command.

with(Vocab, tapply(vocabulary,sex,mean))

# Question 4 (1 pt).  Using the abline command, plot lines corresponding to the
# male and female means on the scatterplot we generated earlier (Use your answers to Q2). 
# Use a different color for each line to match the legend of the graph (Use the col= argument to
# specify the color - see R's help)

abline(a=vocab.mean.Female, b=0, col='black')
abline(a=vocab.mean.Male, b=0, col='red')


# Question 5a (0.5 pts).  Use the lm command to test for the effect of sex and
# education on test score (be sure to include interactions!). Record the summary
# results from this new model. 

vocab.mod.2  <- lm(vocabulary ~ sex*education, data=Vocab)
summary(vocab.mod.2)


# Question 5b (0.5 pts). Compare this new model to the model from Question 2,
# focusing your attention on the main effect of sex. Describe in words what your
# results show: No R code here: just type in a comment.

# I notice that the main effect of sex is not significant once education is taken into account.



# # Question 6 (2 pts). Run the following command to make a new scatterplot:
scatterplot(jitter(vocabulary, factor=2) ~ jitter(education, factor=2) | sex, cex=0.5, data=Vocab, subset=select, smooth=FALSE, reg.line=FALSE)
# Now, using abline, plot the two lines corresponding to the predicted 
# vocabulary scores as a function of education for males vs. females. Use the
# predictions generated by the model in question 5a.

a.Female = vocab.mod.2$coefficients[1]
b.Female = vocab.mod.2$coefficients[3]
a.Male = a.Female + vocab.mod.2$coefficients[2]
b.Male = b.Female + vocab.mod.2$coefficients[4]

abline(a=a.Female, b=b.Female, col='black')
abline(a=a.Male, b=b.Male, col='red')


# Question 7 (3 pts).  Use a for loop to generate 1000 estimates for the male 
# population mean vocab score. To generate an estimate, use the "sample" command
# to samples 81 values from the male vocab scores (no replacement), and take
# their mean.Use your 1000 estimates to calculate the standard error of the
# sample mean (SEM). Compare this estimate to the SEM calculated using the
# standard deviation of our entire sample.

# Get started by making a new variable that contais the vocab scores for males
# only.

vocab.Male  <- subset(Vocab$vocabulary, Vocab$sex=='Male')
n.Males  <- length(vocab.Male)
n.Means  <- 1000
n.Samp  <- 81
samp.Means  <- array(0,n.Means)

for (i in 1:n.Means) {
  samp.Means[i] <- mean(vocab.Male[sample(n.Males,n.Samp)])
}

sd.Means  <- sd(samp.Means)

# to compare to the standard error of the full sample
(sd.Means.Full  <-  sd.Means*sqrt(n.Samp)/sqrt(n.Males))

# And directly from the complete data
se <- function(x) sqrt(var(x)/length(x))
with(Vocab, tapply(vocabulary,sex,se))

# I got 0.02262 which is very close to the 0.0227 estimate obtained from all the samples

# Midterm 2 Solutions #####
# EXERCISE 1
# We will be working with the Guyer dataset. This dataset contains the number of
# cooperative choices (out of 120) made by Men vs Women under Public vs
# Anomymous conditions.
?Guyer  # For additional information on the Guyer dataset
Guyer  # Print dataframe to the console

# Exercise 1a. (1 point) Perform a two-way anova (with interaction) to evaluate 
# whether condition and/or sex significantly affects the number of cooperative
# choices a subject makes. Summarize your results in a few sentences. 

guyer.mod <- lm(cooperation ~ condition*sex, data = Guyer)
summary(guyer.mod)

# Only the main effect of condition is significant.  

# Exercise 1b. (1 point) As you know, the coefficients in anova are directly 
# related to the means of each group. Explain this relationship: in particular, 
# explain how to interpret the intercept and the coefficient for condition in
# your anova output.

(guyer.means <- with(Guyer, tapply(cooperation, list(Condition=condition, Sex=sex), mean)))
#          Sex
# Condition    F    M
#         A 40.2 41.6
#         P 57.4 54.0


# The intercept is the mean of the Female, Anomymous condition. The coefficient
# for condition is difference between the anomymous and public condition for
# women. Subjects make on average 17.2 more cooperative choices in the Public condition.
(guyer.means[2,1]-guyer.means[1,1])  
# 17.2

# EXERCISE 2
# We are now going to look at the Ginzberg dataset
?Ginzberg # For additional information on the Ginzberg dataset
Ginzberg  # Print dataframe to the console

# Exercise 2a. (1 point) Using multiple linear regression, fit a general linear
# model to predict adjusted depression scores using adjusted simplicity and
# adjusted fatalism scores. Do not include an interaction term.

depression.model  <- lm(adjdep ~ adjsimp+adjfatal, data=Ginzberg)
summary(depression.model)

# Exercise 2b. (2 points) Calculate the adjusted R^2, F-value, and p-value for 
# your model from 2a "by hand" (i.e., use R to calculate SSerror and SStotal
# and use these values to compute the relevent statistics for your model).
k1 <- 1;
k2 <- length(depression.model$coefficients)
n <- length(depression.model$fitted.values)
sserror <- sum(depression.model$residual^2) # 11.47789
ss2  <- sserror  # I will use this below
mean.depression <- mean(depression.model$model$adjdep)
sstotal <- sum((depression.model$model$adjdep - mean.depression)^2) # 20.24999

adj.rsquare <- 1 - ((sserror/(n-k2))/(sstotal/(n-k1)) # 0.4188405
                    
(dfmodel <- k2-k1) # 2
(dferror <- n-k2) # 79
(fvalue <- ((sstotal-sserror)/dfmodel)/(sserror/dferror)) # 30.18827
                    
pf(fvalue, dfmodel, dferror, lower.tail=FALSE) # 1.822967e-10
# The adjusted R^2, F-value and p-value match those found at the end of the summary.
                    
# Exercise 2c. (1 point) Compare your model from 2a to a model that uses only 
# adjusted fatality scores to predict adjusted depression scores. Is the 
# difference between the two models significant? Perform your model comparison
# "by hand." (From the errors in each model)
depression.model.1  <- lm(adjdep ~ adjfatal, data=Ginzberg)
summary(depression.model.1)
                    
k1 <- length(depression.model.1$coefficients)
ss1 <- sum(depression.model.1$residual^2)
                    
(dfmodel <- k2-k1) # 1
(df2 <- n-k2) # 79
(fvalue <- ((ss1-ss2)/dfmodel)/(ss2/df2)) # 13.31235
                    
# This will return the probability
pf(fvalue, dfmodel, df2, lower.tail = FALSE) # 0.0004712635
                    
# Exercise 2d. (1 point) List two commands you could run to make R perform the 
# model comparison in 2c for you. Verify that when you run these commands, the
# p-value you get matches the p-value you calculated "by hand".
                    
# I can get this by comparing the two models
anova(depression.model.1, depression.model)
                    
# or from the Anova command
Anova(depression.model)
                    
# and it is also in the fourth column showing the p value for ajdsimp in the
# summary of the complete model.
                    
                    
# EXERCISE 3
# We are going to use a data set from the UCLA stats department on grad school
# admissions
gradSchoolAdmit <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
                    
# This dataset has a binary response variable called "admit". There are three 
# predictor variables:  "gre", "gpa" and "rank". We will treat "gre" and "gpa"
# as continuous variables. The variable "rank" takes values 1 through 4 where 
# institutions with a rank of 1 have the highest prestige, and institutions with
# a rank of 4 have the lowest and we will consider it a factor.
                    
# Exercise 3a. (1 point). Perform logistic regression on admit using gre,
# gpa and rank as predictors. Summarize the results (in words!).
                    
gradSchoolAdmit.model <- glm(admit ~ gre + gpa + as.factor(rank), data = gradSchoolAdmit, family=binomial(link=logit))
summary(gradSchoolAdmit.model)
# It looks like all factors affect admissions to graduate school
                    
# Exercise 3b. (2 points). Notice that you attend a rank 1 school (go bears!). But what
# is the effect of increasing your overall GPA by a full grade point?
                    
round(exp(cbind(Estimate=coef(gradSchoolAdmit.model), confint(gradSchoolAdmit.model))), 2)
#                  Estimate 2.5 % 97.5 %
# (Intercept)          0.02  0.00   0.17
# gre                  1.00  1.00   1.00
# gpa                  2.23  1.17   4.32
# as.factor(rank)2     0.51  0.27   0.94
# as.factor(rank)3     0.26  0.13   0.51
# as.factor(rank)4     0.21  0.09   0.47
  
# The odds of being admitted to graduate school are 2.23 greater if you increase
# your GPA by one point! A 123% increase in odds of admission???
                    

# Practice final Solutions ##########
# Psych 102. Practice Final. Fall 2014.           Name:  KEY
# Prof Theunissen

library(car)      # Always usefull to have!
# Note that you might have to load other libraries to get your work done

# Problem 1.
# Load and visualize the sleep data set
data(sleep)
View(sleep)

# This data set has the additional number of spleep hours (extra) as a function 
# of two different drugs given (group), each drug condition given to 10 
# different patients.  See ?sleep for more info.

#1a. Perform the correct linear model to see if there is a difference between
#the two drugs in terms of sleep hours. Display the summary results of this
#model. (2-pts)

# Because it is a within-subject test we need to use a mixed-effects model.
library(nlme)
mod1 = lme(extra ~ group, random= ~ 1 | ID, data=sleep)
summary(mod1)

#1b. Using the correct ttest perform the same statistical test.  Compare the
#results of the two ways of doing this by interpreting the coefficients of your
#model and comparing the p-values.(2 pts)
with(sleep, t.test(extra[group==1], extra[group==2], paired = TRUE ))

# The first coefficient of the model corresponds to the mean of group1 (0.75)
# the second coefficient corresponds to the mean of group 2 - mean of group 1 -
# in other words, the difference in means.  One would obtain the same result with lm.
# But the p-value takes into account the within subject design by estimating the variance
# of the differences and not the difference of the variances.  The p-values of the paired t-test
# correspond to the p-value of the lmne model. Compare to the p-value to
# the one given by the lm model, which fails to find significance:
mod1.wrong  <- lm(extra~group, data=sleep)
summary(mod1.wrong)

#1.c Use the tapply command to also find the value of the coefficients.
with(sleep, tapply(extra, group, mean))


#
# Problem 2.
#

# Load and visualize the wine data set from a Berkeley stats course...
wine = read.csv('http://www.stat.berkeley.edu/~s133/data/wine.data',header=FALSE)
names(wine) = c("Cultivar", "Alcohol", "Malic.acid", "Ash", "Alkalinity.ash",
                "Magnesium", "Phenols", "Flavanoids", "NF.phenols", "Proanthocyanins",
                "Color.intensity","Hue","OD.Ratio","Proline")
wine$Cultivar = factor(wine$Cultivar)
View(wine) 

#      These data are the results of a chemical analysis of
#      wines grown in the same region in Italy but derived from three
#      different cultivars.
#      The analysis determined the quantities of 13 constituents
#      found in each of the three types of wines. 

#2a. Perform the right analysis to determine whether wines from the three different
# Cultivars can be distinguished based on all 13 constituents.  How many dimensions are significant? 
# Generate a nice plot to look at these results. (2pts)

library(MASS)  
wine.mod  <- lda(Cultivar ~ ., data=wine)
wine.mod
# Two discriminant functions are siginificant.  
# The plot command gives a nice visual display.  It looks
# like the three dimensions are well separated.
plot(wine.mod)

# 2b.  Determine the percentage of average classification
# using 10 cross-validation sets where 10% of the data is saved
# each time for validation. Use a flat prior (ie. equal probability) (4pts)
nsets  <-  10
pcorrect  <- array(0, dim=nsets)
n  <- nrow(wine)
nvalid  <- floor(n*0.1)

for (i in 1:nsets) {
  valid <- sample(n, nvalid)
  wine.mod.cv <- lda(Cultivar ~ ., data=wine, prior = c(1,1,1)/3, subset = -valid)
  predict.Cultivar  <- predict(wine.mod.cv, wine[valid, ])$class
  pcorrect[i]  <- sum(predict.Cultivar == wine$Cultivar[valid])/nvalid 
}

pcorrect.avg  <- mean(pcorrect)

# I get a percent correct classification of 99%!

#
# Problem 3
#

# Let's examine the effect of smoking (smoke), race, number of
# physician visits (ftv), age and mother's weight (lwt) to predict birth weight (bwt)
#
library(MASS)
data(birthwt)
View(bithwt)
# Make some of the variables factors
birthwt$race  <- factor(birthwt$race, labels=c('white','black','other'))
birthwt$smoke  <- factor(birthwt$smoke, labels=c('no','yes'))
birthwt$low  <- factor(birthwt$low)
#3.a Use a scatter plot matrix to visualize the data and the potential effects.
# In one plot use different lines for different races and in another plot
# use different lines for smokers and non-smokers. You might want to turn off the smoother fit
# to prevent crowding. (2pts)

scatterplotMatrix(~ bwt + age + lwt + ftv, data=birthwt, group=birthwt$race, by.group=TRUE, smoother=FALSE) 
scatterplotMatrix(~ bwt + age + lwt + ftv, data=birthwt, group=birthwt$smoke, by.group=TRUE, smoother=FALSE)  

sessionInfo()
scatterplotMatrix(~ bwt + age + lwt +ftv | race, data=birthwt, by.groups.=TRUE, smooth=FALSE)

#3.b after visualizing the data fit the model or couple of models that
# seem the most appropriate.  Justify your choice and make conclusions. (4pts)

# First it seems that physicians vists are not correlated with birthweight - let's do a quick check...
bwt.model.check  <- lm(bwt ~ ftv*(race + smoke), data = birthwt)
summary(bwt.model.check)
# indeed none of the coefficients that include ftv (main effects or interactions) are significant
# I will exclude ftv from my final model.

# Let's try the model without interactions first
bwt.model  <- lm(bwt ~ age + smoke + race + lwt, data=birthwt)
summary(bwt.model)
# The model shows that even after controling for age and mothers weight, there is
# a main effect of race and smoking on birth weight.  Smoking results in
# an average decrease of 400 grams.

# One can also test interactions 
bwt.model.inter  <- lm(bwt ~ age*(smoke + race) + lwt, data=birthwt)
summary(bwt.model.inter)
# Our main effects for smoking and race seem to have dissapear.  Looking at the coefficients
# we can seen the age interacts with the smoke (worst for older smokers) and race
# variable (race effect is also greater for older moms). We would need more
# data to further examine those interactions since they are not significant.
# This data set is too small to test models with so many parameters.

# 3c. Compare the quality of the model that includes smoke, race and lwt to
# a model that only includes lwt (without interactions). Are these two models
# statistically different?  To answer that question, use a simple R command and
# verify the results by using the sum of square errors for each model and 
# calculating the appropriate F and df and doing the test "by hand" (4 pts)

mod2  <- lm(bwt ~ smoke + race + lwt, data=birthwt)
summary(mod2)
mod1  <- lm(bwt ~ lwt, data=birthwt)
summary(mod1)
anova(mod1, mod2)
# The two models are statiscally different from each other.

# We now perform this test by hand:
sserror1 <- sum(mod1$residual^2)
sserror2  <- sum(mod2$residual^2)
n <- length(mod2$model$bwt)
k1 <- length(mod1$coefficients)  # number of coefficients
k2 <- length(mod2$coefficients)  # number of coefficients
(fvalue <- ((sserror1-sserror2)/(k2-k1))/(sserror2/(n-k2)))
# This will return the probability
pf(fvalue, k2-k1, n-k2, lower.tail = FALSE)
# YES! I get the same results

#3.d The low variable is a binary variable that classifies 
# birthweights as being low or high.  Perform a logistic regression
# to test whether one can perform such classfication based
# on smoking, race and lwt.  Give an interpretation of the
# coefficients in terms of odds ratio. (2pts)

bwt.model.logistic  <- glm(low ~ smoke + race + lwt, data = birthwt, family = binomial)
summary(bwt.model.logistic) 

round(exp(cbind(Estimate=coef(bwt.model.logistic), confint(bwt.model.logistic))), 2)

# Smoking increases the odds of being in low category by 189%, black women
# odds ration of having low weith babies is 263% greater than whites, and other
# races see an increase of 164%.  Every additional lb for the mothers
# weithg decreases the odds of being in the low group by 1%.

#Random Stuff I thought was helpful

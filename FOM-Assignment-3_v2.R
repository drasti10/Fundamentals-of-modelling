#---------- A1: Skewness, Kurtosis, and Box Plots--------------
#install.packages("readxl")
#install.packages("e1071")
#install.packages("moments")
#install.packages("dplyr")
#install.packages("datarium")
#install.packages("rstatix")

library(readxl)
library(e1071)
library(dplyr)
library(datarium)
library(rstatix)
library(ggplot2)
library(bestNormalize)
library(chisq.posthoc.test)
#library(moments)

#dataset-1
rat_data <- read_excel("FOM_Assignment3_PartA_data.xlsx", sheet = 1)
#colnames(rat_data) <- "mm_of_water"
get_summary_stats(rat_data)
var(rat_data$`Millilitres of Water`)

# answer differs for skewness and kurtosis
skewness(rat_data$`Millilitres of Water`)
kurtosis(rat_data$`Millilitres of Water`)

boxplot(rat_data$`Millilitres of Water`,
        xlab = 'Millimeters of Water consumed on Day 1',
        horizontal = TRUE)
axis(1, at=seq(0, 18, 2))

#dataset-2
pop_data <- read_excel("FOM_Assignment3_PartA_data.xlsx", sheet = 2)
summary(pop_data$`2004 Population`)

# answer differs for sd, var, skewness, and kurtosis
sd(pop_data$`2004 Population`)
var(pop_data$`2004 Population`)
skewness(pop_data$`2004 Population`)
kurtosis(pop_data$`2004 Population`)

# answer differs
boxplot(pop_data$`2004 Population`/1000000,
        xlab = "Population in Milllions", horizontal = TRUE)
axis(1, at=seq(0, 40, 10))

#---------- A1: Skewness, Kurtosis, and Box Plots--------------


#---------- A2: Independent Groups t-Test--------------
#dataset-3
drug_data <- read_excel("FOM_Assignment3_PartA_data.xlsx", sheet = 3)

#the rstatix package lets you use field names by themselves
#we can't do much with "THC" and "Placebo" values in separate columns
#let's gather them into the SAME column in a new table
drug_data_updated <- gather(drug_data, key = "drug", value = "memory", THC, Placebo)

drug_data_grouping <- group_by(drug_data_updated,drug)

# let's find some stats
get_summary_stats(drug_data_grouping, memory, type = "mean_sd")
get_summary_stats(drug_data_grouping, memory, type = "full")
var(drug_data$THC)
var(drug_data$Placebo)

#we need to test some assumptions about our data first!
#first, does the sample contain any extreme outliers?
identify_outliers(drug_data_grouping, memory)
#yay! there are no extreme outliers in the sample

#second, is our data normally distributed?
#if p is > .05, the answer is yes
shapiro_test(drug_data_grouping, memory)
#yay! Shapiro says our sample is normally distributed! 
#as p-values for Placebo is 0.903 and THC is 0.513 both greater than 0.05


#we're not done testing yet!
#finally, we need to test for homogeneity of variance
#the ~ operator is a model separator:
#what's on the right of the ~ influences what's on the left
#if p is > .05, variances are homogenous
levene_test(drug_data_updated, memory ~ drug)
#yay! p-value is 1 so variance are homogenous
#you might get the error "group coerced to factor" - this has to do with
#the format of the variables in question - just disregard


#yay! now we can run our t-test!
#if we had p<0.05 in Levene results we will set var.equal to FALSE
# in our  case we won't do it
#this is called a "Welch's" t-test as opposed to a "Student's" t-test
# answer differs for t obt shows -1.42 instead of 1.42
t.result  <- t_test(drug_data_updated, memory ~ drug)
add_significance(t.result)

#you can't use p values to describe effect size
#instead, you would use Cohen's d
#if we had p<0.05 in Levene results we will set var.equal to FALSE
# in our  case we won't do it
cohens_d(drug_data_updated, memory ~ drug)

# bar plot remaining
#---------- A2: Independent Groups t-Test--------------




#---------- A3: Dependent Groups t-Test--------------

#dataset-4
bar_presses_data <- read_excel("FOM_Assignment3_PartA_data.xlsx", sheet = 4)
colnames(bar_presses_data) <- c("Rat_No","Pretest","After_Training")

#we can't do much with "Pretest" and "After_Training" values in separate columns
#let's gather them into the SAME column in a new table
bar_presses.long <- gather(bar_presses_data, key = "group", value = "barpress",
                           Pretest, After_Training)

#now let's get some stats

bar_presses.grouping <- group_by(bar_presses.long,group)
get_summary_stats(bar_presses.grouping, barpress, type = "mean_sd")
get_summary_stats(bar_presses.grouping, barpress, type = "full")
var(bar_presses_data$Pretest)
var(bar_presses_data$After_Training)


#it's not the scores that matter, but the differences
#so we need a new column that shows the differences
#the mutate function lets you transform data and add columns
bar_presses_data <- mutate(bar_presses_data, differences = Pretest - After_Training)

#we also need to identify any outliers in the new differences column
identify_outliers(bar_presses_data, differences)
#yay! no outliers were reported - not even moderate ones


#next we test for normality within the differences distribution
#if p is > .05, the distribution is normal
shapiro_test(bar_presses_data,differences)
#yay! Differences are normally distributed
#we DON'T need to test homogeneity of variances, since we're
#only evaluating ONE field - the differences column!

#yay! now we can run our t-test!
#we will set paired = TRUE
#note that we're running the test on bar_presses.long - the table
#we reorganized at the start of the analysis
# answer differs for t obt shows 2.66 instead of -2.66
t.result <- t_test(bar_presses.long,barpress ~ group,paired = TRUE)
add_significance(t.result)

#as before, we need to perform Cohen's d to measure effect size
#we will set paired = TRUE
#don't forget we're using the reorganized table
cohens_d(bar_presses.long,barpress ~ group,paired = TRUE)

#bar plot remaining
#---------- A3: Dependent Groups t-Test--------------



#---------- A4: Chi-Square Test--------------

#dataset-5
dropout_data <- read_excel("FOM_Assignment3_PartA_data.xlsx", sheet = 5)

# transposing the table and setting colnames for this particular case
dropout_data2 <- data.frame(t(dropout_data[-1]))
colnames(dropout_data2) <- c("Dropped Out:","Did not Drop:")

dropped <- dropout_data2$`Dropped Out:`
didnotdrop <- dropout_data2$`Did not Drop:`

# answer differs for chi-sqaured and p-value
#chisq.test(x = dropped, p = didnotdrop, rescale.p = TRUE)

# answer differs for p-value
chisq.test(x = dropout_data2, simulate.p.value = TRUE)

#---------- A4: Chi-Square Test--------------

#---------- A4: Chi-Square Test--------------

#dataset-6
sleep_data <- read_excel("FOM_Assignment3_PartA_data.xlsx", sheet = 6)

# transposing the table and setting colnames for this particular case
sleep_data2 <- data.frame(t(sleep_data[-1]))
colnames(sleep_data2) <- c("Observed:","Expected:")

obs <- sleep_data2$`Observed:`
exp <- sleep_data2$`Expected:`

chisq.test(x = obs, p = exp, rescale.p = TRUE)

#---------- A4: Chi-Square Test--------------




#---------- A5: Correlation and Regression--------------

#dataset-7
performance_data <- read_excel("FOM_Assignment3_PartA_data.xlsx", sheet = 7)

get_summary_stats(performance_data)

cor(x = performance_data$IQ, y = performance_data$Interview, method = "pearson")
cor(x = performance_data$IQ, y = performance_data$Performance, method = "pearson")
cor(x = performance_data$Performance, y = performance_data$Interview, method = "pearson")

lm(performance_data$Performance ~ performance_data$IQ, 
   data = performance_data)
# slope 0.0643 and intercept 0.7025

plot(x = performance_data$IQ,
     y = performance_data$Interview,
     main = "IQ vs Interview Score",
     xlab = "IQ",
     ylab = "Interview Score",
     xlim = c(70,150),
     ylim = c(1,7),
     pch = 16,col = "blue")

plot(x = performance_data$IQ,
     y = performance_data$Performance,
     main = "IQ vs Performance",
     xlab = "IQ",
     ylab = "Performance",
     xlim = c(70,150),
     ylim = c(1,10),
     pch = 16,col = "blue")
abline(lm(performance_data$Performance ~ performance_data$IQ, 
          data = performance_data), col = "red", lwd = 3)
text(x=130,y=7,labels="y = 0.0643 + 0.7025", col = "red")
text(x=132,y=6.5,labels="R2 = 0.4603", col = "red")

plot(x = performance_data$Interview,
     y = performance_data$Performance,
     main = "Interview Score vs Performance",
     xlab = "Interview Score",
     ylab = "Performance",
     xlim = c(1,8),
     ylim = c(1,10),
     pch = 16,col = "blue")
#---------- A5: Correlation and Regression--------------



#---------- B1. Are the continuous variables of "AGE", "OPEN", and "Neurotic" 
# in the data-set normally distributed? If any are not, how would you describe 
# these distributions (and what can you do to make them more normal?).--------------
#install.packages("bestNormalize")

p6_data <- read_excel("Sample6.xls")

colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
                       "Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1",
                       "MHgroup5")

# plotting histogram for visual representation of normal distribution of age
hist(p6_data$Age,xlab = "Age",main = "Age Distribution")
# looking at the histogram age does seem to be normally distributed

# we will run the shaprio test to further strengthen our view on normal distribution of age
# if p>0.05, the answer is yes
shapiro_test(p6_data, Age)
#yay! Shapiro says Age is normally distributed! (As p-value is 0.148>0.05)


# plotting histogram for visual representation of normal distribution of OPen
hist(p6_data$Open,xlab = "Openness",main = "Openness Distribution")
# looking at the histogram Open does not seem to be normally distributed

# we will run the shaprio test to further strengthen our view on normal distribution of age
# if p>0.05, the answer is yes
shapiro_test(p6_data, Open)
#p-value for Shapiro is 8.77e-13 which means Open is not normally distributed!
skewness(p6_data$Open)
#Here is the value of skewness is positive (2.056), which means the data are positively skewed 
# or skewed right, meaning that the right tail of the distribution is longer than the left.

# to make it more normal we would use bestNormalize functiom

open_normal <- bestNormalize(p6_data$Open)
hist(open_normal$x.t,xlab = "Log Transformed Openness",
     main = "Log Transformed Openness Distribution")
shapiro_test(open_normal$x.t)
skewness(open_normal$x.t)


# plotting histogram for visual representation of normal distribution of Neurotic
hist(p6_data$Neurotic,xlab = "Neuroticism",main = "Neuroticism Distribution")
# looking at the histogram Neurotic it is hard to decide if it's normally distributed

# we will run the shaprio test to further strengthen our view on normal distribution of age
# if p>0.05, the answer is yes
shapiro_test(p6_data, Neurotic)
#yay! Shapiro says Age is normally distributed! (As p-value is 0.115>0.05)

#----------end B1 end--------------



#---------- B2. Using an appropriate statistic, determine whether men and women
# scored significantly different on the variables of "Agreeable", "Cons", or "Extra".
# Also, comment on the appropriateness of the statistic(s) used from the perspective 
# of basic assumptions. --------------

# we have to compare mean values so we would use the t-test
# as there's no before after kinda values so we would use independent t-test

# lets start with Agreeable variable
p6_data <- read_excel("Sample6.xls")
colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
"Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1", "MHgroup5")

p6_data.grouping <- group_by(p6_data, Gender)
get_summary_stats(p6_data.grouping, Agreeable, type = "mean_sd")

#we need to test some assumptions about our data first!
#first, does the sample contain any extreme outliers?
identify_outliers(p6_data.grouping, Agreeable)
#yay! there are no extreme outliers in the sample

#second, is our data normally distributed?
#if p is > .05, the answer is yes
shapiro_test(p6_data.grouping, Agreeable)
#yay! Shapiro says our sample is normally distributed
# as p-value for Agreeable is 0.254 and 0.445 for m and f respectively

# Levene's test requires variable on the right hand side to be a factor
# so we will convert gender variable into factor

p6_data$Gender <-factor(p6_data$Gender, labels=c("Male","Female"))
is.factor(p6_data$Gender)

#we're not done testing yet!
#finally, we need to test for homogeneity of variance
#the ~ operator is a model separator:
#what's on the right of the ~ influences what's on the left
#if p is > .05, variances are homogenous
levene_test(p6_data, Agreeable ~ Gender)
# we got p-value as 0.218 so the variance between groups are same.

#yay! now we can run our t-test!
#we will set var.equal to FALSE if the Levene results shows p-value < 0.05
#this is called a "Welch's" t-test as opposed to a "Student's" t-test

t_test(p6_data, Agreeable ~ Gender)
# t-obt is -2.54 df is 95.5 and p-value is 0.0128

#being lazy we will run this function
t.result_agreeable <- t_test(p6_data, Agreeable ~ Gender)
add_significance(t.result_agreeable)
# we have single  * so we need to interpret accordingly

#you can't use p values to describe effect size
#instead, you would use Cohen's d
#once again, we set var.equal to FALSE if the Levene's result has p-value < 0.05
cohens_d(p6_data, Agreeable ~ Gender)
# that's a moderate effect.


# lets go ahead with Cons variable

p6_data <- read_excel("Sample6.xls")
colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
"Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1","MHgroup5")

#p6_data$Cons <- bestNormalize(p6_data$Cons)$x.t

p6_data.grouping <- group_by(p6_data, Gender)
get_summary_stats(p6_data.grouping, Cons, type = "mean_sd")

#we need to test some assumptions about our data first!
#first, does the sample contain any extreme outliers?
identify_outliers(p6_data.grouping, Cons)
#yay! there are no extreme outliers in the sample


#second, is our data normally distributed?
#if p is > .05, the answer is yes
shapiro_test(p6_data.grouping, Cons)
# shaprio values p < 0.05, so don;t go ahead

# Levene's test requires variable on the right hand side to be a factor
# so we will convert gender variable into factor

p6_data$Gender <-factor(p6_data$Gender, labels=c("Male","Female"))
is.factor(p6_data$Gender)

#we're not done testing yet!
#finally, we need to test for homogeneity of variance
#the ~ operator is a model separator:
#what's on the right of the ~ influences what's on the left
#if p is > .05, variances are homogenous
levene_test(p6_data, Cons ~ Gender)
# we got p-value as 0.869 so the variance between groups are same.

#yay! now we can run our t-test!
#we will set var.equal to FALSE if the Levene results shows p-value < 0.05
#this is called a "Welch's" t-test as opposed to a "Student's" t-test

t_test(p6_data, Cons ~ Gender)
# t-obt is -0.395 df is 98.0 and p-value is 0.694

#being lazy we will run this function
t.result_cons <- t_test(p6_data, Cons ~ Gender)
add_significance(t.result_cons)
# we have ns so we need to interpret accordingly

#you can't use p values to describe effect size
#instead, you would use Cohen's d
#once again, we set var.equal to FALSE if the Levene's result has p-value < 0.05
cohens_d(p6_data, Cons ~ Gender)
# that's a negligible effect.



# lets go ahead with Extra variable

p6_data <- read_excel("Sample6.xls")
colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
"Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1","MHgroup5")

p6_data.grouping <- group_by(p6_data, Gender)
get_summary_stats(p6_data.grouping, Extra, type = "mean_sd")

#we need to test some assumptions about our data first!
#first, does the sample contain any extreme outliers?
identify_outliers(p6_data.grouping, Extra)
#yay! there are no extreme outliers in the sample

#second, is our data normally distributed?
#if p is > .05, the answer is yes
shapiro_test(p6_data.grouping, Extra)
#yay! Shapiro says our sample is normally distributed
# as p-value for Extra is 0.0696 and 0.210 for m and f respectively

# Levene's test requires variable on the right hand side to be a factor
# so we will convert gender variable into factor

p6_data$Gender <-factor(p6_data$Gender, labels=c("Male","Female"))
is.factor(p6_data$Gender)

#we're not done testing yet!
#finally, we need to test for homogeneity of variance
#the ~ operator is a model separator:
#what's on the right of the ~ influences what's on the left
#if p is > .05, variances are homogenous
levene_test(p6_data, Extra ~ Gender)
# we got p-value as 0.794 so the variance between groups are same.

#yay! now we can run our t-test!
#we will set var.equal to FALSE if the Levene results shows p-value < 0.05
#this is called a "Welch's" t-test as opposed to a "Student's" t-test

t_test(p6_data, Extra ~ Gender)
# t-obt is 0.456 df is 98.0 and p-value is 0.65

#being lazy we will run this function
t.result_extra <- t_test(p6_data, Extra ~ Gender)
add_significance(t.result_extra)
# we have ns so we need to interpret accordingly

#you can't use p values to describe effect size
#instead, you would use Cohen's d
#once again, we set var.equal to FALSE if the Levene's result has p-value < 0.05
cohens_d(p6_data, Extra ~ Gender)
# that's a negligible effect.

#----------end B2 end--------------



#----------B3. Determine whether mental health symptoms ("T1MH" vs "T5MH") changed over the 5 years
# of the study for the sample. Also, comment on the appropriateness of the statistic(s) used
# from the perspective of basic assumptions. --------------

# as there's a before after kinda scenario (changed over the 5 years) so we would use paired t-test

p6_data <- read_excel("Sample6.xls")
colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
                       "Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1","MHgroup5")

#we can't do much with "T1MH" and "T5MH" values in separate columns
#let's gather them into the SAME column in a new table

p6_data.long <- gather(p6_data, key = "years", value = "severity", T1MH, T5MH)

#now let's get some summary statistics
p6_data.grouping <- group_by(p6_data.long, years)
get_summary_stats(p6_data.grouping, severity, type = "mean_sd")

#it's not the scores that matter, but the differences
#so we need a new column that shows the differences
#the mutate function lets you transform data and add columns
p6_data <- mutate(p6_data, differences = T1MH - T5MH)

#we also need to identify any outliers in the new differences column
identify_outliers(p6_data, differences)
# there are extreme outliers

#next we test for normality within the differences distribution
#if p is > .05, the distribution is normal
shapiro_test(p6_data, differences)
# not normally distributed


#----------end B3 end--------------



#----------B3. Determine whether mental health symptoms ("T1MH" vs "T5MH") changed over the 5 years
# of the study for the sample. Also, comment on the appropriateness of the statistic(s) used
# from the perspective of basic assumptions. --------------

# as there's a before after kinda scenario (changed over the 5 years) chi square goodness of fit test 
p6_data <- read_excel("Sample6.xls")
colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
                       "Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1","MHgroup5")

#

t1f <- c("0","1","2","3","4","5","6")
p6_data$T1MH <- factor(x=p6_data$T1MH, levels = t1f)
p6_data$T5MH <- factor(x=p6_data$T5MH, levels = t1f)

#create a frequency table from the data
t1.obs <- prop.table(ftable(p6_data$T1MH))

t5.obs <- ftable(p6_data$T5MH)

chisq.test(x = t5.obs, p = t1.obs)
#----------end B3 end--------------




#----------B4.Are the proportions of men and women reporting the presence (or absence) of mental health
# problems different at Time 1 ("MHgroup1")? What about at Time 5 ("Mhgroup5")? --------------
#install.packages("chisq.posthoc.test")


#chi square test of independence
p6_data <- read_excel("Sample6.xls")
colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
                       "Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1","MHgroup5")

p6_data.df1 <- table(p6_data$Gender,p6_data$MHgroup1)

chisq_test(x = p6_data.df1, simulate.p.value = TRUE)
chisq.posthoc.test(p6_data.df1)

p6_data.df5 <- table(p6_data$Gender,p6_data$MHgroup5)
chisq_test(x = p6_data.df5, simulate.p.value = TRUE)

chisq.posthoc.test(p6_data.df5)

#----------end B4 end--------------




#----------B5.What are the correlations (report to 3 decimals) for the following pairs of variables: T1MH
# and neurotic; extra and age; and age and T1MH. Report the p-values for each correlation. For
# each of the relevant correlations, what is the slope and intercept when T1MH is the Y
# variable (dependent variable)? One of the key assumptions when interpreting a correlation is
# that the x and y variables are linearly related. Do you think this assumption is met for each of
# the 3 correlations? If any correlation is problematic, what can you do to improve
# interpretability? Demonstrate empirically that the potential improvement works.  --------------
#install.packages("Hmisc")
#install.packages("corrplot")
# library(corrplot)
# library(Hmisc)

p6_data <- read_excel("Sample6.xls")
colnames(p6_data) <- c("Gender", "Age", "AgeGroups", "Agreeable", "Cons",
                       "Extra", "Neurotic", "Open", "T1MH", "T5MH", "MHgroup1","MHgroup5")

# p6_data.cor = cor(p6_data, method = c("pearson"))
# p6_data.rcorr = rcorr(as.matrix(p6_data))
# p6_data.rcorr
# 
# p6_data.coeff = p6_data.rcorr$r
# p6_data.p = p6_data.rcorr$P
# 
# corrplot(p6_data.cor)

#get_summary_stats(p6_data)

T1N <-cor.test(x = p6_data$T1MH, y = p6_data$Neurotic, method = "pearson")
ExAge <- cor.test(x = p6_data$Extra, y = p6_data$Age, method = "pearson")
AgeT1 <- cor.test(x = p6_data$Age, y = p6_data$T1MH, method = "pearson")

T1N$estimate
ExAge$estimate
AgeT1$estimate

T1N$p.value
ExAge$p.value
AgeT1$p.value

# according to the correlation coefficient &
# p-value only T1MH and Neurotic have moderate correlation

lm(p6_data$T1MH ~ p6_data$Neurotic,data = p6_data)
# here intercept is -0.49891 and slope is 0.03902


#One of the key assumptions when interpreting a correlation is
# that the x and y variables are linearly related. Do you think this assumption is met for each of
# the 3 correlations? If any correlation is problematic, what can you do to improve
# interpretability? Demonstrate empirically that the potential improvement works. 




# plot(x = p6_data$Neurotic,
#      y = p6_data$T1MH,
#      main = "T1MH vs Neurotic",
#      xlab = "Neurotic",
#      ylab = "T1MH",
#      pch = 16,col = "blue")
# abline(lm(p6_data$T1MH ~ p6_data$Neurotic,data = p6_data),col = "red")

# plot(x = p6_data$Extra,
#      y = p6_data$Age,
#      main = "Extra vs Age",
#      xlab = "Extra",
#      ylab = "Age",
#      pch = 16,col = "blue")
# abline(lm(p6_data$Age ~ p6_data$Extra,data = p6_data),col = "red")

# plot(x = p6_data$Age,
#      y = p6_data$T1MH,
#      main = "Age vs T1MH",
#      xlab = "Age",
#      ylab = "T1MH",
#      pch = 16,col = "blue")
# abline(lm(p6_data$T1MH ~ p6_data$Age,data = p6_data),col = "red")
#----------end B5 end--------------
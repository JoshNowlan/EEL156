#### Install Packages ####

if (!require("lme4")){install.packages("lme4")} #if required, install 

if (!require("lmerTest")){install.packages("lmerTest")} #if required, install 
library(lmerTest) #load library

if (!require("lmtest")){install.packages("lmtest")} # if required, install 
library(lmtest) #load library

if (!require("emmeans")){install.packages("emmeans")} # if required, install 
library(emmeans) #load library

if (!require("car")){install.packages("car")} # if required, install 
library("car") #load library

#==============================================================================
#Couple of quick notes:
#-Before inputting a file into the R script, copy and paste the data into a new excel file as values only. Because of the functions we use, R treats the data as non-numerical. So by doing this step, it allows the script to work. 

#-You will need to update which participants need to be removed. For example, if you have 10 people, then participants 11 and 12 need to be removed. 

#-For the all variables, when you are getting the mean and SD, you’ll need to change the number for the number of decimal places on the lines that looks like this:

#summary_stats_Core_Temp$Mean_Core_Temp <- round(summary_stats_Core_Temp$Mean_Core_Temp, 1) #Change the number at the end for number of decimal places

#summary_stats_Core_Temp$SD_Core_Temp <- round(summary_stats_Core_Temp$SD_Core_Temp, 1)  #Change the number at the end for number of decimal places

#Where 1 = one decimale place, and 2 = two decimal places. Depending on the data you may want 0,1, or 2 decimal places

#-For the 3x5 variables, just copy and paste the code from one of the variables (e.g. Core_Temp). Then highlight “Core_Temp”, press ctrl + F to find, then replace with a new variable name (e.g. Finger_Temp) Variable names match the column headers. ****Make sure not to replace the original variable (e.g. don’t replace the old Core_Temp variable. Make sure to change the decimal places.

#============================================================================================================

#This is for comparing the 1X3 Variables (such as TT time, these are the variables with only 1 datapoint)
#The 1 x 3 LMMs with Assumptions, ANOVA P values, and Pairwise Comparison (if necessary)

#import the csv file
import_data <- read.csv("C:/users/pwall/Downloads/EEL156-Data Reduction-THISONE-1x3data.csv")

participant_to_remove <- c("9","10","11","12" )
import_data1 <- subset(import_data, Participant != participant_to_remove)

# recode condition as factor
import_data1$Condition <- factor(import_data1$Condition,
                                 levels = c(1,2,3),
                                 labels = c("10C","20C", "32C"))

#Convert Data to numeric. No idea why its not numeric. I think it makes more sense when the data is complete, to copy and paste it into a new csv as values only to remove this issue
import_data1$TT_min <- as.numeric(import_data1$TT_min)
import_data1$TT_s <- as.numeric(import_data1$TT_s)

#Variable 1 - TT (minutes)

# Step 1 - visualize TT_min data. This is a good way to visualize and identify outliers
boxplot(import_data1$TT_min ~ import_data1$Condition)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model TT_min x Condition
TT_minxCondition.model <- lmer(TT_min ~ Condition + (1|Participant), data = import_data1) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(TT_minxCondition.model), freq = FALSE, main = "Residuals Histogram")
residuals_TT_min <- residuals(TT_minxCondition.model) #determine residuals for levines test
qqnorm(residuals_TT_min) #QQ Plot for Normality
qqline(residuals_TT_min) #QQ Plot for Normality
shapiro.test(residuals_TT_min) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(TT_minxCondition.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(TT_min ~ Condition, data = import_data1) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_TT_min <- aggregate(TT_min ~ Condition, data = import_data1, FUN = mean, na.rm = TRUE)
sd_TT_min <- aggregate(TT_min ~ Condition, data = import_data1, FUN = sd, na.rm = TRUE)
summary_stats_TT_min <- merge(mean_TT_min, sd_TT_min, by = "Condition")
names(summary_stats_TT_min) <- c("Condition", "Mean_TT_min", "SD_TT_min")
summary_stats_TT_min$Mean_TT_min <- round(summary_stats_TT_min$Mean_TT_min, 2)
summary_stats_TT_min$SD_TT_min <- round(summary_stats_TT_min$SD_TT_min, 2)
print(summary_stats_TT_min)

# Step 6 Run an RM-ANOVA on the LMM to gtet p values and effect size

anova(TT_minxCondition.model) # output typical ANOVA stats
effectsize::eta_squared(TT_minxCondition.model, partial = TRUE) # calclate partial eta square effect size

#Step 7 If significant (p < 0.05), run pairwise comparisons

emmeans(TT_minxCondition.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Variable 2: TT (seconds)

# Step 1 - visualize TT_s data. This is a good way to visualize and identify outliers
boxplot(import_data1$TT_s ~ import_data1$Condition)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model TT_s x Condition
TT_sxCondition.model <- lmer(TT_s ~ Condition + (1|Participant), data = import_data1) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(TT_sxCondition.model), freq = FALSE, main = "Residuals Histogram")
residuals_TT_s <- residuals(TT_sxCondition.model) #determine residuals for levines test
qqnorm(residuals_TT_s) #QQ Plot for Normality
qqline(residuals_TT_s) #QQ Plot for Normality
shapiro.test(residuals_TT_s) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(TT_sxCondition.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(TT_s ~ Condition, data = import_data1) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_TT_s <- aggregate(TT_s ~ Condition, data = import_data1, FUN = mean, na.rm = TRUE)
sd_TT_s <- aggregate(TT_s ~ Condition, data = import_data1, FUN = sd, na.rm = TRUE)
summary_stats_TT_s <- merge(mean_TT_S, sd_TT_S, by = "Condition")
names(summary_stats_TT_s) <- c("Condition", "Mean_TT_s", "SD_TT_s")
summary_stats_TT_s$Mean_TT_s <- round(summary_stats_TT_s$Mean_TT_s, 1)
summary_stats_TT_s$SD_TT_s <- round(summary_stats_TT_s$SD_TT_s, 1)
print(summary_stats_TT_s)


# Step 6 Run an RM-ANOVA on the LMM to gtet p values and effect size

anova(TT_sxCondition.model) # output typical ANOVA stats
effectsize::eta_squared(TT_sxCondition.model, partial = TRUE) # calclate partial eta square effect size


#Step 7 If significant (p < 0.05), run pairwise comparisons

emmeans(TT_sxCondition.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Variable 3: Delta Tcore

# Step 1 - visualize Delta_Tcore data. This is a good way to visualize and identify outliers
boxplot(import_data1$Delta_Tcore ~ import_data1$Condition)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model Delta_Tcore x Condition
Delta_TcorexCondition.model <- lmer(Delta_Tcore ~ Condition + (1|Participant), data = import_data1) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(Delta_TcorexCondition.model), freq = FALSE, main = "Residuals Histogram")
residuals_Delta_Tcore <- residuals(Delta_TcorexCondition.model) #determine residuals for levines test
qqnorm(residuals_Delta_Tcore) #QQ Plot for Normality
qqline(residuals_Delta_Tcore) #QQ Plot for Normality
shapiro.test(residuals_Delta_Tcore) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(Delta_TcorexCondition.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(Delta_Tcore ~ Condition, data = import_data1) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_Delta_Tcore <- aggregate(Delta_Tcore ~ Condition, data = import_data1, FUN = mean, na.rm = TRUE)
sd_Delta_Tcore <- aggregate(Delta_Tcore ~ Condition, data = import_data1, FUN = sd, na.rm = TRUE)
summary_stats_Delta_Tcore <- merge(mean_Delta_Tcore, sd_Delta_Tcore, by = "Condition")
names(summary_stats_Delta_Tcore) <- c("Condition", "Mean_Delta_Tcore", "SD_Delta_Tcore")
summary_stats_Delta_Tcore$Mean_Delta_Tcore <- round(summary_stats_Delta_Tcore$Mean_Delta_Tcore, 1)
summary_stats_Delta_Tcore$SD_Delta_Tcore <- round(summary_stats_Delta_Tcore$SD_Delta_Tcore, 1)
print(summary_stats_Delta_Tcore)


# Step 6 Run an RM-ANOVA on the LMM to gtet p values and effect size

anova(Delta_TcorexCondition.model) # output typical ANOVA stats
effectsize::eta_squared(Delta_TcorexCondition.model, partial = TRUE) # calclate partial eta square effect size


#Step 7 If significant (p < 0.05), run pairwise comparisons

emmeans(Delta_TcorexCondition.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#This Next set of data covers the 3 x 5 physiological data. Treat this as its own new thing and separate set of data

#import the csv file
import_data2 <- read.csv("C:/users/pwall/Downloads/EEL156-Data Reduction-THISONE-1x5data.csv")

#Remove Participants and Distances
participant_to_remove <- c("9", "10", "11", "12")
distance_to_remove <- c("2", "3", "4", "5", "7", "8", "9", "10", "12", "13", "14", "15", "17", "18", "19", "20")
import_data3 <- subset(import_data2, !(Participant %in% participant_to_remove))
import_data4 <- subset(import_data3, !(Distance %in% distance_to_remove))


# recode condition as factor
import_data4$Condition <- factor(import_data4$Condition,
                                 levels = c(1,2,3),
                                 labels = c("10C","20C", "32C"))

# recode distance as factor
import_data4$Distance <- factor(import_data4$Distance,
                                levels = c(1,6,11,16,21),
                                labels = c("0km","5km","10km","15km","20km"))

#============================================================================================================

#Variable 1 - Core_Temp

# Step 1 - visualize Core_Temp data. This is a good way to visualize and identify outliers
boxplot(import_data4$Core_Temp ~ import_data4$Condition)
boxplot(import_data4$Core_Temp ~ import_data4$Distance)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model Core_Temp x Condition
Core_TempxConditionxDistance.model <- lmer(Core_Temp ~ Condition * Distance + (1|Participant), data = import_data4) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(Core_TempxConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Core_Temp <- residuals(Core_TempxConditionxDistance.model) #determine residuals for levines test
qqnorm(residuals_Core_Temp) #QQ Plot for Normality
qqline(residuals_Core_Temp) #QQ Plot for Normality
shapiro.test(residuals_Core_Temp) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(Core_TempxConditionxDistance.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(Core_Temp ~ Condition, data = import_data4) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_Core_Temp <- aggregate(Core_Temp ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Core_Temp <- aggregate(Core_Temp ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Core_Temp <- merge(mean_Core_Temp, sd_Core_Temp, by = c("Condition", "Distance"))
names(summary_stats_Core_Temp) <- c("Condition", "Distance", "Mean_Core_Temp", "SD_Core_Temp")
summary_stats_Core_Temp$Condition <- factor(summary_stats_Core_Temp$Condition,
                                            levels = c("10C", "20C", "32C"))

summary_stats_Core_Temp$Distance <- factor(summary_stats_Core_Temp$Distance,
                                           levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Core_Temp <- summary_stats_Core_Temp[order(summary_stats_Core_Temp$Condition, summary_stats_Core_Temp$Distance), ]
summary_stats_Core_Temp$Mean_Core_Temp <- round(summary_stats_Core_Temp$Mean_Core_Temp, 1) #Change the number at the end for number of decimal places
summary_stats_Core_Temp$SD_Core_Temp <- round(summary_stats_Core_Temp$SD_Core_Temp, 1)  #Change the number at the end for number of decimal places
print(summary_stats_Core_Temp)


# Step 6 Run an RM-ANOVA on the LMM to get p values and effect size

anova(Core_TempxConditionxDistance.model) # output typical ANOVA stats
effectsize::eta_squared(Core_TempxConditionxDistance.model, partial = TRUE) # calclate partial eta square effect size

#Step 7 If Condition is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(Core_TempxConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 8 If Distance is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(Core_TempxConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 9 If Condition* Distance Interaction is significant (p < 0.05), run pairwise comparisons, if not, do not run. Here, you are essentially running an RM ANOVA at each distance

emmeans(Core_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "0km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Core_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "5km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Core_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "10km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Core_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "15km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Core_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "20km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline

#============================================================================================================
#Variable 2 - Split_Time_min

# Step 1 - visualize Split_Time_min data. This is a good way to visualize and identify outliers
boxplot(import_data4$Split_Time_min ~ import_data4$Condition)
boxplot(import_data4$Split_Time_min ~ import_data4$Distance)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model Split_Time_min x Condition
Split_Time_minxConditionxDistance.model <- lmer(Split_Time_min ~ Condition * Distance + (1|Participant), data = import_data4) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(Split_Time_minxConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Split_Time_min <- residuals(Split_Time_minxConditionxDistance.model) #determine residuals for levines test
qqnorm(residuals_Split_Time_min) #QQ Plot for Normality
qqline(residuals_Split_Time_min) #QQ Plot for Normality
shapiro.test(residuals_Split_Time_min) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(Split_Time_minxConditionxDistance.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(Split_Time_min ~ Condition, data = import_data4) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_Split_Time_min <- aggregate(Split_Time_min ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Split_Time_min <- aggregate(Split_Time_min ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Split_Time_min <- merge(mean_Split_Time_min, sd_Split_Time_min, by = c("Condition", "Distance"))
names(summary_stats_Split_Time_min) <- c("Condition", "Distance", "Mean_Split_Time_min", "SD_Split_Time_min")
summary_stats_Split_Time_min$Condition <- factor(summary_stats_Split_Time_min$Condition,
                                                 levels = c("10C", "20C", "32C"))

summary_stats_Split_Time_min$Distance <- factor(summary_stats_Split_Time_min$Distance,
                                                levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Split_Time_min <- summary_stats_Split_Time_min[order(summary_stats_Split_Time_min$Condition, summary_stats_Split_Time_min$Distance), ]
summary_stats_Split_Time_min$Mean_Split_Time_min <- round(summary_stats_Split_Time_min$Mean_Split_Time_min, 2) #Change the number at the end for number of decimal places
summary_stats_Split_Time_min$SD_Split_Time_min <- round(summary_stats_Split_Time_min$SD_Split_Time_min, 2)  #Change the number at the end for number of decimal places
print(summary_stats_Split_Time_min)


# Step 6 Run an RM-ANOVA on the LMM to get p values and effect size

anova(Split_Time_minxConditionxDistance.model) # output typical ANOVA stats
effectsize::eta_squared(Split_Time_minxConditionxDistance.model, partial = TRUE) # calclate partial eta square effect size

#Step 7 If Condition is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(Split_Time_minxConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 8 If Distance is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(Split_Time_minxConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 9 If Condition* Distance Interaction is significant (p < 0.05), run pairwise comparisons, if not, do not run. Here, you are essentially running an RM ANOVA at each distance

emmeans(Split_Time_minxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "0km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Split_Time_minxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "5km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Split_Time_minxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "10km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Split_Time_minxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "15km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Split_Time_minxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "20km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline

#============================================================================================================
#Variable 3 - Watts

# Step 1 - visualize Watts data. This is a good way to visualize and identify outliers
boxplot(import_data4$Watts ~ import_data4$Condition)
boxplot(import_data4$Watts ~ import_data4$Distance)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model Watts x Condition
WattsxConditionxDistance.model <- lmer(Watts ~ Condition * Distance + (1|Participant), data = import_data4) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(WattsxConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Watts <- residuals(WattsxConditionxDistance.model) #determine residuals for levines test
qqnorm(residuals_Watts) #QQ Plot for Normality
qqline(residuals_Watts) #QQ Plot for Normality
shapiro.test(residuals_Watts) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(WattsxConditionxDistance.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(Watts ~ Condition, data = import_data4) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_Watts <- aggregate(Watts ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Watts <- aggregate(Watts ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Watts <- merge(mean_Watts, sd_Watts, by = c("Condition", "Distance"))
names(summary_stats_Watts) <- c("Condition", "Distance", "Mean_Watts", "SD_Watts")
summary_stats_Watts$Condition <- factor(summary_stats_Watts$Condition,
                                        levels = c("10C", "20C", "32C"))

summary_stats_Watts$Distance <- factor(summary_stats_Watts$Distance,
                                       levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Watts <- summary_stats_Watts[order(summary_stats_Watts$Condition, summary_stats_Watts$Distance), ]
summary_stats_Watts$Mean_Watts <- round(summary_stats_Watts$Mean_Watts, 0) #Change the number at the end for number of decimal places
summary_stats_Watts$SD_Watts <- round(summary_stats_Watts$SD_Watts, 0)  #Change the number at the end for number of decimal places
print(summary_stats_Watts)


# Step 6 Run an RM-ANOVA on the LMM to get p values and effect size

anova(WattsxConditionxDistance.model) # output typical ANOVA stats
effectsize::eta_squared(WattsxConditionxDistance.model, partial = TRUE) # calclate partial eta square effect size

#Step 7 If Condition is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(WattsxConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 8 If Distance is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(WattsxConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 9 If Condition* Distance Interaction is significant (p < 0.05), run pairwise comparisons, if not, do not run. Here, you are essentially running an RM ANOVA at each distance

emmeans(WattsxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "0km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(WattsxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "5km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(WattsxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "10km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(WattsxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "15km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(WattsxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "20km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline

#============================================================================================================
#Variable 4 - Heat_Exchange

# Step 1 - visualize Heat_Exchange data. This is a good way to visualize and identify outliers
boxplot(import_data4$Heat_Exchange ~ import_data4$Condition)
boxplot(import_data4$Heat_Exchange ~ import_data4$Distance)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model Heat_Exchange x Condition
Heat_ExchangexConditionxDistance.model <- lmer(Heat_Exchange ~ Condition * Distance + (1|Participant), data = import_data4) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(Heat_ExchangexConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Heat_Exchange <- residuals(Heat_ExchangexConditionxDistance.model) #determine residuals for levines test
qqnorm(residuals_Heat_Exchange) #QQ Plot for Normality
qqline(residuals_Heat_Exchange) #QQ Plot for Normality
shapiro.test(residuals_Heat_Exchange) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(Heat_ExchangexConditionxDistance.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(Heat_Exchange ~ Condition, data = import_data4) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_Heat_Exchange <- aggregate(Heat_Exchange ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Heat_Exchange <- aggregate(Heat_Exchange ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Heat_Exchange <- merge(mean_Heat_Exchange, sd_Heat_Exchange, by = c("Condition", "Distance"))
names(summary_stats_Heat_Exchange) <- c("Condition", "Distance", "Mean_Heat_Exchange", "SD_Heat_Exchange")
summary_stats_Heat_Exchange$Condition <- factor(summary_stats_Heat_Exchange$Condition,
                                          levels = c("10C", "20C", "32C"))

summary_stats_Heat_Exchange$Distance <- factor(summary_stats_Heat_Exchange$Distance,
                                         levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Heat_Exchange <- summary_stats_Heat_Exchange[order(summary_stats_Heat_Exchange$Condition, summary_stats_Heat_Exchange$Distance), ]
summary_stats_Heat_Exchange$Mean_Heat_Exchange <- round(summary_stats_Heat_Exchange$Mean_Heat_Exchange, 0) #Change the number at the end for number of decimal places
summary_stats_Heat_Exchange$SD_Heat_Exchange <- round(summary_stats_Heat_Exchange$SD_Heat_Exchange, 0)  #Change the number at the end for number of decimal places
print(summary_stats_Heat_Exchange)


# Step 6 Run an RM-ANOVA on the LMM to get p values and effect size

anova(Heat_ExchangexConditionxDistance.model) # output typical ANOVA stats
effectsize::eta_squared(Heat_ExchangexConditionxDistance.model, partial = TRUE) # calclate partial eta square effect size

#Step 7 If Condition is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(Heat_ExchangexConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 8 If Distance is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(Heat_ExchangexConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 9 If Condition* Distance Interaction is significant (p < 0.05), run pairwise comparisons, if not, do not run. Here, you are essentially running an RM ANOVA at each distance

emmeans(Heat_ExchangexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "0km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Heat_ExchangexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "5km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Heat_ExchangexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "10km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Heat_ExchangexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "15km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(Heat_ExchangexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "20km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Variable 5 - Cadence

# Step 1 - visualize Cadence data. This is a good way to visualize and identify outliers
boxplot(import_data4$Cadence ~ import_data4$Condition)
boxplot(import_data4$Cadence ~ import_data4$Distance)

# Next step, need to first build the model, then check for normal distribution and homoscedascticity. If all good, run the ANOVA, if significant (p < 0.05) run pairwise comparisons

# Step 2 - create model Cadence x Condition
CadencexConditionxDistance.model <- lmer(Cadence ~ Condition * Distance + (1|Participant), data = import_data4) # Builds the model w/ Random intercept for participant

# Step 3 Check for normality using wilk shapiro (only works if no missing data) and Q-Q Plot

hist(residuals(CadencexConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Cadence <- residuals(CadencexConditionxDistance.model) #determine residuals for levines test
qqnorm(residuals_Cadence) #QQ Plot for Normality
qqline(residuals_Cadence) #QQ Plot for Normality
shapiro.test(residuals_Cadence) #(if p < 0.05, no normality combined with Q-Q plot interpretation)

#Step 4 Check for homoscedasticity using a plot and levenetest (if all data present)

plot(CadencexConditionxDistance.model, resid = TRUE) #visual plot of residuals over fitted model, aiming for constant spread around zero, do not want funnel shape or systematice change in spread
leveneTest(Cadence ~ Condition, data = import_data4) #(if p < 0.05, no homoscedasticity combined with plot interpretation)

#Step 5 Find Means and Standard Deviations to be used in results section and graphpad figures:

mean_Cadence <- aggregate(Cadence ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Cadence <- aggregate(Cadence ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Cadence <- merge(mean_Cadence, sd_Cadence, by = c("Condition", "Distance"))
names(summary_stats_Cadence) <- c("Condition", "Distance", "Mean_Cadence", "SD_Cadence")
summary_stats_Cadence$Condition <- factor(summary_stats_Cadence$Condition,
                                          levels = c("10C", "20C", "32C"))

summary_stats_Cadence$Distance <- factor(summary_stats_Cadence$Distance,
                                         levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Cadence <- summary_stats_Cadence[order(summary_stats_Cadence$Condition, summary_stats_Cadence$Distance), ]
summary_stats_Cadence$Mean_Cadence <- round(summary_stats_Cadence$Mean_Cadence, 0) #Change the number at the end for number of decimal places
summary_stats_Cadence$SD_Cadence <- round(summary_stats_Cadence$SD_Cadence, 0)  #Change the number at the end for number of decimal places
print(summary_stats_Cadence)


# Step 6 Run an RM-ANOVA on the LMM to get p values and effect size

anova(CadencexConditionxDistance.model) # output typical ANOVA stats
effectsize::eta_squared(CadencexConditionxDistance.model, partial = TRUE) # calclate partial eta square effect size

#Step 7 If Condition is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(CadencexConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 8 If Distance is significant (p < 0.05), run pairwise comparisons, if not, do not run

emmeans(CadencexConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni") # pairwise comp w/ Bonferroni

#Step 9 If Condition* Distance Interaction is significant (p < 0.05), run pairwise comparisons, if not, do not run. Here, you are essentially running an RM ANOVA at each distance

emmeans(CadencexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "0km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(CadencexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "5km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(CadencexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "10km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(CadencexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "15km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
emmeans(CadencexConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = "20km"), adjust = "bonferroni") # pairwise comp for interaction w/ Bonferroni at baseline
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Variable 5 - Finger_Temp

# Step 1 - Visualize Finger_Temp data
boxplot(import_data4$Finger_Temp ~ import_data4$Condition)
boxplot(import_data4$Finger_Temp ~ import_data4$Distance)

# Step 2 - Create model Finger_Temp x Condition x Distance
Finger_TempxConditionxDistance.model <- lmer(Finger_Temp ~ Condition * Distance + (1|Participant), data = import_data4)

# Step 3 - Check for normality using Shapiro-Wilk and Q-Q Plot
hist(residuals(Finger_TempxConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Finger_Temp <- residuals(Finger_TempxConditionxDistance.model)
qqnorm(residuals_Finger_Temp)
qqline(residuals_Finger_Temp)
shapiro.test(residuals_Finger_Temp)

# Step 4 - Check for homoscedasticity using a residuals plot and Levene's test
plot(Finger_TempxConditionxDistance.model, resid = TRUE)
leveneTest(Finger_Temp ~ Condition, data = import_data4)

# Step 5 - Find Means and Standard Deviations
mean_Finger_Temp <- aggregate(Finger_Temp ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Finger_Temp <- aggregate(Finger_Temp ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Finger_Temp <- merge(mean_Finger_Temp, sd_Finger_Temp, by = c("Condition", "Distance"))
names(summary_stats_Finger_Temp) <- c("Condition", "Distance", "Mean_Finger_Temp", "SD_Finger_Temp")
summary_stats_Finger_Temp$Condition <- factor(summary_stats_Finger_Temp$Condition,
                                              levels = c("10C", "20C", "32C"))
summary_stats_Finger_Temp$Distance <- factor(summary_stats_Finger_Temp$Distance,
                                             levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Finger_Temp <- summary_stats_Finger_Temp[order(summary_stats_Finger_Temp$Condition, summary_stats_Finger_Temp$Distance), ]
summary_stats_Finger_Temp$Mean_Finger_Temp <- round(summary_stats_Finger_Temp$Mean_Finger_Temp, 0)
summary_stats_Finger_Temp$SD_Finger_Temp <- round(summary_stats_Finger_Temp$SD_Finger_Temp, 0)
print(summary_stats_Finger_Temp)

# Step 6 - RM-ANOVA and effect size
anova(Finger_TempxConditionxDistance.model)
effectsize::eta_squared(Finger_TempxConditionxDistance.model, partial = TRUE)

# Step 7 - Pairwise comparisons for Condition
emmeans(Finger_TempxConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni")

# Step 8 - Pairwise comparisons for Distance
emmeans(Finger_TempxConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni")

# Step 9 - Pairwise comparisons for Condition*Distance Interaction
distances <- c("0km", "5km", "10km", "15km", "20km")
for (distance in distances) {
  emmeans(Finger_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = distance), adjust = "bonferroni")
}
#######################################################################################
# Variable 5 - Mean_Skin_Temp

# Step 1 - Visualize Mean_Skin_Temp data
boxplot(import_data4$Mean_Skin_Temp ~ import_data4$Condition)
boxplot(import_data4$Mean_Skin_Temp ~ import_data4$Distance)

# Step 2 - Create model Mean_Skin_Temp x Condition x Distance
Mean_Skin_TempxConditionxDistance.model <- lmer(Mean_Skin_Temp ~ Condition * Distance + (1|Participant), data = import_data4)

# Step 3 - Check for normality using Shapiro-Wilk and Q-Q Plot
hist(residuals(Mean_Skin_TempxConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Mean_Skin_Temp <- residuals(Mean_Skin_TempxConditionxDistance.model)
qqnorm(residuals_Mean_Skin_Temp)
qqline(residuals_Mean_Skin_Temp)
shapiro.test(residuals_Mean_Skin_Temp)

# Step 4 - Check for homoscedasticity using a residuals plot and Levene's test
plot(Mean_Skin_TempxConditionxDistance.model, resid = TRUE)
leveneTest(Mean_Skin_Temp ~ Condition, data = import_data4)

# Step 5 - Find Means and Standard Deviations
mean_Mean_Skin_Temp <- aggregate(Mean_Skin_Temp ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Mean_Skin_Temp <- aggregate(Mean_Skin_Temp ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Mean_Skin_Temp <- merge(mean_Mean_Skin_Temp, sd_Mean_Skin_Temp, by = c("Condition", "Distance"))
names(summary_stats_Mean_Skin_Temp) <- c("Condition", "Distance", "Mean_Mean_Skin_Temp", "SD_Mean_Skin_Temp")
summary_stats_Mean_Skin_Temp$Condition <- factor(summary_stats_Mean_Skin_Temp$Condition,
                                                 levels = c("10C", "20C", "32C"))
summary_stats_Mean_Skin_Temp$Distance <- factor(summary_stats_Mean_Skin_Temp$Distance,
                                                levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Mean_Skin_Temp <- summary_stats_Mean_Skin_Temp[order(summary_stats_Mean_Skin_Temp$Condition, summary_stats_Mean_Skin_Temp$Distance), ]
summary_stats_Mean_Skin_Temp$Mean_Mean_Skin_Temp <- round(summary_stats_Mean_Skin_Temp$Mean_Mean_Skin_Temp, 0)
summary_stats_Mean_Skin_Temp$SD_Mean_Skin_Temp <- round(summary_stats_Mean_Skin_Temp$SD_Mean_Skin_Temp, 0)
print(summary_stats_Mean_Skin_Temp)

# Step 6 - RM-ANOVA and effect size
anova(Mean_Skin_TempxConditionxDistance.model)
effectsize::eta_squared(Mean_Skin_TempxConditionxDistance.model, partial = TRUE)

# Step 7 - Pairwise comparisons for Condition
emmeans(Mean_Skin_TempxConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni")

# Step 8 - Pairwise comparisons for Distance
emmeans(Mean_Skin_TempxConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni")

# Step 9 - Pairwise comparisons for Condition*Distance Interaction
for (distance in distances) {
  emmeans(Mean_Skin_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = distance), adjust = "bonferroni")
}
##################################################################################
# Variable 5 - Cleaned_HR

# Step 1 - Visualize Cleaned_HR data
boxplot(import_data4$Cleaned_HR ~ import_data4$Condition)
boxplot(import_data4$Cleaned_HR ~ import_data4$Distance)

# Step 2 - Create model Cleaned_HR x Condition x Distance
Cleaned_HRxConditionxDistance.model <- lmer(Cleaned_HR ~ Condition * Distance + (1|Participant), data = import_data4)

# Step 3 - Check for normality using Shapiro-Wilk and Q-Q Plot
hist(residuals(Cleaned_HRxConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Cleaned_HR <- residuals(Cleaned_HRxConditionxDistance.model)
qqnorm(residuals_Cleaned_HR)
qqline(residuals_Cleaned_HR)
shapiro.test(residuals_Cleaned_HR)

# Step 4 - Check for homoscedasticity using a residuals plot and Levene's test
plot(Cleaned_HRxConditionxDistance.model, resid = TRUE)
leveneTest(Cleaned_HR ~ Condition, data = import_data4)

# Step 5 - Find Means and Standard Deviations
mean_Cleaned_HR <- aggregate(Cleaned_HR ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Cleaned_HR <- aggregate(Cleaned_HR ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Cleaned_HR <- merge(mean_Cleaned_HR, sd_Cleaned_HR, by = c("Condition", "Distance"))
names(summary_stats_Cleaned_HR) <- c("Condition", "Distance", "Mean_Cleaned_HR", "SD_Cleaned_HR")
summary_stats_Cleaned_HR$Condition <- factor(summary_stats_Cleaned_HR$Condition,
                                             levels = c("10C", "20C", "32C"))
summary_stats_Cleaned_HR$Distance <- factor(summary_stats_Cleaned_HR$Distance,
                                            levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Cleaned_HR <- summary_stats_Cleaned_HR[order(summary_stats_Cleaned_HR$Condition, summary_stats_Cleaned_HR$Distance), ]
summary_stats_Cleaned_HR$Mean_Cleaned_HR <- round(summary_stats_Cleaned_HR$Mean_Cleaned_HR, 0)
summary_stats_Cleaned_HR$SD_Cleaned_HR <- round(summary_stats_Cleaned_HR$SD_Cleaned_HR, 0)
print(summary_stats_Cleaned_HR)

# Step 6 - RM-ANOVA and effect size
anova(Cleaned_HRxConditionxDistance.model)
effectsize::eta_squared(Cleaned_HRxConditionxDistance.model, partial = TRUE)

# Step 7 - Pairwise comparisons for Condition
emmeans(Cleaned_HRxConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni")

# Step 8 - Pairwise comparisons for Distance
emmeans(Cleaned_HRxConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni")

# Step 9 - Pairwise comparisons for Condition*Distance Interaction
for (distance in distances) {
  emmeans(Cleaned_HRxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = distance), adjust = "bonferroni")
}
###########################################################################################
# Variable 5 - Immersion_Box_Temp

# Step 1 - Visualize Immersion_Box_Temp data
boxplot(import_data4$Immersion_Box_Temp ~ import_data4$Condition)
boxplot(import_data4$Immersion_Box_Temp ~ import_data4$Distance)

# Step 2 - Create model Immersion_Box_Temp x Condition x Distance
Immersion_Box_TempxConditionxDistance.model <- lmer(Immersion_Box_Temp ~ Condition * Distance + (1|Participant), data = import_data4)

# Step 3 - Check for normality using Shapiro-Wilk and Q-Q Plot
hist(residuals(Immersion_Box_TempxConditionxDistance.model), freq = FALSE, main = "Residuals Histogram")
residuals_Immersion_Box_Temp <- residuals(Immersion_Box_TempxConditionxDistance.model)
qqnorm(residuals_Immersion_Box_Temp)
qqline(residuals_Immersion_Box_Temp)
shapiro.test(residuals_Immersion_Box_Temp)

# Step 4 - Check for homoscedasticity using a residuals plot and Levene's test
plot(Immersion_Box_TempxConditionxDistance.model, resid = TRUE)
leveneTest(Immersion_Box_Temp ~ Condition, data = import_data4)

# Step 5 - Find Means and Standard Deviations
mean_Immersion_Box_Temp <- aggregate(Immersion_Box_Temp ~ Condition + Distance, data = import_data4, FUN = mean, na.rm = TRUE)
sd_Immersion_Box_Temp <- aggregate(Immersion_Box_Temp ~ Condition + Distance, data = import_data4, FUN = sd, na.rm = TRUE)
summary_stats_Immersion_Box_Temp <- merge(mean_Immersion_Box_Temp, sd_Immersion_Box_Temp, by = c("Condition", "Distance"))
names(summary_stats_Immersion_Box_Temp) <- c("Condition", "Distance", "Mean_Immersion_Box_Temp", "SD_Immersion_Box_Temp")
summary_stats_Immersion_Box_Temp$Condition <- factor(summary_stats_Immersion_Box_Temp$Condition,
                                                     levels = c("10C", "20C", "32C"))
summary_stats_Immersion_Box_Temp$Distance <- factor(summary_stats_Immersion_Box_Temp$Distance,
                                                    levels = c("0km", "5km", "10km", "15km", "20km"))
summary_stats_Immersion_Box_Temp <- summary_stats_Immersion_Box_Temp[order(summary_stats_Immersion_Box_Temp$Condition, summary_stats_Immersion_Box_Temp$Distance), ]
summary_stats_Immersion_Box_Temp$Mean_Immersion_Box_Temp <- round(summary_stats_Immersion_Box_Temp$Mean_Immersion_Box_Temp, 0)
summary_stats_Immersion_Box_Temp$SD_Immersion_Box_Temp <- round(summary_stats_Immersion_Box_Temp$SD_Immersion_Box_Temp, 0)
print(summary_stats_Immersion_Box_Temp)

# Step 6 - RM-ANOVA and effect size
anova(Immersion_Box_TempxConditionxDistance.model)
effectsize::eta_squared(Immersion_Box_TempxConditionxDistance.model, partial = TRUE)

# Step 7 - Pairwise comparisons for Condition
emmeans(Immersion_Box_TempxConditionxDistance.model, list(pairwise ~ Condition), adjust = "bonferroni")

# Step 8 - Pairwise comparisons for Distance
emmeans(Immersion_Box_TempxConditionxDistance.model, list(pairwise ~ Distance), adjust = "bonferroni")

# Step 9 - Pairwise comparisons for Condition*Distance Interaction
for (distance in distances) {
  emmeans(Immersion_Box_TempxConditionxDistance.model, list(pairwise ~ Condition), at = list(Time = distance), adjust = "bonferroni")
}


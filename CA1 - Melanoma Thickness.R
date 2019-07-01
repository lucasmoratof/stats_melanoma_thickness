library(tidyverse) # for data manipulation and plots
library(car) # to generate nice qqplots
library(dunn.test) # post-hoc test
library(FSA) # post-hoc test
library(pwr) #  power test
library(effsize) # cohen test

# Investigation of Melanoma Thichkness

# Load data and checking first lines
# ---------------------------
melanoma <- read_csv("melanoma.csv")
melanoma <- melanoma[,-1] # remove the first unecessary row name column
nrow(melanoma)
head(melanoma)

## Wilcoxon's tests - unpaired
# ---------------------------
# Question: check difference between thickness of melanoma for patients who died based on gender
# Null Hypothesis : there's no difference between the melanoma thickness between the two groups,
# Rank of Males = Rank of females
# Alternative Hyphotesis : there is a dufference between the melanoma thichness between the groups,
# Rank Males different of Rank of Females
# ---------------------------
# Normality Tests
d_melanoma <- melanoma %>% filter(status == 1) # creating group of fatal cases
d_melanoma$sex <- as.factor(d_melanoma$sex) # change sex variable to factor to make easier to plot
m_d_melanoma <- d_melanoma %>% filter(sex==1) # males who died from melanoma
f_d_melanoma <- d_melanoma %>% filter(sex==0) # females who died from melanoma
# Qqplot
qqPlot(m_d_melanoma$thickness, main = "Males", ylab = "Melanoma Thickness")
qqPlot(f_d_melanoma$thickness, main = "Females", col.lines = "red", ylab = "Melanoma Thickness")
# Boxplot 
boxplot(d_melanoma$thickness ~ d_melanoma$sex, 
        main = "Thickness of melanoma for patients\n who died, by gender", 
        names = c("Female","Male")) 
# Distribution Plot
d_melanoma %>% 
  ggplot(., aes(x=thickness, fill=sex)) +
  geom_density(alpha=0.4) +
  ggtitle("Melanoma thickness in fatal cases\n based on gender") +
  scale_fill_discrete(name = "Gender", labels=c("Female","Male")) 
# Shapiro tests
# For males
shapiro.test(m_d_melanoma$thickness) # not normal
# For females
shapiro.test(f_d_melanoma$thickness) # not normal

# Performing Wilcoxon's test
# --------------------------
rst = wilcox.test(m_d_melanoma$thickness, f_d_melanoma$thickness, paired = FALSE)
rst
qnorm(rst$p.value/2) # gives the Z value to use on the report
median(f_d_melanoma$thickness) # mediam for formal report
median(m_d_melanoma$thickness) # mediam for formal report
# There is a significant difference on melanoma thickness between males and females who died from
# this type of cancer.

# Power test
# --------------------
# Power for thickness for people who died form melanoma grouped by 'sex' factor. 
# Two levels: 1 (males), 0 (females)
cohen.d(m_d_melanoma$thickness, f_d_melanoma$thickness) # d=0.18
# Find the n for each group
plot(pwr.t.test(d=0.18, power = 0.95, sig.level = 0.001)) # n=1507, total of 3014 people
# finding the power of the biggest sample group we have, n = 29 (f_d_melanoma)
power.t.test(power = NULL, n= 29, d= 0.18, sig.level = 0.001, type = "two.sample", alternative = "two.sided")

## Kruskal-Wallis test
# --------------------
# Question: difference on melanoma thickness of whom: 
# 1 - died from melonoma;
# 2 - survived the melanoma and are still alive;
# 3 - survived the melanoma and died from unrelated reasons
# --------------------
# Creating groups
group_1 <- melanoma %>% filter(status == 1)
group_2 <- melanoma %>% filter(status == 2)
group_3 <- melanoma %>% filter(status == 3)
# Qqplot
qqPlot(group_1$thickness, main = "Patients who died\n from Melanoma", ylab = "Melanoma Thickness")
qqPlot(group_2$thickness, main = "Patients who survived\n Melanoma and are alive", 
       col.lines = "green")
qqPlot(group_3$thickness, main = "Patients who survived Melanoma\n but died from unrelated reasons", 
       col.lines = "red")
# Boxplot of distribution among the groups
boxplot(melanoma$thickness ~ melanoma$status, 
        main = "Thickness of melanoma among\n different groups of patients", 
        names = c("Died from\n Melanoma","Still Alive","Died from\n unrelated reason")) # boxplot
# Density Plot
melanoma$status <- as.factor(melanoma$status)
melanoma %>% 
  ggplot(., aes(x=thickness, fill=status)) +
  theme(legend.position = "bottom") +
  geom_density(alpha=0.4) +
  ggtitle("Melanoma Thickness According to\n Patient Status") +
  scale_fill_discrete(name = "Status:", labels=c("Died From Melanoma","Still Alive","Died other\n reasons")) 
# Shapiro Test
shapiro.test(group_1$thickness) # not normal
shapiro.test(group_2$thickness) # not normal
shapiro.test(group_3$thickness) # not normal
# -------------------
# Performing Kruskal-Wallis test
# Hypothesis:
# H0 = no difference on the mediam ranks 
k_test <- kruskal.test(thickness ~ status, data = melanoma)
summary(k_test)
# p-value = 3.846e-07 < alpha 0.05 - There is a difference between groups
# Reject Null Hypothesis (H0), at least one of the groups are significant different
# formal report kw: x^2(2)=40.6, p<0.001
# N = number of individuals, K = number of groups

# -------------------
# Performing Post-Hoc test
dunn.test(melanoma$thickness, g=melanoma$status, method = "bh")
# post hoc test shows that a difference exists between groups 1 and 2, that means between the 
# one who died from melanoma and the one who survived and are still alive.

# -------------------
## Multiple Linear Regression

# MAYBE PUT OFF THE BOOLEAN STUFF
# Question: predict thickness of the melanona based on it's correlation whithin other variables
# H0 = no variables are significant for the model
# H1 = at least one variable is signifcant
melanoma$ulcer = as.numeric(melanoma$ulcer)
melanoma$status = as.numeric(melanoma$status)
cor(melanoma)
mel_model <- lm(thickness ~ ulcer + age, data= melanoma)
mel_model
summary(mel_model)

# Normality of the model
qqnorm(residuals(mel_model));qqline(residuals(mel_model))
plot(mel_model_res, col="green");abline(0,0)
#formal report F(3, 201) = 18.16, p<0.001, R^2=0.20
#variables ulcer=1 (yes for ulcer), and age were statistically significant (p<0.05) for the regression 

# Two Way Anova
# -------------------
# H1: there's no difference in the means of "factor 1"
# H2: there's no difference in the means of "factor 2"
# H3: there's no interaction between "factor 1" and "factor 2"
# Verify the number of factors
attach(melanoma)
ulcer <- as.factor(ulcer)
status <- as.factor(status)
levels(ulcer) # returns 1 and 0 
levels(status) # returns 1, 2 and 3

# Have a look at the 3x2 design
boxplot(thickness ~ ulcer,
        main = "Melanoma Thickness by\n Ulceration of tumor",
        names= c("Not present","Present"))
boxplot(thickness ~ status, 
        main = "Thickness of melanoma among\n different groups of patients", 
        names = c("Died from\n Melanoma","Still Alive","Died from\n unrelated reason"))
# Two way ANOVA 
m1 = aov(thickness ~ ulcer + status + ulcer:status)
summary(m1)
plot(m1)
# Post-hoc for significant differences between n>2 groups
ph_status = TukeyHSD(m1, "status")
ph_status
plot(ph_status)

# Power test - not included on final report

# Power for 'ulcer' factor. Levels: 0 (No ulcer), 1 (ulcer)
library(effsize)
melanoma$ulcer = as.numeric(melanoma$ulcer)
y_ulc = melanoma %>% filter(ulcer==1)
n_ulc = melanoma %>% filter(ulcer==0)
cohen.d(y_ulc$thickness, n_ulc$thickness) # d=0.94
plot(pwr.t.test(d=0.94, power = 0.95, sig.level = 0.001))

# Power for 'status' factor. Levels: 1 (died from mel.), 2 (survived and alive), 3 (died from other reasons)
cohen.(group_1$thickness, group_2$thickness, group_3$thickness)
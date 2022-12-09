# Lam Lay
# Heart Attack Analysis & Prediction Dataset
# Final code

# ==================================================
# Data Cleaning
rm(list = ls())  # remove all variables stored previously

library(Hmisc)

heart = read.csv('heart.csv')  # import data
nrow(heart)                    # number of rows 
summary(heart)

str(heart)          # type/class of each variable

colSums(is.na(heart))  # No missing values

heart2 = unique(heart) # Removing any duplicate rows => it seems like there's no duplicate rows in the original dataset

colSums(heart==0)   # trtbps and chol do not have any zero values => good

# ========================================================
# Graphical Presentation
attach(heart)

# Fig.1: Correlation plots
library(corrplot)
library(RColorBrewer)
M = cor(heart)
corrplot(M, method = 'shade', diag = FALSE) # colorful number

# Fig.2: Histogram of chest pain
table(heart$output, heart$cp)        # Contingency table for risk vs chest pain
dat_cp = read.table(text ="typical_angina atypical_angina non-anginal_pain asymptomatic
High_Risk 39 41 69 16  
Low_Risk 104 9 18 7 ", header= TRUE)
barplot(as.matrix(dat_cp),col=c("red","green"),legend = TRUE, 
        main = "Chest pain vs Risk", xlab = "Types of Chest pain", ylab = "Counts",)

# Fig.3: Density plot of cholesterol
HighRisk_dat = subset(heart, output == 1)
LowRisk_dat = subset(heart, output == 0)

library(ggplot2)
ggplot() +
  geom_density(aes(chol, fill = 'High Risk'), alpha = .2, data = HighRisk_dat) +
  geom_density(aes(chol, fill = 'Low Risk'), alpha = .2, data = LowRisk_dat) +
  scale_fill_manual(name = "output", values = c("red", "green"))

# Fig.4: Histogram of gender
table(heart$output, heart$sex)       # Contingency table for risk vs sex
dat_sex = read.table(text ="Male Female
High_Risk 93 72  
Low_Risk 114 24 ", header= TRUE)
barplot(as.matrix(dat_sex),col=c("red","green"),legend = TRUE, 
        main = "Gender vs Risk", xlab = "Gender", ylab = "Counts",
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

# Fig.5:  Histogram of caa
table(heart$output, heart$caa)       # Contingency table for risk vs caa
dat_caa= read.table(text ="Zero One Two Three Four
High_Risk 130 21 7 3 4 
Low_Risk 45 44 31 17 1 ", header= TRUE)
barplot(as.matrix(dat_caa),col=c("red","green"),legend.text = TRUE, args.legend = list(x = "topright"),
        main = "Number of major vessels vs Risk", xlab = "Number of major vessels ", ylab = "Counts",
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

# ============================================================
# presence of chest pain vs risks
table(heart$output, heart$cp)       # Contingency table for risk vs chest pain

library(gmodels)                               # calculate chi-square
CrossTable(heart$output, heart$cp)
total_chi_squared = 23.200+8.329+11.801+1.153+19.404+6.966+9.870+0.964
total_chi_squared
1 - pchisq(total_chi_squared, df = 1, lower.tail=TRUE)

chisq.test(heart$output, heart$cp)

# ===============================================================
# Cholesterol vs output
boxplot(heart$chol, ylab = "chol")
out <- boxplot.stats(heart$chol)$out
out_ind <- which(heart$chol %in% c(out))
out_ind
heart_noOutliers = heart[-c(out_ind),]

# check normality (Goodness of fit test)
ks.test(heart$chol, 'pnorm', mean(heart$chol), sd(heart$chol))     # Kolmogorov–Smirnov test

# Check the variances
var.test(chol ~ output, heart, alternative = "two.sided")

# Because the variances are equal, we use pooled t-test. 
t.test(heart$chol ~ heart$output, var.equal=TRUE, alternative="greater")

# ===========================================================
# Females present in this data set appear to be at a higher risk of heart attack than males. 
table(heart$output, heart$sex)       # Contingency table for risk vs gender

n_m = 114 + 93 
n_f = 24 + 72 
p_m = 93/n_m
p_f = 72/n_f

SE=sqrt(p_m*(1-p_m)/n_m+p_f*(1-p_f)/n_f)
Z=(p_f-p_m)/SE

p = 1-pnorm(Z)            # one-sided test

# ================================================================
# caa vs output
table(heart$output, heart$caa)       # Contingency table for risk vs caa

chisq.test(heart$output, heart$caa)

library(gmodels)
CrossTable(heart$output, heart$caa)
total_chi_squared2 = 15.110+7.001+10.834+6.836+0.716+ 12.637+5.855+9.061+5.717+0.599
total_chi_squared2
1 - pchisq(total_chi_squared2, df = 1, lower.tail=TRUE)
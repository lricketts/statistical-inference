setwd("C:/Program Files/R/Coursera/statinfer/statinfer_courseproject")

library(datasets)
library(dplyr)
library(ggplot2)

# Load ToothGrowth data: The Effect of Vitamin C on Tooth Growth in Guinea Pigs
# Variable List:
# len = Tooth length
# supp = Supplement type (VC = ascorbic acid, OJ = orange juice)
# dose = Dose in milligrams/day
data(ToothGrowth)
head(ToothGrowth)

# Calculate summary statistics for tooth length, by supplement and dosage
summarize(group_by(ToothGrowth, supp, dose), mean = mean(len), sd = sd(len),
          min = min(len), max = max(len))

# Plot the distribution of tooth length, by supplement and dosage
# It appears that OJ has greater tooth lengths across lower dosages, and similar
# lengths at the highest dosage
ggplot(ToothGrowth, aes(len)) + 
        geom_density(fill = "lightblue") + 
        facet_wrap(~supp + dose) + 
        ggtitle("Figure 1: Tooth Length Distribution, By Supplement & Dosage") +
        labs(x = "Tooth Length", y = "Density")

# Test difference in supplement type, was orange juice more effective than
# ascorbic acid? H0 = There was no difference in mean tooth length between VC 
# and OJ.
mu_oj <- mean(ToothGrowth$len[ToothGrowth$supp == "OJ"])
mu_vc <- mean(ToothGrowth$len[ToothGrowth$supp == "VC"])
sd_oj <- sd(ToothGrowth$len[ToothGrowth$supp == "OJ"])
sd_vc <- sd(ToothGrowth$len[ToothGrowth$supp == "VC"])
n_oj <- length(ToothGrowth$len[ToothGrowth$supp == "OJ"])
n_vc <- length(ToothGrowth$len[ToothGrowth$supp == "VC"])

# Pooled variance estimate
sp <- sqrt( ((29 * (sd_oj ^ 2)) + 29 * (sd_vc ^ 2)) / 
        (n_oj + n_vc -2))

# 95 % confidence interval for the difference in supplement group means
(mu_oj - mu_vc) + c(-1,1) * qt(.975, 58) * sp * (1/30 + 1/30) ^ .5

# Using a 90% confidence interval
(mu_oj - mu_vc) + c(-1,1) * qt(.95, 58) * sp * (1/30 + 1/30) ^ .5

t.test(ToothGrowth$len ~ ToothGrowth$supp, paired = FALSE)

# Does the difference between OJ and VC change by dose?
# Low Dose: Difference in mean highly significiant
lowdose <- filter(ToothGrowth, dose == 0.5)
t.test(lowdose$len ~ lowdose$supp, paired = FALSE)
low <- t.test(lowdose$len ~ lowdose$supp, paired = FALSE)
low
ci1 <- low$conf.int[1:2]
mu1 <- low$estimate[1] - low$estimate[2]

# Moderate Dose: Difference in mean highly significiant
meddose <- filter(ToothGrowth, dose == 1)
t.test(meddose$len ~ meddose$supp, paired = FALSE)
med <- t.test(meddose$len ~ meddose$supp, paired = FALSE)
med
ci2 <- med$conf.int[1:2]
mu2 <- med$estimate[1] - med$estimate[2]

# High Dose: Null is not rejected, CI is very wide
highdose <- filter(ToothGrowth, dose == 2)
t.test(highdose$len ~ highdose$supp, paired = FALSE)
high <- t.test(highdose$len ~ highdose$supp, paired = FALSE)
high
ci3 <- high$conf.int[1:2]
mu3 <- high$estimate[1] - high$estimate[2]

ci_intervals <- cbind(rbind(ci1, ci2, ci3),
                           dose = c(0.5, 1, 2),
                        mean = c(mu1, mu2, mu3))
ci_intervals <- data.frame(ci_intervals)
ci_intervals <- rename(ci_intervals, lower = V1, higher = V2)

g <- ggplot(ci_intervals, aes(x = dose, y = mean)) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymax = higher, ymin = lower)) +
        geom_hline(yintercept = 0, size = 1) +
        labs(x = "Dose", y = "95% Confidence Interval") +
        ggtitle("Figure 2: Difference Between OJ and VC Group Means, By Dose")
g

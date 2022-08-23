#Run everything in the main .Rmd to set up the dataframes for these models. 
#Note: modelling takes a while. 

library(WARN)

kellis <- subset(egyptCN, siteName == "Kellis 1" | siteName == "Kellis 2" & sampleType == "Bone") %>% 
  filter(!is.na(d15N))

kellis2 <- subset(kellis, select = c(refIndividualInPubli, humanSex, humanAgeEstimAvg, d13C, d15N, humanAgeClass))

nonadult <- subset(kellis2, humanAgeEstimAvg <= 10)
#nonadult$humanAgeEstimAvg <- as.numeric(as.character(nonadult$humanAgeEstimAvg))
adult <- subset(kellis2, humanAgeClass == "Adult")
female <- subset(adult, adult$humanSex == "Female")
female.mean = mean(female$d15N)

warn.Kellis <- warn(
  age = nonadult$humanAgeEstimAvg, 
  d15N = nonadult$d15N, 
  female.mean = mean(female$d15N),
  female.sd = sd(female$d15N))

## Indicate summary.
summary(warn.Kellis)

#simple plot
plot(warn.Kellis,
     #  hline.adult = F,
     #   adult.mean = mean(adult$N),
     #  adult.sd = sd(adult$N),
     is.female = T, 
     ylim = c(15, 24),
     xlab = "Age (years)", 
     ylab = expression(paste(delta^15, "N"[collagen], " (\u2030, AIR)")),
     main = "Kellis")

Kellis.CI <- warnCI(warn.Kellis, 0.95)
plot(Kellis.CI, "age", 
     xlab = expression("t"[1]~"(years)"), 
     ylab = expression("t"[2]~"(years)"),
     main = "Tell Barri")
Kellis.CI

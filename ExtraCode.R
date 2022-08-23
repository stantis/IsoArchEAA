#Extra code I don't think has a place in the presentation. 


#Some basic sex-based comparisons. 
sexCN <- manova(cbind(d13C, d15N) ~ humanSex, data = subset(ane, !is.na(humanSex)))
summary(sexCN)

with(ane, shapiro.test(d13C[humanSex == "Male"]))
with(ane, shapiro.test(d13C[humanSex == "Female"]))

with(ane, shapiro.test(d15N[humanSex == "Male"]))
with(ane, shapiro.test(d15N[humanSex == "Female"]))

wilcox.test(subset(ane, humanSex == "Male")$d13C, subset(ane, humanSex == "Female")$d13C, paired= F)

ggplot(data = ane, aes(x = humanSex, y = d15N, color = country)) + 
  geom_jitter() + 
  scale_color_manual(values = custom.col) + 
  theme_classic()

ggplot(data = ane, aes(x = humanSex, y = d13C)) + 
  geom_boxplot(aes(fill = humanSex)) +
  #  geom_jitter() + 
  scale_color_manual(values = custom.col) + 
  scale_fill_manual(values = custom.col) +
  theme_classic()

sexCountry <- table(subset(ane, country != "Lebanon" & country != "Syria")$humanSex, subset(ane, country != "Lebanon" & country != "Syria")$country)

chisq.test(sexCountry)


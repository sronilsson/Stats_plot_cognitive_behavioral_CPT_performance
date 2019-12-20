library(lmerTest)
library(ggplot2)

#load RDA file
load("/Users/simon/Documents/NYU/0. Code/R/analyse ABET/campdenCPT/CPT.RData")
rm(Donepezil)
rm(FourE6)

#load Donepezil file
FourE6 = read.csv("FourE6.csv", header = TRUE)
FourE6$Dose <- as.factor(FourE6$Dose)

#ANALYSE DONEPEZIL CPT - TWO WAY MIXED MODEL ANOVA
anova4E6 <- anova(lmer(Efficency ~ Dose*Genotype + (1|Animal.ID), data=FourE6))
anova4E6

#LINE GRAPH
plot <- ggplot(FourE6, aes(x = Dose, y = Criterion, fill = Genotype, group = Genotype)) +
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2) +
  stat_summary(
    mapping = aes(colour = Genotype),
    geom = "line",
    fun.y = mean,
    show.legend = FALSE
  )
plot <- plot + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + theme_light()
plot <- plot + theme(legend.background = element_rect(fill="white", size=0.8, linetype="solid", colour = "darkblue"), legend.position="top", legend.title = element_blank())
plot


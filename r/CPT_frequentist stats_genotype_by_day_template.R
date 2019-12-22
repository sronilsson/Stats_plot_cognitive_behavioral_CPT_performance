library(lmerTest)
library(ggplot2)
library(reshape2)
library(Cairo)
library(officer)

#load RDA file
load("/Users/simon/Documents/NYU/0. Code/R/analyse ABET/campdenCPT/CPT.RData")


#SELECT DATA AND CAST INTO CORRECT FORMAT
DprimeData <- dplyr::select(BaselineCPT, Animal.ID, Session, Dprime, Genotype)
DprimeData <- dcast(DprimeData, Animal.ID+Genotype~Session, value.var="Dprime")
colnames(DprimeData) <- c("Animal.ID", "Genotype", "Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7", "Day8", "Day9", "Day10","Day11", "Day12")

#PUT DATA IN BINS, TRANSFER TO NEW DATAFRAME, AND MELT
DprimeData$Bin1 <- rowMeans(subset(DprimeData, select = c(Day1, Day2, Day3)), na.rm = TRUE)
DprimeData$Bin2 <- rowMeans(subset(DprimeData, select = c(Day4, Day5, Day6)), na.rm = TRUE)
DprimeData$Bin3 <- rowMeans(subset(DprimeData, select = c(Day7, Day8, Day9)), na.rm = TRUE)
DprimeData$Bin4 <- rowMeans(subset(DprimeData, select = c(Day10, Day11, Day12)), na.rm = TRUE)
DprimeFrame <- dplyr::select(DprimeData, Animal.ID, Bin1, Bin2, Bin3, Bin4, Genotype)
DprimeFrame <- melt(DprimeFrame, id=c("Animal.ID", "Genotype"))
names(DprimeFrame)[names(DprimeFrame) == "value"] <- 'Dprime'
names(DprimeFrame)[names(DprimeFrame) == "variable"] <- 'Bin'
DprimeFrame$Bin <- as.factor(DprimeFrame$Bin)
DprimeFrame$Genotype <- as.factor(DprimeFrame$Genotype)

#ANALYSE BASELINE CPT - TWO WAY MIXED MODEL ANOVA
anovaBaselineCPT <- anova(lmer(Dprime ~ Bin*Genotype + (1|Animal.ID), data=DprimeFrame))
anovaBaselineCPT

#LINE GRAPH
cairo_ps(filename='Dprime.emf', width=7, height=7)
plot <- ggplot(DprimeFrame, aes(x = Bin, y = Dprime, fill = Genotype, group = Genotype)) +
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

dev.off()
plot

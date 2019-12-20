library(ggplot2)
library(dplyr)
library(gplots)
library(reshape2)
library(plyr)
library(nlme)

#READ IN CSV SHEET
dataTable <- read.csv('/Users/simon/Documents/NYU/NYU/0. Code/R/line graph R - FIBER PHOTOMETRY/Outfile2.csv', sep = "\t", header=TRUE)
scaleFUN <- function(x) sprintf("%.2f", x)

#MELT DATA
dataTable$X1 <- 0
dataTable2 <- melt(dataTable, id='OUTCOME')
dataTable2$value = (as.numeric(as.character(dataTable2$value)))
dataTable2$value = dataTable2$value * 100

#PLOT
plot <- ggplot(dataTable2, aes(x = variable, y = value, fill = OUTCOME, group = OUTCOME)) +
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2) +
  stat_summary(
    mapping = aes(colour = OUTCOME),
    geom = "line",
    fun.y = mean,
    show.legend = FALSE
  )

#PLOT AESTETICS
plot <- plot + theme_light()
plot <- plot + theme(axis.text.x = element_text(angle=90, size=12))
plot <- plot + theme(axis.text.y = element_text(size=12))

plot <- plot + labs(y = "ΔF/F") + labs(x = "time bin (0.2s)", size = 1.9)
plot <- plot +  scale_x_discrete(labels = c('-4','-3','-2','-1', 'Stimulus', '+1', '+2', '+3', '+4', '+5', '+6', '+7', '+8', '+9', '+10', '+11', '+12', '+13', '+14', '+15', '+16'))
#plot <- plot + annotate("rect", xmin=5, xmax=6, ymin=-0.01, ymax=0.02, alpha=0.1, fill="black")
plot <- plot + theme(axis.text.x = element_text(vjust = 0.5))
plot <- plot + theme(legend.position="top")
plot <- plot + theme(legend.title = element_blank())
plot <- plot + theme(legend.background = element_rect(fill="white", size=0.8, linetype="solid", colour = "darkblue"))
plot <- plot + theme(legend.spacing.x = unit(0.2, 'cm'))
plot <- plot + theme(axis.title=element_text(size=14,face="bold"))
plot <- plot + scale_y_continuous(labels=scaleFUN)

#PRINT AND SAVE PLOT
print(plot)
ggsave(filename="misses2.png", plot=plot)

# Two-way ANOVA
model = lm(value ~ variable + OUTCOME + variable:OUTCOME, data=dataTable2)
anova(model)

#  COLLAPSED Non-respond vs. respond
responseFactor <- factor(dataTable2$OUTCOME)
levels(responseFactor) <- list(Response=c("HIT", "FALSE ALARM"), Non.response=c("CORRECT REJECTION", "MISS"))
dataTable2$RESPONSE.VAR <- responseFactor

# Welch t-test on final pre-Stim group Non-respond vs. respond
newdata<- dataTable2[dataTable2$variable == 'X4',] 
print(newdata)
ttest = t.test(value ~ RESPONSE.VAR, newdata)
ttest

library(ggplot2)
library(gplots)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(lattice)
library(reshape2)


#READ IN CSV SHEET
dataTable <- read.csv('/Users/simon/Documents/NYU/0. Code/R/R_rastaplot/Book2.csv', header=TRUE, sep = ",")

#CREATE FOUR TABLES
cr_trials <- subset(dataTable, OUTCOME == 'CORRECT REJECTION')
hit_trials <- subset(dataTable, OUTCOME == 'HIT')
miss_trials <- subset(dataTable, OUTCOME == 'MISS')
FA_trials <- subset(dataTable, OUTCOME == 'FALSE ALARM')
cr_trials$OUTCOME <- NULL
hit_trials$OUTCOME <- NULL
miss_trials$OUTCOME <- NULL
FA_trials$OUTCOME <- NULL
cr.matrix <- data.matrix(cr_trials)
hit.matrix <- data.matrix(hit_trials)
FA.matrix <- data.matrix(FA_trials)
miss.matrix <- data.matrix(miss_trials)

# CORRECT REJECTIONS - 'GGPLOT'
rnames <- rownames(cr_trials)
cr_trials <- cbind(rnames,data.frame(cr_trials))
cr_trials.matrix <- melt(cr_trials)
cr_trials.matrix$rnames = as.numeric(as.character(cr_trials.matrix$rnames))
plot <- ggplot(data = cr_trials.matrix, aes(variable, rnames)) + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(colours = c("blue", "white", "red"), limits=c(0.950,1.050), values = scales::rescale(c(-0.5, -0.15, 0, 0.3, 0.5))) + ylab("trial") + xlab("time bin (0.2s)")
plot <- plot + theme(axis.text.x = element_text(angle=90, size=18), axis.text.y = element_blank())
plot <- plot + guides(fill = guide_colourbar(label.theme = element_text(angle = 0)))
plot <- plot + guides(fill = guide_colourbar(label.theme = element_text(angle = 0, size = 25)))
plot <- plot + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))
plot <- plot + theme(axis.title.x = element_text(size = rel(2.5)))
plot <- plot +  scale_x_discrete(labels = c('-4','-3','-2','-1', 'Stimulus', '+1', '+2', '+3', '+4', '+5', '+6', '+7', '+8', '+9', '+10', '+11', '+12', '+13', '+14', '+15', '+16'))
ggsave(filename="CR.png", plot=plot)
plot

# HITS - 'GGPLOT'
rnames <- rownames(hit_trials)
hit_trials <- cbind(rnames,data.frame(hit_trials))
hit_trials.matrix <- melt(hit_trials)
hit_trials.matrix$rnames = as.numeric(as.character(hit_trials.matrix$rnames))
plot <- ggplot(data = hit_trials.matrix, aes(variable, rnames)) + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(colours = c("blue", "white", "red"), limits=c(0.950,1.050), values = scales::rescale(c(-0.5, -0.15, 0, 0.3, 0.5))) + ylab("trial") + xlab("time bin (0.2s)")
plot <- plot +theme(axis.text.x = element_text(angle=90, size=18), axis.text.y = element_blank())
plot <- plot + guides(fill = guide_colourbar(label.theme = element_text(angle = 0)))
plot <- plot + guides(fill = guide_colourbar(label.theme = element_text(angle = 0, size = 25)))
plot <- plot + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))
plot <- plot + theme(axis.title.x = element_text(size = rel(2.5)))
plot <- plot +  scale_x_discrete(labels = c('-4','-3','-2','-1', 'Stimulus', '+1', '+2', '+3', '+4', '+5', '+6', '+7', '+8', '+9', '+10', '+11', '+12', '+13', '+14', '+15', '+16'))
ggsave(filename="hits.png", plot=plot)


# MISSES - 'GGPLOT'
rnames <- rownames(miss_trials)
miss_trials <- cbind(rnames,data.frame(miss_trials))
miss_trials.matrix <- melt(miss_trials)
miss_trials.matrix$rnames = as.numeric(as.character(miss_trials.matrix$rnames))
plot <- ggplot(data = miss_trials.matrix, aes(variable, rnames)) + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(colours = c("blue", "white", "red"), limits=c(0.950,1.050), values = scales::rescale(c(-0.5, -0.15, 0, 0.3, 0.5))) + ylab("trial") + xlab("time bin (0.2s)")
plot <- plot+theme(axis.text.x = element_text(angle=90, size=18), axis.text.y = element_blank())
plot <- plot + guides(fill = guide_colourbar(label.theme = element_text(angle = 0)))
plot <- plot + guides(fill = guide_colourbar(label.theme = element_text(angle = 0, size = 25)))
plot <- plot + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))
plot <- plot + theme(axis.title.x = element_text(size = rel(2.5)))
plot <- plot +  scale_x_discrete(labels = c('-4','-3','-2','-1', 'Stimulus', '+1', '+2', '+3', '+4', '+5', '+6', '+7', '+8', '+9', '+10', '+11', '+12', '+13', '+14', '+15', '+16'))
ggsave(filename="misses.png", plot=plot)
plot


# FALSE ALARMS - 'GGPLOT'
rnames <- rownames(FA_trials)
FA_trials <- cbind(rnames,data.frame(FA_trials))
FA_trials.matrix <- melt(FA_trials)
FA_trials.matrix$rnames = as.numeric(as.character(FA_trials.matrix$rnames))
plot <- ggplot(data = FA_trials.matrix, aes(variable, rnames)) + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(colours = c("blue", "white", "red"), limits=c(0.950,1.050), values = scales::rescale(c(-0.5, -0.15, 0, 0.3, 0.5))) + ylab("trial") + xlab("time bin (0.2s)")
plot <- plot+theme(axis.text.x = element_text(angle=90, size=18), axis.text.y = element_blank())
plot <- plot + guides(fill = guide_colourbar(label.theme = element_text(angle = 0, size = 25)))
plot <- plot + theme(axis.title.y = element_text(size = rel(2.5), angle = 90))
plot <- plot + theme(axis.title.x = element_text(size = rel(2.5)))
plot <- plot +  scale_x_discrete(labels = c('-4','-3','-2','-1', 'Stimulus', '+1', '+2', '+3', '+4', '+5', '+6', '+7', '+8', '+9', '+10', '+11', '+12', '+13', '+14', '+15', '+16'))
ggsave(filename="FA.png", plot=plot)
plot

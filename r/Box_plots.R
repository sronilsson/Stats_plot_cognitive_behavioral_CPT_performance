library(openxlsx)
library(ggplot2)
library(Rmisc)
library(RColorBrewer)
library(ggsignif)
library(ggpubr)

#READ IN CSV SHEET
dataTable <- read.xlsx("/Users/simon/Documents/NYU/NYU/0. Code/R/Graphs - ABET/MeanCompiled.xlsx", sheet = 'Summary')

#READ IN CSV SHEET AND SET SEM
dataTable$GROUP <- factor(dataTable$GROUP)

# FUNCTION FOR PLOTTING MANY ERROR BAR GRAPHS
create_graph <- function(columnName) {

  plot <- ggboxplot(dataTable, x = "GROUP", y = columnName,
                 color = "GROUP", palette = "jco",
                 add = "jitter")
  #plot <- plot + stat_compare_means(method = "t.test")
  plot <- plot + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour = "darkblue"))
  plot <- plot + theme(legend.title = element_blank())
  plot <- plot + stat_compare_means( aes(label = ..p.signif..), label.x = 1.5, size = 14, vjust = 1)
  
  
  # SAVE PLOT
  print(plot)
  fileName <- gsub("[.]", "_", columnName)
  fileName <- paste(fileName, ".png", sep = '')
  print(fileName)
  ggsave(filename=fileName, plot=plot)
}

#SET SHEET NAME AND RUN IT THROUGH FUNCTION
headerList <- list('Dprime')
for (i in headerList){
  columnName <- i
  create_graph(columnName)
}

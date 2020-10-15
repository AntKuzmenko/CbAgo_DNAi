##########################################################################################
# Written by Anastasiya Oguienko, PhD student, Caltech.
# This script is under GPLv3 licensing
##########################################################################################
# The script contains the code used to calculate AT and GC percentage for paper  published in Nature XXX

# Load/install packages for data analysis and visualization
library(ggplot2)

# reading .txt files with nucleotide frequencies calculated based on the results of WebLogo tool on Galaxy platform
# adjust 'skip' argument for your file

Galaxy_table <- read.delim('path_to_the_file/file_name.txt',
                           stringsAsFactors = F,
                           skip = 7)

# calculating AT and GC percentage
Galaxy_table$ATpercent <- rowSums(Galaxy_table[ , c(2, 5)])/rowSums(Galaxy_table[,2:5])
Galaxy_table$GCpercent <- rowSums(Galaxy_table[ , c(3, 4)])/rowSums(Galaxy_table[,2:5])

# delete the last row if it contains string (use your row number instead of '46')
Galaxy_table <- Galaxy_table[-c(46), ]

# rewrite the first column as integers; use your number instead of '45'
Galaxy_table$`X.` <- seq(1, 45)

# making a plot
both <- ggplot(data = Galaxy_table, aes(X., ATpercent))+
  geom_path(size=0.25, colour = "#F68C1F")+
  geom_point(aes(X., ATpercent), size=0.5, colour = "#F68C1F")+
  geom_path(aes(X., GCpercent), size=0.25, colour="#3953A4")+
  geom_point(aes(X., GCpercent), size=0.5, colour="#3953A4")+
  theme_classic()+
  theme(axis.ticks.length = unit(0.0744, "cm"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title=element_text(size=8))+
  scale_y_continuous(name=NULL, breaks=seq(0, 1, by=0.1), limits=c(0.25, 0.75), )+
  scale_x_continuous(name=NULL, breaks=seq(0, 45, by=1), labels=seq(-15, 30, by=1))+
  theme(axis.text = element_text(size = 8, colour = "black"), 
        axis.ticks = element_line(colour = "black", size = 0.25),
        axis.line = element_line(colour = 'black', size = 0.25))

ggsave('path_to_the_file/file_name.pdf',
       plot = both, height = 1, width = 3.319, useDingbats=F)

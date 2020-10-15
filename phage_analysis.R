##########################################################################################
# Written by Anastasiya Oguienko, PhD student, Caltech.
# This script is under GPLv3 licensing
##########################################################################################
# The script contains the code used to analyse phage experiments data deposited at published in Nature XXX

# Load/install packages for data analysis and visualization

if (!require(ggplot2)) {install.packages("ggplot2")}
if (!require(data.table)) {install.packages("data.table")}
if (!require(readxl)) {install.packages("readxl")}
if (!require(DescTools)) {install.packages("DescTools")}
if (!require(dplyr)) {install.packages("dplyr")}


library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)
library(DescTools)

# reading file with measurments - it has also columns 'mean' and 'SD' (standard deviation); 
#NOT tidy data table; input is .xlsx file; columns are: hours, Ago, rep1, rep2, rep3, rep4, rep...

phage_dataframe <- read_excel("path_to_the_file/file.xlsx")

# assign 'Ago' and 'hours' variables the status of factors
phage_dataframe$Ago <- as.factor(phage_dataframe$Ago)
phage_dataframe$hours <- as.factor(phage_dataframe$hours)

#implement if there are different MOI values in the table 
phage_dataframe$MOI <- as.factor(phage_dataframe$MOI)


#calculate mean and SD for each type of experiment
phage_dataframe$mean <- apply(phage_dataframe[,4:9], MARGIN = 1, FUN = mean, na.rm = T)
phage_dataframe$SD <- apply(phage_dataframe[,4:9], MARGIN = 1, FUN = sd, na.rm = T)


# calculating mean+SD and mean-SD for convenient plotting of errorbars
phage_dataframe$`mean-SD` <- phage_dataframe$mean - phage_dataframe$SD
phage_dataframe$`mean+SD` <- phage_dataframe$mean + phage_dataframe$SD

#since tha plot will be in log-scale, we need to replace all non-positive values with 0.1
phage_dataframe$`mean-SD` <- sapply(phage_dataframe$`mean-SD`, function(x) ifelse(x < 0, 0.1, x))

#plot data
plot_phage <- ggplot(phage_dataframe, aes(hours, mean, fill=Ago)) +
  geom_bar(stat="identity", 
           color="black", 
           size = 3,
           position=position_dodge2(0.9, preserve = "single", padding=0))+
  geom_errorbar(aes(ymin = `mean-SD`, ymax = `mean+SD`), 
                size = 3,
                width = 0.3, 
                position=position_dodge(width = 0.9, preserve='single'))+
  scale_fill_manual("Ago", values = c("Cb" = "#f4898b", 
                                      "dCb" = "#92c4ea", 
                                      "no Cb" = "#acd373"))+
  geom_point(aes(hours, rep1), 
             shape = 21, size = 10,
             colour = 'black', fill = 'white',
             stroke = 3, 
             position=position_dodge2(0.9, padding=0))+
  geom_point(aes(hours, rep2), 
             shape = 21, size = 10,
             colour = 'black', fill = 'white', 
             stroke = 3, 
             position=position_dodge2(0.9, padding=0))+
  geom_point(aes(hours, rep3), 
             shape = 21, size = 10,
             colour = 'black', fill = 'white', 
             stroke = 3, 
             position=position_dodge2(0.9, padding=0))+
  geom_point(aes(hours, rep4), 
             shape = 21, size = 10,
             colour = 'black', fill = 'white', 
             stroke = 3, 
             position=position_dodge2(0.9, padding=0))+
  # number of geom_points should be the same as number of replicas
  theme_classic()+
  theme(axis.text = element_text(size = 50), 
        axis.title = element_text(size = 50),
        axis.ticks.length = unit(0.3, 'in'),
        axis.ticks = element_line(size = 3),
        axis.line = element_line(size = 3),
        legend.position = 'top',
        legend.justification='left',
        legend.direction = 'horizontal',
        legend.key.size = unit(0.7, 'in'),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 50),
        strip.background = element_rect(size = 0, colour = NULL),
        strip.text = element_text(size = 50, hjust = 0))+
  scale_y_continuous(name=NULL, trans='log10')+
  coord_cartesian(ylim=c(1e4, 1e10))+
  facet_wrap(~MOI, scales = "free", ncol=3)

plot(plot_phage)

#save the plot in pdf to desired folder with customized height and width
ggsave(plot = plot_phage, height = 15.625, width = 49, useDingbats = F, 
       filename = "path_to_the_file/file_name.pdf")

#statistical analysis of the data
#for statistical analysis log-transformation of data was performed
#as an input please use tidy (!) dataframe with log-transfromed values

phage_tidy_dataframe_log <- read_excel("path_to_the_file/file_name.xlsx")

# assign 'Ago' and 'hours' variables the status of factors
phage_tidy_dataframe_log$Ago <- as.factor(phage_tidy_dataframe_log$Ago)
phage_tidy_dataframe_log$hours <- as.factor(phage_tidy_dataframe_log$hours)

#implement if there are different MOI values in the table 
phage_tidy_dataframe_log$MOI <- as.factor(phage_tidy_dataframe_log$MOI)

#creating list of subsets of data corresponding to each set of conditions within which we want to carry statistical analysis
# as an example here: MOI = 0.1 / 1 / 5 and hours = 160 / 220 / 340
# if the initial data do not contain MOI, use only hours
phage_list <- list(phage_log_1_160 <- subset(phage_tidy_dataframe_log, MOI == 1 & hours == 160),
     phage_log_1_340 <- subset(phage_tidy_dataframe_log, MOI == 1 & hours == 340),
     phage_log_1_220 <- subset(phage_tidy_dataframe_log, MOI == 1 & hours == 220),
     phage_log_01_160 <- subset(phage_tidy_dataframe_log, MOI == 0.1 & hours == 160),
     phage_log_01_220 <- subset(phage_tidy_dataframe_log, MOI == 0.1 & hours == 220),
     phage_log_01_340 <- subset(phage_tidy_dataframe_log, MOI == 0.1 & hours == 340),
     phage_log_5_160 <- subset(phage_tidy_dataframe_log, MOI == 5 & hours == 160),
     phage_log_5_340 <- subset(phage_tidy_dataframe_log, MOI == 5 & hours == 340),
     phage_log_5_220 <- subset(phage_tidy_dataframe_log, MOI == 5 & hours == 220))


#statistical analysis

StatAnalysis <- function(df) {
  #This function does the following:
  # 1. Fits an Analysis of Variance Model
  # 2. Creates ANOVA table with Df, Mean Sq, p-value, etc.
  # 3. Carries Scheffe Test for multiple comparisons of means
  # 4. Writes the results of the above steps to a list indicating first experiment conditions (MOI and/or hours)
  # As input uses dataframe
  aov_df <- aov(Value ~ Ago, df)
  aov_df_sum <- summary.aov(aov_df)
  scheffe_test_res <- ScheffeTest(aov_df)
  return(list(df[1,1:2], aov_df, aov_df_sum, scheffe_test_res))
}

# applying StatAnalysis function to the list of subsets
# the result is a list of lists with calculated statistical data for each subset
phage_stat_data <- lapply(phage_list, StatAnalysis)



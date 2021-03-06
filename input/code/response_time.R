#############################################################################################
###################################### ANGER PROJECT ########################################
###################################### RESPONSE TIME #######################################
#############################################################################################
# 1. calculate mean values of acceptance/default allocation
# 2. calculate pvalue for difference in means
# 3. create & save jpeg graphs
#############################################################################################

library(dplyr)
library(ggplot2)
library(magrittr)
library(readr)
library(ggpubr)
library(grid)
library(gridExtra)

## --- Clean workspace --- ##
rm(list = ls())

## --- Parse CLI --- ##
args <- commandArgs(trailingOnly = TRUE)
in_csv <- args[1]
out_path <- args[2]



#-- Create output folder --- #
if (!dir.exists("./output")) {
  dir.create("./output")
}
if (!dir.exists("./output/graphs")) {
  dir.create("./output/graphs")
}

# -- Check Files meet requirements -- #
checkCLI = function(in_cli, file_ending) {

   print(paste("Checking argument:", in_cli))

   if ( !endsWith(in_cli, file_ending)  && !dir.exists(in_cli) ){
     stop("Command Line Input fails type check.")
   }
 }


checkCLI(in_csv, ".csv")
checkCLI(out_path, ".csv")


print("All Command Line Inputs passed file ending check.")


## --- Read the data --- ##
# anger_df <- read.csv('output/data/cleaned_data.csv', header = TRUE)
anger_df <- read.csv(in_csv, header = TRUE)


## --- Calculate initial expectation --- ##

anger_df$expect <- NA
anger_df$expect[anger_df$player == 2 & anger_df$payoff_tr == 0 & is.na(anger_df$accept) == 0] <- (8*anger_df$belief + 2*(1- anger_df$belief)*anger_df$accept)[anger_df$player == 2 & anger_df$payoff_tr == 0 & is.na(anger_df$accept) == 0]
anger_df$expect[anger_df$player == 2 & anger_df$payoff_tr == 1 & is.na(anger_df$accept) == 0] <- (11*anger_df$belief + 2*(1- anger_df$belief)*anger_df$accept)[anger_df$player == 2 & anger_df$payoff_tr == 1 & is.na(anger_df$accept) == 0]

anger_df$frust[anger_df$player == 2 & anger_df$expect <2 & is.na(anger_df$accept) == 0] <- 0
anger_df$frust[anger_df$player == 2 & anger_df$expect >=2 & is.na(anger_df$accept) == 0] <- (anger_df$expect - 2)[anger_df$player == 2 & anger_df$expect >=2 & is.na(anger_df$accept) == 0]


## --- Graphs --- ##
# Eclude outliers - 5 sd far from the mean
outlier_min_b <- mean(anger_df$sec_accept, na.rm = TRUE)+ sd(anger_df$sec_accept, na.rm = TRUE)*5
paste('drop outlier:', sum(anger_df$sec_accept > outlier_min_b & is.na(anger_df$sec_accept) == 0))


# By method of play: red Direct, blu Strategy (blue accept, black reject)

# Frustration
d_accept <- ggplot(data=subset(anger_df, direct==1 & is.na(accept) == 0 & sec_accept <outlier_min_b) , mapping=aes(x=frust, y=sec_accept)) + geom_point(aes(colour = accept))  +
   guides(color=FALSE)  + labs(x = 'Frustration',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "red") + stat_cor(method = 'pearson')

dm_accept <- ggplot(data=subset(anger_df, direct==1 & is.na(accept) == 0& sec_accept <outlier_min_b & gender == 0) , mapping=aes(x=frust, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Frustration',   y = 'Response Time') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "red") + stat_cor(method = 'pearson')

df_accept <- ggplot(data=subset(anger_df, direct==1 & is.na(accept) == 0& sec_accept <outlier_min_b & gender  == 1) , mapping=aes(x=frust, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Frustration',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "red") + stat_cor(method = 'pearson')

s_accept <- ggplot(data=subset(anger_df, direct==0 & is.na(accept) == 0 & sec_accept <outlier_min_b) , mapping=aes(x=frust, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Frustration',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "blue") + stat_cor(method = 'pearson')

sm_accept <- ggplot(data=subset(anger_df, direct==0 & is.na(accept) == 0 & sec_accept <outlier_min_b & gender == 0) , mapping=aes(x=frust, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Frustration',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "blue") + stat_cor(method = 'pearson')

sf_accept <- ggplot(data=subset(anger_df, direct==0 & is.na(accept) == 0 & sec_accept <outlier_min_b & gender  == 1) , mapping=aes(x=frust, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Frustration',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "blue") + stat_cor(method = 'pearson')




direct <- grid.arrange(d_accept, dm_accept, df_accept,
             nrow = 1, left = textGrob('Direct', rot = 90, gp = gpar(fontface = "bold", cex = 1.25)))

s <- grid.arrange(s_accept, bottom = textGrob('All', hjust = 0, gp = gpar(fontface = "bold", cex = 1.25)))
sm <- grid.arrange(sm_accept, bottom = textGrob('Male', hjust = 0, gp = gpar(fontface = "bold", cex = 1.25)))
sf <- grid.arrange(sf_accept, bottom = textGrob('Female', hjust = 0, gp = gpar(fontface = "bold", cex = 1.25)))

strategy <- grid.arrange(s, sm, sf,
             nrow =1, left = textGrob('Strategy', rot = 90, gp = gpar(fontface = "bold", cex = 1.25)))

jpeg(paste(out_path, 'frustration', '.jpeg', sep=""), width = 1000, height = 600, units = "px", pointsize = 12,
     quality = 75)
grid.arrange(direct, strategy ,nrow=2,
             #top = textGrob('Effect of Initial Expectations', vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
             bottom = textGrob( "Blue = accept, Black = reject.", gp = gpar(fontface = 3, fontsize = 12),  hjust = 1,  x = 1))
dev.off()


# Expectation
d_accept <- ggplot(data=subset(anger_df, direct==1 & is.na(accept) == 0 & sec_accept <outlier_min_b) , mapping=aes(x=expect, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Initial Expectations',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "red") + stat_cor(method = 'pearson')

dm_accept <- ggplot(data=subset(anger_df, direct==1 & is.na(accept) == 0 & sec_accept <outlier_min_b & gender == 0) , mapping=aes(x=expect, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Initial Expectations',   y = 'Response Time') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "red") + stat_cor(method = 'pearson')

df_accept <- ggplot(data=subset(anger_df, direct==1 & is.na(accept) == 0 & sec_accept <outlier_min_b & gender  == 1) , mapping=aes(x=expect, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Initial Expectations',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "red") + stat_cor(method = 'pearson')

s_accept <- ggplot(data=subset(anger_df, direct==0 & is.na(accept) == 0 & sec_accept <outlier_min_b) , mapping=aes(x=expect, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Initial Expectations',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "blue") + stat_cor(method = 'pearson')

sm_accept <- ggplot(data=subset(anger_df, direct==0 & is.na(accept) == 0 & sec_accept <outlier_min_b & gender == 0) , mapping=aes(x=expect, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Initial Expectations',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "blue") + stat_cor(method = 'pearson')

sf_accept <- ggplot(data=subset(anger_df, direct==0 & is.na(accept) == 0 & sec_accept <outlier_min_b & gender  == 1) , mapping=aes(x=expect, y=sec_accept)) + geom_point(aes(colour = accept))  +
  guides(color=FALSE)  + labs(x = 'Initial Expectations',   y = 'RT (sec)') +
  geom_smooth( inherit.aes = TRUE, method='lm',col = "blue") + stat_cor(method = 'pearson')


direct <- grid.arrange(d_accept, dm_accept, df_accept,
                       nrow = 1, left = textGrob('Direct', rot = 90, gp = gpar(fontface = "bold", cex = 1.25)))

s <- grid.arrange(s_accept, bottom = textGrob('All', hjust = 0, gp = gpar(fontface = "bold", cex = 1.25)))
sm <- grid.arrange(sm_accept, bottom = textGrob('Male', hjust = 0, gp = gpar(fontface = "bold", cex = 1.25)))
sf <- grid.arrange(sf_accept, bottom = textGrob('Female', hjust = 0, gp = gpar(fontface = "bold", cex = 1.25)))

strategy <- grid.arrange(s, sm, sf,
                         nrow =1, left = textGrob('Strategy', rot = 90, gp = gpar(fontface = "bold", cex = 1.25)))

jpeg(paste(out_path,'expectations', '.jpeg', sep=""), width = 1000, height = 600, units = "px", pointsize = 12,
     quality = 75)
grid.arrange(direct, strategy ,nrow=2,
             #top = textGrob('Effect of Initial Expectations', vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
             bottom = textGrob( "Blue = accept, Black = reject.", gp = gpar(fontface = 3, fontsize = 12),  hjust = 1,  x = 1))
dev.off()

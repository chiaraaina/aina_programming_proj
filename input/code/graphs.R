#############################################################################################
###################################### ANGER PROJECT ########################################
###################################### GRAPHS w/ PVAL #######################################
#############################################################################################
# 1. calculate mean values of acceptance/default allocation
# 2. calculate pvalue for difference in means
# 3. create & save jpeg graphs
#############################################################################################
library(dplyr)
library(ggplot2)
library(readr)
library(magrittr)

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
#anger_df <- read.csv('output/data/cleaned_data.csv', header = TRUE)
anger_df <- read.csv(in_csv, header = TRUE)
name_treatment <- c('S1', 'S2', 'D1', 'D2')

#########################################################################################################
## ---  Mean acceptance by treatments--- ##
#########################################################################################################
accept_mean <- anger_df %>% filter (player == 2) %>% group_by(direct, payoff_tr) %>% summarize(m = mean(accept, na.rm = TRUE))
accept_mean <- pull(accept_mean[3])
m_accept_mean <- anger_df %>% filter (player == 2 & gender == 0) %>% group_by(direct, payoff_tr) %>% summarize(m = mean(accept, na.rm = TRUE))
m_accept_mean <- pull(m_accept_mean[3])
f_accept_mean <- anger_df %>% filter (player == 2 & gender == 1) %>% group_by(direct, payoff_tr) %>% summarize(m = mean(accept, na.rm = TRUE))
f_accept_mean <- pull(f_accept_mean[3])

## ---  Mean default allocation by treatments--- ##
offer_mean <- anger_df %>% filter (player == 1) %>% group_by(direct, payoff_tr) %>% summarize(m = mean(offer, na.rm = TRUE))
offer_mean <- pull(offer_mean[3])
m_offer_mean <- anger_df %>% filter (player == 1 & gender == 0) %>% group_by(direct, payoff_tr) %>% summarize(m = mean(offer, na.rm = TRUE))
m_offer_mean <- pull(m_offer_mean[3])
f_offer_mean <- anger_df %>% filter (player == 1 & gender == 1) %>% group_by(direct, payoff_tr) %>% summarize(m = mean(offer, na.rm = TRUE))
f_offer_mean <- pull(f_offer_mean[3])

#########################################################################################################
## ---  Wilcoxon Test --- ##
#########################################################################################################
# Acceptance - all
accept_s1 <- anger_df %>% filter(player == 2, direct ==0, payoff_tr == 0) %>% select(accept)
accept_s2 <- anger_df %>% filter(player == 2, direct ==0, payoff_tr == 1) %>% select(accept)
accept_d1 <- anger_df %>% filter(player == 2, direct ==1, payoff_tr == 0) %>% select(accept)
accept_d2 <- anger_df %>% filter(player == 2, direct ==1, payoff_tr == 1) %>% select(accept)
accept_s_pool <- anger_df %>% filter(player == 2, direct ==0) %>% select(accept)
accept_d_pool <- anger_df %>% filter(player == 2, direct ==1) %>% select(accept)

accept_s <- wilcox.test(pull(accept_s1), pull(accept_s2), exact = FALSE, na.rm = TRUE)
accept_d <- wilcox.test(pull(accept_d1), pull(accept_d2), exact = FALSE, na.rm = TRUE)
accept_across <- wilcox.test(pull(accept_s_pool), pull(accept_d_pool), exact = FALSE, na.rm = TRUE)

pval_accept <- c(accept_s[3], accept_d[3], accept_across[3])

# Acceptance - male
m_accept_s1 <- anger_df %>% filter(player == 2, direct ==0, payoff_tr == 0, gender == 0) %>% select(accept)
m_accept_s2 <- anger_df %>% filter(player == 2, direct ==0, payoff_tr == 1, gender == 0) %>% select(accept)
m_accept_d1 <- anger_df %>% filter(player == 2, direct ==1, payoff_tr == 0, gender == 0) %>% select(accept)
m_accept_d2 <- anger_df %>% filter(player == 2, direct ==1, payoff_tr == 1, gender == 0) %>% select(accept)
m_accept_s_pool <- anger_df %>% filter(player == 2, direct ==0, gender == 0) %>% select(accept)
m_accept_d_pool <- anger_df %>% filter(player == 2, direct ==1, gender == 0) %>% select(accept)

m_accept_s <- wilcox.test(pull(m_accept_s1), pull(m_accept_s2), exact = FALSE, na.rm = TRUE)
m_accept_d <- wilcox.test(pull(m_accept_d1), pull(m_accept_d2), exact = FALSE, na.rm = TRUE)
m_accept_across <- wilcox.test(pull(m_accept_s_pool), pull(m_accept_d_pool), exact = FALSE, na.rm = TRUE)

m_pval_accept <- c(m_accept_s[3], m_accept_d[3], m_accept_across[3])

# Acceptance - female
f_accept_s1 <- anger_df %>% filter(player == 2, direct ==0, payoff_tr == 0, gender == 1) %>% select(accept)
f_accept_s2 <- anger_df %>% filter(player == 2, direct ==0, payoff_tr == 1, gender == 1) %>% select(accept)
f_accept_d1 <- anger_df %>% filter(player == 2, direct ==1, payoff_tr == 0, gender == 1) %>% select(accept)
f_accept_d2 <- anger_df %>% filter(player == 2, direct ==1, payoff_tr == 1, gender == 1) %>% select(accept)
f_accept_s_pool <- anger_df %>% filter(player == 2, direct ==0, gender == 1) %>% select(accept)
f_accept_d_pool <- anger_df %>% filter(player == 2, direct ==1, gender == 1) %>% select(accept)

f_accept_s <- wilcox.test(pull(f_accept_s1), pull(f_accept_s2), exact = FALSE, na.rm = TRUE)
f_accept_d <- wilcox.test(pull(f_accept_d1), pull(f_accept_d2), exact = FALSE, na.rm = TRUE)
f_accept_across <- wilcox.test(pull(f_accept_s_pool), pull(f_accept_d_pool), exact = FALSE, na.rm = TRUE)

f_pval_accept <- c(f_accept_s[3], f_accept_d[3], f_accept_across[3])

# Default Offer - all
offer_s1 <- anger_df %>% filter(player == 1, direct ==0, payoff_tr == 0) %>% select(offer)
offer_s2 <- anger_df %>% filter(player == 1, direct ==0, payoff_tr == 1) %>% select(offer)
offer_d1 <- anger_df %>% filter(player == 1, direct ==1, payoff_tr == 0) %>% select(offer)
offer_d2 <- anger_df %>% filter(player == 1, direct ==1, payoff_tr == 1) %>% select(offer)
offer_s_pool <- anger_df %>% filter(player == 1, direct ==0) %>% select(offer)
offer_d_pool <- anger_df %>% filter(player == 1, direct ==1) %>% select(offer)

offer_s <- wilcox.test(pull(offer_s1), pull(offer_s2), exact = FALSE, na.rm = TRUE)
offer_d <- wilcox.test(pull(offer_d1), pull(offer_d2), exact = FALSE, na.rm = TRUE)
offer_across <- wilcox.test(pull(offer_s_pool), pull(offer_d_pool), exact = FALSE, na.rm = TRUE)

pval_offer <- c(offer_s[3], offer_d[3], offer_across[3])

# Default Offer  - male
m_offer_s1 <- anger_df %>% filter(player == 1, direct ==0, payoff_tr == 0, gender == 0) %>% select(offer)
m_offer_s2 <- anger_df %>% filter(player == 1, direct ==0, payoff_tr == 1, gender == 0) %>% select(offer)
m_offer_d1 <- anger_df %>% filter(player == 1, direct ==1, payoff_tr == 0, gender == 0) %>% select(offer)
m_offer_d2 <- anger_df %>% filter(player == 1, direct ==1, payoff_tr == 1, gender == 0) %>% select(offer)
m_offer_s_pool <- anger_df %>% filter(player == 1, direct ==0, gender == 0) %>% select(offer)
m_offer_d_pool <- anger_df %>% filter(player == 1, direct ==1, gender == 0) %>% select(offer)

m_offer_s <- wilcox.test(pull(m_offer_s1), pull(m_offer_s2), exact = FALSE, na.rm = TRUE)
m_offer_d <- wilcox.test(pull(m_offer_d1), pull(m_offer_d2), exact = FALSE, na.rm = TRUE)
m_offer_across <- wilcox.test(pull(m_offer_s_pool), pull(m_offer_d_pool), exact = FALSE, na.rm = TRUE)

m_pval_offer <- c(m_offer_s[3], m_offer_d[3], m_offer_across[3])

# Default Offer  - female
f_offer_s1 <- anger_df %>% filter(player == 1, direct ==0, payoff_tr == 0, gender == 1) %>% select(offer)
f_offer_s2 <- anger_df %>% filter(player == 1, direct ==0, payoff_tr == 1, gender == 1) %>% select(offer)
f_offer_d1 <- anger_df %>% filter(player == 1, direct ==1, payoff_tr == 0, gender == 1) %>% select(offer)
f_offer_d2 <- anger_df %>% filter(player == 1, direct ==1, payoff_tr == 1, gender == 1) %>% select(offer)
f_offer_s_pool <- anger_df %>% filter(player == 1, direct ==0, gender == 1) %>% select(offer)
f_offer_d_pool <- anger_df %>% filter(player == 1, direct ==1, gender == 1) %>% select(offer)

f_offer_s <- wilcox.test(pull(f_offer_s1), pull(f_offer_s2), exact = FALSE, na.rm = TRUE)
f_offer_d <- wilcox.test(pull(f_offer_d1), pull(f_offer_d2), exact = FALSE, na.rm = TRUE)
f_offer_across <- wilcox.test(pull(f_offer_s_pool), pull(f_offer_d_pool), exact = FALSE, na.rm = TRUE)

f_pval_offer <- c(f_offer_s[3], f_offer_d[3], f_offer_across[3])

# Parametric ttest
# par_accept <- c(
#  t.test(anger_df$accept[anger_df$player == 2 & anger_df$direct == 0] ~ anger_df$payoff_tr[anger_df$player == 2 & anger_df$direct == 0])[3],
#  t.test(anger_df$accept[anger_df$player == 2 & anger_df$direct == 1] ~ anger_df$payoff_tr[anger_df$player == 2 & anger_df$direct == 1])[3],
#  t.test(anger_df$accept[anger_df$player == 2] ~ anger_df$direct[anger_df$player == 2])[3])

#########################################################################################################
## ---  Function that create Mean Graphs w/ Pvalues - save as jpeg in 'output/graphs/'--- ##
#########################################################################################################
# offset1 = strategy,  offset2 = direct,  offset3 = across, hh = text
barP <- function(means, pvals, offset1=0.1, offset2=0.1, offset3 = 0.4, hh, ...) {
  jpeg(paste(out_path, deparse(substitute(means)), '.jpeg', sep=""), width = 1000, height = 800, units = "px", pointsize = 12,
       quality = 75)
  breaks <- barplot(means, ylim=c(0, 1.15), ...)
  ylims <- diff(means) + means[-length(means)] + 0.1
  # Strategy
  segments(x0=breaks[-length((breaks))][1], y0=ylims[1] + offset1, x1 = breaks[-1][1], y1= ylims[1]+ offset1 )
  segments(x0=c(breaks[-length(breaks)][1], breaks[-1][1]),
           y0=rep(ylims[1] + offset1, each=2), y1=rep(ylims[1]-0.05/2 + offset1, each=2))
  text(breaks[-length(breaks)][1]+diff(breaks)[1]/2, ylims[1] + offset1 + hh,
       labels=paste("p=", round(pvals[1], digits =3)))
  # Direct
  segments(x0=breaks[-length((breaks))][3], y0=ylims[3] + offset2, x1 = breaks[-1][3], y1= ylims[3] + offset2)
  segments(x0=c(breaks[-length(breaks)][3], breaks[-1][3]),
           y0=rep(ylims[3] + offset2, each=2), y1=rep(ylims[3]-0.05/2 + offset2, each=2))
  text(breaks[-length(breaks)][3]+diff(breaks)[3]/2, ylims[3] + offset2 +  hh,
       labels=paste("p=", round(pvals[2], digits = 3)))
  # Across
  segments(x0=breaks[-length((breaks))][1]+0.5, y0=ylims[3]+offset3, x1 = breaks[-1][3]-0.5, y1= ylims[3]+offset3)
  segments(x0=c(breaks[-length(breaks)][1]+0.5, breaks[-1][3]-0.5),
           y0=rep(ylims[3]+offset3, each=2), y1=rep(ylims[3]-0.05/2+offset3, each=2))
  text(breaks[-length(breaks)][2]+0.5, ylims[3]+ offset3 + hh,
       labels=paste("p=", round(pvals[3], digits =3)))
  # labels
  text(x=breaks, means, labels = signif(means, digits = 3)*100, pos =3, cex = 0.9 )
  dev.off()
}
#########################################################################################################
# Acceptance - all
barP(accept_mean, as.numeric(pval_accept), offset1=0.02, offset2=0.04, offset3 = 0.2, hh = 0.02,
                    ylab = '% Acceptance',
                    names.arg=toupper(name_treatment)
                )


# Acceptance - male
barP(m_accept_mean, as.numeric(m_pval_accept), offset1=0.3, offset2=0.1, offset3 = 0.3, hh = 0.02,
 ylab = '% Acceptance',
                     names.arg=toupper(name_treatment), col = 'darkblue'
)

# Acceptance - female
barP(f_accept_mean, as.numeric(f_pval_accept), offset1=0, offset2=0.1, offset3 = 0.25, hh = 0.01,
     ylab = '% Acceptance',
     names.arg=toupper(name_treatment), col = 'darkred'
)

# Default Allocation
barP( offer_mean, as.numeric(pval_offer), offset1=0.1, offset2=0.1, hh = 0.03,
      ylab = '% Default Allocation',
      names.arg=toupper(name_treatment)
)

# Default Allocation - male
barP(m_offer_mean, as.numeric(m_pval_offer), offset1=0.15, offset2=0.15, hh = 0.03,
     ylab = '% Default Allocation',
     names.arg=toupper(name_treatment), col = 'darkblue'
)

# Default Allocation - female
barP(f_offer_mean, as.numeric(f_pval_offer), offset1=0.2, offset2=0.2, offset3 = 0.4, hh = 0.03,
     ylab = '% Default Allocation',
     names.arg=toupper(name_treatment), col = 'darkred'
)

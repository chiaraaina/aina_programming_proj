#############################################################################################
###################################### ANGER PROJECT ########################################
###################################### FAKE DATASET  ########################################
#############################################################################################
# 1. import csv
# 2. substitute data
#############################################################################################
library(dplyr)
library(tibble)
library(magrittr)
library(readr)
library(haven)

## --- Clean workspace --- ##
rm(list = ls())


## --- Parse CLI --- ##
args <- commandArgs(trailingOnly = TRUE)
in_folder <- args[1]
out_csv <- args[2]


# -- Check Files meet requirements -- #
checkCLI = function(in_cli, file_ending, type) {
  
  print(paste("Checking argument:", in_cli))
  
  if ( (endsWith(in_cli, file_ending) == FALSE) ){
    stop("Command Line Input fails type check.")
  }
}

checkCLI(in_folder, "dta", input)
checkCLI(out_csv, "csv", output)

print("All Command Line Inputs passed file ending check.")

## --- Read the data --- ##
#anger_df <- read_dta('input/raw-data/final_psycho.dta')
anger_df <- read_dta(in_folder)

### --- Renaming sessions labels & assign to treatments --- ###
sessioncode_list = c('boe0nxlk', '4eatvz92', 'dzxspxf1', 'gcuqszar',
                     'fmbabny8', 'ypt3etsj', 'nx1o3dga', 'z2d59abn',
                     '95lqqwx8', 'ihtrombf', 'j1mlfxc6', '6wql170q',
                     'x2t5tuno', 'nddfu59v', 'xnlln9h6', '1pexnuy7',
                     '3fk4qcz4', 'bvbfw45c', 'o0przvr1', '4knmaxrg',
                     'akv4qvsw', '86dj741b', 'xwo7rsfc', 'd4gbwm27')

# Check list has all elements - sessioncode_list is in order
if (length(unique(anger_df$sessioncode)) == length(sessioncode_list)) {
  print('Ok. Same list in order.')
} else {
  print('Check the list.')
}

for (i in seq_along(sessioncode_list)){
  anger_df$sessionlabel[anger_df$sessioncode ==sessioncode_list[i]] <- i
}

# Assign treatments labels to sessions (2x2 design: direct/strategy vs m1/m2/m3)
s_m2 = c(1,2,7,8,10,11,12,15)
s_m3 = c(17,18,19,20,21,22,23,24)

# Payoff treatment indicator: 0 m1 1 m2 NA m3
anger_df$payoff_tr <- 0
# Payoff profile: 1 m1 2 m2 3 m3
anger_df$payoff_profile <- 1



for(i in s_m2){
  
  anger_df$payoff_tr[anger_df$sessionlabel == i] <- 1
  anger_df$payoff_profile[anger_df$sessionlabel == i] <- 2
}

for(i in s_m3){
  anger_df$payoff_tr[anger_df$sessionlabel == i] <- NA
  anger_df$payoff_profile[anger_df$sessionlabel == i] <- 3
}


# Method of play: 0 Strategy 1 Direct
s_dir = c(2,4,5,7,9,10,12,14,15,16,17,18,19, 21,22)
anger_df$direct <- 0

for(i in s_dir){
  anger_df$direct[anger_df$sessionlabel == i] <- 1
}

# Treatment: identify 1 S1 2 D1 3 S2 4 D2 5 S3 6 M3
anger_df$treat <- NA
anger_df$treat[anger_df$direct ==  0 & anger_df$payoff_profile == 1] <- 1 #S1
anger_df$treat[anger_df$direct ==  1 & anger_df$payoff_profile == 1] <- 2 #D1
anger_df$treat[anger_df$direct ==  0 & anger_df$payoff_profile == 2] <- 3 #S2
anger_df$treat[anger_df$direct ==  1 & anger_df$payoff_profile == 2] <- 4 #D2
anger_df$treat[anger_df$direct ==  0 & anger_df$payoff_profile == 3] <- 5 #S3
anger_df$treat[anger_df$direct ==  1 & anger_df$payoff_profile == 3] <- 6 #D3


# Offer: 0 Greedy 1 Default - define for both players
anger_df$groupoffer[anger_df$groupoffer == 'Offerta A'] <- 1
anger_df$groupoffer[anger_df$groupoffer == 'Offerta B'] <- 0

mean_offer = mean(as.numeric(anger_df$groupoffer))
mean_accept = mean(as.numeric(anger_df$groupoffer_accept), na.rm= TRUE)

### --- Substitute Data--- ###
# 1. random offer for player == 1 (same for player == 2 with same 'groupid_in_subsession' per 'sessioncode')
# 2. if offer == 1 groupoffer_accept
table(anger_df$player, anger_df$payoff_profile)

# Unique ID per group
anger_df <- anger_df %>% arrange(sessioncode, groupid_in_subsession)
anger_df <- within(anger_df, unique_group <- as.numeric(interaction(sessioncode,groupid_in_subsession,
                                                                    drop=TRUE, lex.order=TRUE)))



for(i in seq_along(unique(anger_df$unique_group))){
  random_offer <- rbinom(n=1, size=1, prob = mean_offer)
  random_accept <- rbinom(n=1, size=1, prob = mean_accept)
  anger_df$groupoffer[anger_df$unique_group == i] <- random_offer
  anger_df$groupoffer_accept[anger_df$unique_group == i & anger_df$ direct == 0] <- random_accept
  anger_df$groupoffer_accept[anger_df$unique_group == i & anger_df$ direct == 1 & random_offer == 0] <- random_accept
  anger_df$groupoffer_accept[anger_df$unique_group == i & anger_df$ direct == 1 & random_offer == 1] <- NaN
}

#View(select(anger_df, sessioncode, groupid_in_subsession, player, unique_group, groupoffer_accept, groupoffer))

### --- Eliminating variables for treatments so that it looks like the real dataset --- ###
drop_col = c('direct', 'payoff_profile', 'payoff_tr', 'groupgame_outcome')
anger_df[, drop_col] <- list(NULL)

### --- Save clean data --- ###
# write.csv(anger_df, file = 'input/raw-data/fake_data.csv')
write.csv(anger_df, out_csv)


#############################################################################################
###################################### ANGER PROJECT ########################################
###################################### DATA CLEANING ########################################
#############################################################################################
# 1. import csv
# 2. clean data
# 3. create relevant variables
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
out1_csv <- args[2]
out2_csv <- args[3]

#-- Create output folder --- #
if (!dir.exists("./output")) {
  dir.create("./output")
}
if (!dir.exists("./output/data")) {
  dir.create("./output/data")
}


# -- Check Files meet requirements -- #
 checkCLI = function(in_cli, file_ending, type) {

   print(paste("Checking argument:", in_cli))

   if ( (endsWith(in_cli, file_ending) == FALSE) ){
     stop("Command Line Input fails type check.")
   }
 }

checkCLI(in_folder, "csv", input)
checkCLI(out1_csv, "csv", output)
checkCLI(out2_csv, "csv", output)

print("All Command Line Inputs passed file ending check.")

## --- Read the data --- ##
# anger_df <- read_dta('input/raw-data/final_psycho.dta')
#anger_df <- read.csv('input/raw-data/fake_data.csv')
anger_df <- read.csv(in_folder)


## --- Eliminate oTree coulumns with no infomation --- ##
drop_col = c('participantid_in_session', 'participant_is_bot', 'participant_max_page_index', 'participant_current_app_name',
             'participant_round_number', 'participantip_address', 'participantmturk_worker_id', 'participant_index_in_pages',
             'participant_current_page_name', 'participantexclude_from_data_an', 'participantvisited',
             'participantmturk_assignment_id', 'subsessionround_number', 'sessionexperimenter_name',
             'sessionmturk_HITId', 'sessionmturk_HITGroupId', 'sessioncomment', 'sessionis_demo'
             )
anger_df <- anger_df %>% select( - drop_col)


### --- Renaming sessions labels & assign to treatments --- ###
sessioncode_list = c('boe0nxlk', '4eatvz92', 'dzxspxf1', 'gcuqszar',
                     'fmbabny8', 'ypt3etsj', 'nx1o3dga', 'z2d59abn',
                     '95lqqwx8', 'ihtrombf', 'j1mlfxc6', '6wql170q',
                     'x2t5tuno', 'nddfu59v', 'xnlln9h6', '1pexnuy7',
                     '3fk4qcz4', 'bvbfw45c', 'o0przvr1', '4knmaxrg',
                     'akv4qvsw', '86dj741b', 'xwo7rsfc', 'd4gbwm27')

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

# Higher A default payoff: 0 m1 m2 1  m3
anger_df$a_high <- 0
# Higher B payoff: 0 m1 1 m2 m3
anger_df$b_high <- 0
# Inequity in Default: 0 m1 1 m2 0 m3
anger_df$inequity_default <-0


for(i in s_m2){

  anger_df$payoff_tr[anger_df$sessionlabel == i] <- 1
  anger_df$a_high[anger_df$sessionlabel == i] <- 0
  anger_df$b_high[anger_df$sessionlabel == i] <- 1
  anger_df$payoff_profile[anger_df$sessionlabel == i] <- 2
  anger_df$inequity_default[anger_df$sessionlabel == i] <- 1
}

for(i in s_m3){
  anger_df$payoff_tr[anger_df$sessionlabel == i] <- NA
  anger_df$a_high[anger_df$sessionlabel == i] <- 1
  anger_df$b_high[anger_df$sessionlabel == i] <- 1
  anger_df$payoff_profile[anger_df$sessionlabel == i] <- 3
}



# Method of play: 0 Strategy 1 Direct
s_dir = c(2,4,5,7,9,10,12,14,15,16,17,18,19, 21,22)
anger_df$direct <- 0

for(i in s_dir){
  anger_df$direct[anger_df$sessionlabel == i] <- 1
}

# Treatment: identify 1 S1 2 D1 3 S2 4 D2 5 S3 6 M3
anger_df$treat <-NA
anger_df$treat[anger_df$direct ==  0 & anger_df$payoff_profile == 1] <- 1 #S1
anger_df$treat[anger_df$direct ==  1 & anger_df$payoff_profile == 1] <- 2 #D1
anger_df$treat[anger_df$direct ==  0 & anger_df$payoff_profile == 2] <- 3 #S2
anger_df$treat[anger_df$direct ==  1 & anger_df$payoff_profile == 2] <- 4 #D2
anger_df$treat[anger_df$direct ==  0 & anger_df$payoff_profile == 3] <- 5 #S3
anger_df$treat[anger_df$direct ==  1 & anger_df$payoff_profile == 3] <- 6 #D3

# table(anger_df$payoff_profile, anger_df$direct)


## --- Renaming variables --- ###
anger_df <-rename(anger_df,
       accept = groupoffer_accept,
       belief = playerbelief, # report in Astrid do file
       belief_payoff = playerbelief_payoff,
       game_payoff = playergame_payoff,
       payoff = playerpayoff,
       offer = groupoffer)


anger_df$final_payoff <- anger_df$payoff - 4

# Create dummies for pairs
# Accept: 0,1 - defined for both A and B



# table(anger_df$offer, anger_df$accept, exclude = anger_df$direct)

# Belief of Default allocation
anger_df$belief[anger_df$player == 2] <- (anger_df$belief/10)[anger_df$player == 2]
anger_df$belief[anger_df$player == 1 & anger_df$direct == 0] <- (anger_df$belief/10)[anger_df$player == 1 & anger_df$direct == 0]
anger_df$belief[anger_df$player == 1 & anger_df$direct == 1] <- (anger_df$belief/100)[anger_df$player == 1 & anger_df$direct == 1]

### --- Save clean data --- ###
# write.csv(anger_df, file = 'output/data/cleaned_data_all.csv')
write.csv(anger_df, out1_csv)

### --- Save clean data without m3 --- ###
anger_df <- anger_df %>% filter(payoff_profile != 3)

# write.csv(anger_df, file = 'output/data/cleaned_data.csv')
write.csv(anger_df, out2_csv)

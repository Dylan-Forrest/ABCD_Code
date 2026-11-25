########################################
#  Title: Valid Previous N-Back Scores 
#  Date: 9/10/2026       
#  Author: Dylan Forrest Seay 
########################################


# The purpose of this code is to create a column for each participant that indicates
# the amount of previous N-back Accuracy scores they have at any given timepoint.

# Designed for the ABCD Study 6.0 data release, column naming convention may change in future releases

# This code has 2 options for outputs:
  
  # For all ABCD participants
     # Nback_Acc_all_TPs.csv  -  Includes all tps in the study  [Output if code is run as is]
     # Nback_Acc_TPs.csv  -  Only accounts for tps in the study where N-back data was recorded  (session 0, 2, 4, & 6)


#################################
## Importing packages and data ##
#################################

library(tidyverse)
library(tidyverse)

datapath <- "your/ABCD/datapath/6.0"

# Study Timepoints #
All_TPs <- read_tsv(paste0(datapath, "abcd_general/ab_g_dyn.tsv"))

# Nback Data #
Nback <- read_tsv(paste0(datapath, "Imaging/Task_fMRI/mr_y_tfmri__nback__beh.tsv"))


######################################
## Create N-back Accuracy Dataframe ##                         
######################################

# Select & rename relevant rows #
Nback_Clean <- Nback %>%
  select(participant_id, session_id, mr_y_tfmri__nback__beh_acc) %>%
  rename("Nback_Accuracy" = mr_y_tfmri__nback__beh_acc)

All_TPs_Clean <- All_TPs %>%
  select(participant_id, session_id)

# Create dataframe #
Nback_Acc_TPs <- merge(All_TPs_Clean, Nback_Clean, by = c("participant_id", "session_id"), all.x = TRUE)


###########################################################
## Create Column Indicating Valid Previous N-back Scores ##
###########################################################

## Create valid N-back score flag ##
# All study tps #
Nback_Acc_all_TPs <- Nback_Acc_TPs %>% 
  mutate(Nback_Flag = if_else(is.na(Nback_Accuracy), 0, 1))  # 1 = valid data / 0 = no data

# All N-back tps #
Nback_Acc_TPs <- Nback_Acc_TPs %>% 
  mutate(Nback_Flag = if_else(is.na(Nback_Accuracy), 0, 1))  # 1 = valid data / 0 = no data


# Create vector of tps that recorded N-back data #
Nback_TPs <- c("ses-00A", "ses-02A", "ses-04A", "ses-06A")


## Create valid previous N-back score indicator ##
# All study tps #
Nback_Acc_all_TPs <- Nback_Acc_all_TPs %>% 
  group_by(participant_id) %>%
  mutate(Valid_Previous_Nbacks = lag(cumsum(Nback_Flag), default = 0)) %>%
  ungroup()

# All N-back tps #
Nback_Acc_TPs <- Nback_Acc_TPs %>% 
  filter(session_id %in% Nback_TPs) %>%  # keep only tps recording N-back data
  group_by(participant_id) %>%
  mutate(Valid_Previous_Nbacks = lag(cumsum(Nback_Flag), default = 0)) %>%
  ungroup()


##############################     
## Prepping Data for Export ##    
##############################     

#  run section if you want outputs to only include
#  participant_id, session_id, and Valid_Nbacks_at_TP columns

# All ABCD study tps #
Nback_Acc_all_TPs <- Nback_Acc_all_TPs %>%
  select(participant_id, session_id, Valid_Previous_Nbacks)

# Only N-back tps (session 0, 2, 4, & 6) #
Nback_Acc_TPs <- Nback_Acc_TPs %>%
  select(participant_id, session_id, Valid_Previous_Nbacks)



########################
## Export Data to CSV ##   
########################

# All ABCD study tps #
write_csv(Nback_Acc_all_TPs, "Nback_Acc_all_TPs.csv")    # Main output

# Only N-back tps (session 0, 2, 4, & 6) #
#write_csv(Nback_Acc_TPs, "Nback_Acc_TPs.csv")




# "     _|_     "
# "    /o o\    "   # goodbye! 
# "    \___/    "
# "      |/     "
# "     /|      "
# "     / \     "
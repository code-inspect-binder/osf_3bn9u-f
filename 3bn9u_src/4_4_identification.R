library(tidyverse) #tidyverse_1.3.1
library(janitor) #janitor_2.1.0
library(htmlwidgets) #htmlwidgets_1.5.4
library(data.table) #data.table_1.14.2
library(ggpubr) #ggpubr_0.4.0

#### A short script to inspect all responses from individual participants

##### Functions
#####
# function to find participant numbers whose responses match a certain condition
# inputs: dataset, variable name (as string), condition to be matched
# outputs: returns participant number
find_participant <- function(data, variable, condition) {
  # empty vector to take numbers
  participant_numbers <- c()
  # iterates through row numbers
  for (r in 1:nrow(data)) {
    # if the cell in that row and column match the condition
    if (data[[variable]][r] == condition) {
      # return the participant number
      participant_num <- (data[r,1])
      participant_numbers <- c(participant_numbers, participant_num)
    }
  }
  return(participant_numbers)
}

# function to return a single person's response as a dataframe
# inputs: dataset, response ID number
# outputs: dataframe
single_response <- function(dataset, ID_number) {
  # subset single response from dataset
  r <- subset(dataset, response_ID == ID_number)
  # remove empty columns from response  
  r <- remove_empty(r, which = c("cols"), quiet = FALSE)
  # transpose
  t_r <- transpose(r)
  # set row names of excerpted dataset from columns of original
  rownames(t_r) <- colnames(r)
  # remove any rows with "false" (i.e. participant did not select this option)
  t_r <- subset(t_r, V1 != FALSE)
  # return dataset
  return(t_r)
}

# function to generate output csv files for a list of response IDs
# inputs: dataset, list of response IDs
# outputs: csv files named with response IDs
multi_response <- function(dataset, IDs_list) {
  for (i in IDs_list) {
    participant_response <- single_response(dataset, i)
    csv_name <- str_c(i, ".csv", sep = "")
    write.csv(participant_response, csv_name)
  }
}
#####

# import cleaned and processed datasets (exported at end of analysis script)
wos <- read.csv("wos_processed.csv")
aaas <- read_csv("aaas_processed.csv")

# find participants who included their emails
wos_email_participants <- find_participant(wos, "email_text", TRUE)
aaas_email_participants <- find_participant(aaas, "email_text", TRUE)

# export responses to csv files
multi_response(wos, wos_email_participants)
multi_response(aaas, aaas_email_participants)

# find participant with very high harassment score
find_participant(wos, "harassment_score", 20) # participant 209
# find participant with very high harassment intensity
high_intensity_scores <- sort(wos$harassment_intensity, decreasing = TRUE)
high_intensity_scores #highest is 717 
find_participant(wos, "harassment_intensity", 717) # also participant 209
harassment_score_outlier <- single_response(wos, 209)
# identical responses to all media questions
# extraordinarily high harassment frequency compared to other participants
# with higher frequency for more extreme forms of harassment
# and lower frequency for less extreme forms of harassment
write.csv(harassment_score_outlier, "wos/harassment_score_outlier.csv")

# find other participants with high harassment scores
high_harassment_scores <- sort(wos$harassment_score, decreasing = TRUE)
high_harassment_scores #next-highest are 14 and 13 with one participant each
# then multiple participants with scores of 12 and below

find_participant(wos, "harassment_score", 14) # participant 523
hs14 <- single_response(wos, 523) 
# participant provided email address and high profile could be verified
write.csv(hs14, "wos/harassment_score_14.csv")

find_participant(wos, "harassment_score", 13) # participant 52
hs13 <- single_response(wos, 52) # no concrete concerns
write.csv(hs13, "wos/harassment_score_13.csv")

# find high-harassment participant in AAAS survey
high_intensity_scores <- sort(aaas$harassment_intensity, decreasing = TRUE)
high_intensity_scores #highest is 734
find_participant(aaas, "harassment_intensity", 734) # participant 1554
# participant provided email address and high profile could be verified
aaas_harassment_score_outlier <- single_response(aaas, 1554)
write.csv(aaas_harassment_score_outlier, "aaas/harassment_score_outlier.csv")

##### Environment
#####
# platform       x86_64-apple-darwin17.0     
# arch           x86_64                      
# os             darwin17.0                  
# system         x86_64, darwin17.0          
# status                                     
# major          4                           
# minor          1.2                         
# year           2021                        
# month          11                          
# day            01                          
# svn rev        81115                       
# language       R                           
# version.string R version 4.1.2 (2021-11-01)
# nickname       Bird Hippie 
#####

library(tidyverse) #tidyverse_1.3.1
library(janitor) #janitor_2.1.0
library(htmlwidgets) #htmlwidgets_1.5.4
library(data.table) #data.table_1.14.2
library(ggpubr) #ggpubr_0.4.0

##### Functions
#####
# returns a list of columns that match a text string
# inputs: dataset and text string
# outputs: a list of column indices, and a printout of column names
column_chunk <- function(dataset, text_string) {
  questionIDs <- grep(text_string, names(dataset))
  # print relevant column names to check whether last item is a free text field
  print(names(dataset[questionIDs]))
  return(questionIDs)
}

# renames a series of columns that all have the same multiple choice options
# participants could select all items that apply
# variable names should begin with the question name,
# and end with the name of each multiple choice item
# inputs: dataset; list of the indices of the relevant columns (column_indices);
# the question name that should begin each variable name (new_variable_name)
# output is a list of the multiple choice items, to be used in dataviz,
# and changed variable names in the dataset
batch_rename <- function(dataset, column_indices, new_variable_name) {
  # create a list of the names of columns with relevant phrase
  current_names <- names(dataset[column_indices])
  # get the location of the first colon in each of these titles
  colon_positions <- str_locate(current_names, ":")
  # create an empty vector for names of multiple choice options
  checkbox_names <- vector("character", length(current_names))
  # extract the string up to the position of the first colon in each current_names item
  for (i in seq_along(current_names)) {
    checkbox_names[[i]] <- substr(current_names[i], 1, colon_positions[i,1]-1)
  }
  # create list of strings to append to new variable names
  # lowercase, with punctuation removed/replaced with _
  # add colons, full stops
  to_append <- str_to_lower(
    str_replace_all(
      checkbox_names, 
      c("\\(" = "", 
        "\\)" = "", 
        " -" = "",
        "-" = "",
        "," = "_",
        "/" = "_",
        " " = "_"
      )
    )
  )
  # create list with repeated instances of new_variable_name
  variable_name_repeated <- rep(c(new_variable_name), length(current_names))
  # append multiple choice options to each new variable name
  new_names <- str_c(variable_name_repeated, to_append, sep = "_")
  # change variable names in dataset
  dataset <- setnames(dataset, old = current_names, new = new_names)
  # return checkbox_names to be used in .csv printout and dataviz
  return(checkbox_names)
}

# changes binary multiple choice to logical vector
# inputs: dataset, list of column indices
# outputs: dataset
logic_binary <- function(dataset, column_indices) {
  # change rows where field is selected to TRUE and NA to FALSE
  for (i in column_indices) {
    dataset[[i]] <- !is.na(dataset[[i]])
  }
  return(dataset)
}

### all harassment questions involve four sub-questions:
### harassment question 1 (hq1): did you experience this kind of harassment?
### hq2: if yes, through what venues? (multiple columns)
### hq3: if yes, for how long?
### hq4: if yes, how many times / what has the frequency been?
### the function to process them starts with a list of indices for hq2

# processes a series of related harassment questions (HQs):
# creates names of variables;
# renames columns and converts to factors;
# inputs: dataset, column indices for whole question set, question keyword
# outputs: returns new dataset
hq_process <- function(dataset, column_indices, hqtopic){
  # calculate column indices for hq1-4
  hq1_column_index <- column_indices[1]
  hq2_hq4 <- tail(column_indices, 3)
  hq2_free_text_column_index <- hq2_hq4[1]
  hq3_column_index <- hq2_hq4[2]
  hq4_column_index <- hq2_hq4[3]
  hq2_column_indices <- column_indices %>% tail(-1) %>% head(-3)
  all_q_indices <- c(hq1_column_index, hq2_column_indices, hq3_column_index, hq4_column_index)
  # list of current variable names for hq1, 3, 4
  hq1_old_name <- names(dataset[hq1_column_index])
  hq2_free_text_old_name <- names(dataset[hq2_free_text_column_index])
  hq3_old_name <- names(dataset[hq3_column_index])
  hq4_old_name <- names(dataset[hq4_column_index])
  current_names <- c(hq1_old_name, hq2_free_text_old_name,
                     hq3_old_name, hq4_old_name)
  # create new variable names
  hq1_column_new_name <- hqtopic
  hq2_column_name_prefix <- str_c(hqtopic, "venue", sep = "_")
  hq2_free_text_new_column_name <- str_c(hq2_column_name_prefix,
                                         "free_text", sep = "_")
  hq3_column_new_name <- str_c(hqtopic, "timing", sep = "_")
  hq4_column_new_name <- str_c(hqtopic, "frequency", sep = "_")
  new_names <- c(hq1_column_new_name,
                 hq2_free_text_new_column_name,
                 hq3_column_new_name,
                 hq4_column_new_name)
  # rename hq1, hq3, hq4
  dataset <- setnames(dataset, old = current_names, new = new_names)
  # rename hq2 columns and convert to logic
  hq2_options <- batch_rename(dataset, hq2_column_indices,
                              hq2_column_name_prefix)
  dataset <- logic_binary(dataset, hq2_column_indices)
  return(dataset)
}

# applies hq_process to all harassment questions at once
# inputs: dataset, list of keywords used to identify each batch of columns,
# and the prefix to be applied in each batch-renaming
# outputs: returns new dataset
hq_all <- function(dataset, keywords, new_prefixes) {
  for (i in seq_along(keywords)) {
    # identifies the applicable columns for each set of keywords
    columns <- column_chunk(dataset, keywords[i])
    # adds hq4 column to the list
    columns <- append(columns, tail(columns, 1)+1)
    # applies hq_process to these columns
    dataset <- hq_process(dataset, columns, new_prefixes[i])
  }
  return (dataset)
}

# slight modification of hq_process for harassment questions that
# address offline threats, and therefore do not include hq2
# "timing" and "frequency" questions still labelled hq3 and hq4
# for consistency
hq_process2 <- function(dataset, column_indices, hqtopic){
  # calculate column indices for hq1-4
  hq1_column_index <- column_indices[1]
  hq3_column_index <- column_indices[2]
  hq4_column_index <- column_indices[3]
  # list of current variable names for hq1, 3, 4
  hq1_old_name <- names(dataset[hq1_column_index])
  hq3_old_name <- names(dataset[hq3_column_index])
  hq4_old_name <- names(dataset[hq4_column_index])
  current_names <- c(hq1_old_name, hq3_old_name, hq4_old_name)
  # create new variable names
  hq1_column_new_name <- hqtopic
  hq3_column_new_name <- str_c(hqtopic, "timing", sep = "_")
  hq4_column_new_name <- str_c(hqtopic, "frequency", sep = "_")
  new_names <- c(hq1_column_new_name,
                 hq3_column_new_name,
                 hq4_column_new_name)
  # rename hq1, hq3, hq4
  dataset <- setnames(dataset, old = current_names, new = new_names)
  return(dataset)
}

# applies hq_process2 to all harassment questions at once
# inputs: dataset, list of keywords used to identify each batch of columns,
# and the prefix to be applied in each batch-renaming
# outputs: returns new dataset
hq2_all <- function(dataset, keywords, new_prefixes) {
  for (i in seq_along(keywords)) {
    # identifies the applicable columns for each set of keywords
    columns <- column_chunk(dataset, keywords[i])
    # adds hq4 column to the list
    columns <- append(columns, tail(columns, 1)+1)
    # applies hq_process to these columns
    dataset <- hq_process2(dataset, columns, new_prefixes[i])
  }
  return (dataset)
}

#####

##### Data import and basic cleaning
#####

### Web of Science

# import Web of Science (wos) data
wos <- read_csv("wos_deidentified.csv")
#remove empty columns 
wos <- remove_empty(wos, which = c("cols"), quiet = FALSE)
# remove unnecessary "language" column
wos <- wos[, -5]
# rename first few columns
wos <- rename(wos, 
              response_ID = 1,
              time_started = 2,
              date_submitted = 3,
              completed = 4,
              consent = 5,
              covid_publications = 6)
# remove rows with incomplete consent process, or without confirmation
# of having published COVID papers
wos <- drop_na(wos, consent)
wos <- drop_na(wos, covid_publications)
wos <- filter(wos, covid_publications == "Yes")

### AAAS

# import AAAS members (aaas) data
aaas <- read_csv("aaas_deidentified.csv")
#remove empty columns 
aaas <- remove_empty(aaas, which = c("cols"), quiet = FALSE)
# rename first few columns
aaas <- rename(aaas, 
               response_ID = 1,
               time_started = 2,
               date_submitted = 3,
               completed = 4,
               consent = 5,
               scientific_research = 6)
# remove rows with incomplete consent process, or without confirmation
# of conducting research as main professional activity
aaas <- drop_na(aaas, consent)
aaas <- drop_na(aaas, scientific_research)
aaas <- filter(aaas, scientific_research == "Yes")

#####

##### Research discipline(s)
#####

### Web of Science
columns <- column_chunk(wos, 
                           "Please choose the discipline\\(s\\) that most closely describe\\(s\\) your COVID-19 publication\\(s\\)")
# check whether last item in printout is a free text field
# if it is, rename this first
column_index <- tail(columns, n=1)
wos <- rename(wos, covid_papers_discipline_free_text = column_index[1])
#repeat process
columns <- column_chunk(wos, 
                        "Please choose the discipline\\(s\\) that most closely describe\\(s\\) your COVID-19 publication\\(s\\)")
# rename columns
covid_papers_discipline_options <- batch_rename(wos, columns, "covid_papers_discipline")
# convert columns to logical vectors
wos <- logic_binary(wos, columns)

### AAAS
# rename column
aaas <- rename(aaas, research_discipline = 7)
# rename free text column
aaas <- rename(aaas, research_discipline_free_text = 8)

#####

##### Number of COVID papers
#####

### Web of Science
# rename column
wos <- rename(wos, number_of_covid_papers = 18)

### AAAS
# no equivalent question

#####

##### Country of research institution
#####

### Web of Science
# rename column
wos <- rename(wos, research_country = 19)

### AAAS
# rename column
aaas <- rename(aaas, research_country = 9)
#####

##### Country of residence
#####

### Web of Science
# rename column
wos <- rename(wos, residence_country = 20)

### AAAS
# rename column
aaas <- rename(aaas, residence_country = 10)

#####

##### Publicity
#####

### Web of Science
columns <- column_chunk(wos, 
                           "Have you received any public attention as a COVID-19 researcher, or have you spoken publicly about any matters related to COVID-19?")
# rename columns
publicity_options <- batch_rename(wos, columns, "publicity")
# rename free text field
wos <- rename(wos, publicity_free_text = 30)

### AAAS
columns <- column_chunk(aaas, 
                           "Have you received any public attention")
# rename columns
publicity_options <- batch_rename(aaas, columns, "publicity")
# rename free text field
aaas <- rename(aaas, publicity_free_text = 20)

#####

##### Publicity country/countries
##### 

### Web of Science
# get columns that match question text
columns <- column_chunk(wos, 
                           "In what country or countries have")
# rename columns
options <- batch_rename(wos, columns, "publicity_countries")
# convert columns to logical vectors
wos <- logic_binary(wos, columns)

### AAAS
# get columns that match question text
columns <- column_chunk(aaas, 
                           "In what country or countries have")
# rename columns
options <- batch_rename(aaas, columns, "publicity_countries")
# convert columns to logical vectors
aaas <- logic_binary(aaas, columns)

#####

##### Publicity discipline
#####

### Web of Science
# get columns that match question text
columns <- column_chunk(wos, 
                            "Please choose the discipline\\(s\\) or ")
# check whether last item in printout is a free text field
# if it is, rename this first
column_index <- tail(columns, n=1)
wos <- rename(wos, "publicity_discipline_free_text" = column_index[1])
#repeat process
columns <- column_chunk(wos, 
                        "Please choose the discipline\\(s\\) or ")
# rename columns
publicity_discipline_options <- batch_rename(wos, columns, "publicity_discipline")
# convert columns to logical vectors
wos <- logic_binary(wos, columns)

### AAAS
# no equivalent question

#####

##### Publicity topic
#####

### Web of Science
# get columns that match question text
columns <- column_chunk(wos, 
                            "In the course of your work")
# check whether last item in printout is a free text field
# if it is, rename this first
column_index <- tail(columns, n=1)
wos <- rename(wos, "publicity_topic_free_text" = column_index[1])
#repeat process
columns <- column_chunk(wos, 
                        "In the course of your work")
# rename columns
publicity_topic_options <- batch_rename(wos, columns, "publicity_topic")
# convert columns to logical vectors
wos <- logic_binary(wos, columns)

### AAAS
# no equivalent question

#####

##### Harassment questions
#####

# list of keywords to find harassment question columns
phrases <- c("personal insults",
             "physical appearance",
             "insults targeting your race",
             "contact from a single individual",
             "contact from many individuals",
             "corruption",
             "professional capabilities",
             "doxxing",
             "wishes of harm or death",
             "you harm yourself",
             "sexual assault",
             "death threats",
             "wishes of harm directed",
             "threats of harm directed",
             "cyber attacks"
             )
# prefixes for new variable names
column_prefixes <- c("insults", 
                     "physical_appearance",
                     "group_characteristic_insults",
                     "high_volume_individual",
                     "high_volume_many",
                     "corruption_allegations",
                     "professional_capabilities",
                     "doxxing",
                     "death_harm_wishes",
                     "harm_yourself",
                     "harm_threats",
                     "death_threats",
                     "family_harm_wishes",
                     "family_harm_threats",
                     "cyber_attacks"
                     )
# list of keywords to find in-person harassment question columns
phrases_2 <- c("physical mail",
               "vandalism",
               "unwanted visits",
               "protests",
               "physical intimidation")
# prefixes for new variable names
column_prefixes_2 <- c("physical_mail",
                       "vandalism",
                       "unwanted_visits",
                       "protests",
                       "physical_intimidation")

### Web of Science
# apply harassment question function to all relevant columns
wos <- hq_all(wos, phrases, column_prefixes)
# apply second harassment question function to all relevant columns
wos <- hq2_all(wos, phrases_2, column_prefixes_2)
# rename "other kinds of harassment" columns
wos <- rename(wos, 
              other_harassment = 408,
              other_harassment_free_text = 409)
# move free text column so that others can be batch-processed
wos <- relocate(wos, other_harassment_free_text, .before = other_harassment)
# generate column indices
columns <- column_chunk(wos, "other type")
# add hq1 column index to list
columns <- c(409, columns)
# add hq4 column index to list
columns <- append(columns, tail(columns, 1)+1)
# rename all "other harassment" columns
wos <- hq_process(wos, columns, "other_harassment")

### AAAS
# column rename to get around unicode glitch that prevents column
# from being identified in renaming function
aaas <- rename(aaas, 
               "Approximately when did you first start experiencing threats of harm directed at your family?" 
               = 398)
# apply harassment question function to all relevant columns
aaas <- hq_all(aaas, phrases, column_prefixes)
# apply second harassment question function to all relevant columns
aaas <- hq2_all(aaas, phrases_2, column_prefixes_2)
# rename "other kinds of harassment" columns
aaas <- rename(aaas, 
               other_harassment = 427,
               other_harassment_free_text = 428)
# move free text column so that others can be batch-processed
aaas <- relocate(aaas, other_harassment_free_text, .before = other_harassment)
# generate column indices
columns <- column_chunk(aaas, "other type")
# add hq1 column index to list
columns <- c(428, columns)
# add hq4 column index to list
columns <- append(columns, tail(columns, 1)+1)
# rename all "other harassment" columns
aaas <- hq_process(aaas, columns, "other_harassment")
#####

##### Prepandemic harassment
#####

### Web of Science
wos <- rename(wos, prepandemic_harassment = 427)

### AAAS
# no equivalent question

#####

##### Effects of harassment
#####

### Web of Science
columns <- column_chunk(wos, "research affected you")
# rename columns
effects_options <- batch_rename(wos, columns, "effects")

### AAAS
columns <- column_chunk(aaas, "research affected you")
# rename columns
effects_options <- batch_rename(aaas, columns, "effects")

#####

##### Protective measures
#####

### Web of Science
# get columns that match question text
columns <- column_chunk(wos, "measures have you taken")
# check whether last item in printout is a free text field
# if it is, rename this first
column_index <- tail(columns, n=1)
wos <- rename(wos, protective_measures_free_text = column_index[1])
# repeat process
columns <- column_chunk(wos, "measures have you taken")
# rename columns
protective_measure_options <- batch_rename(wos, columns, "protective_measures")
# convert columns to logical vectors
wos <- logic_binary(wos, columns)

### AAAS
# get columns that match question text
columns <- column_chunk(aaas, "measures have you taken")
# check whether last item in printout is a free text field
# if it is, rename this first
column_index <- tail(columns, n=1)
aaas <- rename(aaas, protective_measures_free_text = column_index[1])
#repeat process
columns <- column_chunk(aaas, "measures have you taken")
# rename columns
protective_measure_options <- batch_rename(aaas, columns, "protective_measures")
# convert columns to logical vectors
aaas <- logic_binary(aaas, columns)
#####

##### Employer support
#####

### Web of Science
wos <- rename(wos, 
              employer_legal_support = 466,
              employer_legal_support_satisfaction = 467,
              employer_security_support = 468,
              employer_security_support_satisfaction = 469,
              employer_technological_support = 470,
              employer_technological_support_satisfaction = 471,
              employer_mental_health_support = 472,
              employer_mental_health_support_satisfaction = 473,
              other_employer_support = 474,
              wished_for_support = 475
              )

### AAAS
aaas <- rename(aaas, 
              employer_legal_support = 485,
              employer_legal_support_satisfaction = 486,
              employer_security_support = 487,
              employer_security_support_satisfaction = 488,
              employer_technological_support = 489,
              employer_technological_support_satisfaction = 490,
              employer_mental_health_support = 491,
              employer_mental_health_support_satisfaction = 492,
              other_employer_support = 493,
              wished_for_support = 494
              )
#####

##### Anything else to add?
#####

### Web of Science
wos <- rename(wos, 
              harassment_free_text = 476,
              other_questions_free_text = 477)

### AAAS
aaas <- rename(aaas, 
              harassment_free_text = 495,
              other_questions_free_text = 496)
#####

##### Demographics
#####

### Web of Science
wos <- rename(wos,
              age = 478,
              sector = 551,
              sector_free_text = 552,
              career_stage = 553,
              career_stage_free_text = 554,
              disability = 555,
              ethnicity = 556,
              minority = 557,
              prejudice = 558,
              sexual_orientation = 559,
              sexual_orientation_free_text = 560,
              gender = 561,
              transgender = 562,
              discrimination_free_text = 563
              )

columns <- column_chunk(wos, "In what country or countries did")
# rename columns
options <- batch_rename(wos, columns, "countries_grew_up")
# convert columns to logical vectors
wos <- logic_binary(wos, columns)

### AAAS
aaas <- rename(aaas,
              age = 497,
              sector = 555,
              sector_free_text = 556,
              career_stage = 557,
              career_stage_free_text = 558,
              disability = 559,
              ethnicity = 560,
              minority = 561,
              prejudice = 562,
              sexual_orientation = 563,
              sexual_orientation_free_text = 564,
              gender = 565,
              gender_free_text = 566,
              transgender = 567,
              discrimination_free_text = 568)
columns <- column_chunk(aaas, "In what country or countries did")
# rename columns
options <- batch_rename(aaas, columns, "countries_grew_up")
# convert columns to logical vectors
aaas <- logic_binary(aaas, columns)

#####

##### Charity selection
#####

### Web of Science
wos <- rename(wos, charity_choice = 564)

### AAAS
aaas <- rename(aaas, charity_choice = 569)
#####

##### Export clean datasets
##### 

### Web of Science
write_csv(wos, "wos_clean.csv")

### AAAS
write_csv(aaas, "aaas_clean.csv")
#####


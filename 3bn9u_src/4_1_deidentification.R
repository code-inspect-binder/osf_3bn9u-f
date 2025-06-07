library(tidyverse) #tidyverse_1.3.1
library(janitor) #janitor_2.1.0
library(htmlwidgets) #htmlwidgets_1.5.4
library(data.table) #data.table_1.14.2
library(ggpubr) #ggpubr_0.4.0

#####
# Web of Science survey emails ###
#####

# import data
wos <- read_csv("wos_responses.csv")

# rename response ID and email columns
wos <- rename(wos, 
              response_ID = 1,
              email = 1021)

# new tibble with IDs and emails
wos_emails <- tibble(
  "responseID" = wos$response_ID, 
  "email" = wos$email
  )

# drop rows with empty email fields
wos_emails <- drop_na(
  wos_emails, 
  email
)

# write email file to csv
write_csv(wos_emails, "wos_emails.csv")

# new column showing whether participant entered email text
wos$email_text <- !is.na(wos$email)

# remove email addresses
wos_deidentified <- wos[, -1021]

# create deidentified csv
write_csv(wos_deidentified, "wos_deidentified.csv")

#####

#####
### AAAS Members survey emails ###
#####

# import data
aaas <- read_csv("aaas_responses.csv")

# rename response ID and email columns
aaas <- rename(aaas, 
              response_ID = 1,
              email = 969)

# new tibble with IDs and emails
aaas_emails <- tibble(
  "responseID" = aaas$response_ID, 
  "email" = aaas$email
)

# drop rows with empty email fields
aaas_emails <- drop_na(
  aaas_emails, 
  email
)

# write email file to csv
write_csv(aaas_emails, "aaas_emails.csv")

# new column showing whether participant entered email text
aaas$email_text <- !is.na(aaas$email)

# remove email addresses
aaas_deidentified <- aaas[, -969]

# create deidentified csv
write_csv(aaas_deidentified, "aaas_deidentified.csv")


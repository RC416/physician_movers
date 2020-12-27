# takes the total payments by doctor, adds a year marker and condenses to one file
# addresses errors in the name of states (~1300 entries have an incorrect state id)
# 2013 is being excluded. Smaller size and may have had reporting issues.

library(dplyr)

# ----------------------------------------------------------------------
# Combine datasets
# load pre-cleaned datasets of total physician payments in each year
# docs_2013 <- read.csv('docs_2013.csv', header=T)
docs_2014 <- read.csv('docs_2014.csv', header=T)
docs_2015 <- read.csv('docs_2015.csv', header=T)
docs_2016 <- read.csv('docs_2016.csv', header=T)
docs_2017 <- read.csv('docs_2017.csv', header=T)
docs_2018 <- read.csv('docs_2018.csv', header=T)
docs_2019 <- read.csv('docs_2019.csv', header=T)

# add year 
# docs_2013['year'] <- 2013
docs_2014['year'] <- 2014
docs_2015['year'] <- 2015
docs_2016['year'] <- 2016
docs_2017['year'] <- 2017
docs_2018['year'] <- 2018
docs_2019['year'] <- 2019

# paste tables below each other
all_years_tidy <- bind_rows(docs_2014, docs_2015, docs_2016,
                            docs_2017, docs_2018, docs_2019)

# re-order columns
all_years_tidy <- all_years_tidy[c(1,2,3,4,5,9,7,8,6)]

# drop blank physician ID rows (source of large outliers)
# these payments are large and unlikely attributed to a single physician
# less than 200 rows removed
all_years_tidy <- filter(all_years_tidy, !is.na(Physician_Profile_ID))


# -----------------------------------------------------------------------
# Identify and remove errors in state

# get erroneous state names
state_names <- all_years_tidy %>%
  group_by(Recipient_State) %>%
  summarize(count=n())

state_name_errors <- state_names %>%
  filter(count<1000) # all states with <1000 records correspond to incorrect state ID's: 'MH', 'AS', blank, etc.
state_name_errors <- state_name_errors['Recipient_State'][[1]]

# filter dataset
all_years_tidy <- all_years_tidy %>%
  filter(!Recipient_State %in% state_name_errors)

# -----------------------------------------------------------------------
# Identify and remove errors in payments

# Removing rows with blank Physician ID (line 36) addressed obvious outliers for
# the small-value payment types.


#------------------------------------------------------------------------
# save final dataset
write.csv(all_years_tidy, 'all_years_tidy.csv', row.names=F)








# ----------------------------------------------------------------------
# other useful code

# count unique physicians
nrow(unique(all_years_tidy['Physician_Profile_ID'])) # unique physicians
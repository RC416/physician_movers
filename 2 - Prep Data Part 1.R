# Prepare data for FE model
# filter list of all payments
# identify movers and calculate state means difference (treatment variable)
# return cleaned dataset

# --------------------------------------------------------------------------
# 1 - get dataset of payments grouped by doctor

library(dplyr)
library(tidyr) # for pivot_wider/longer()
library(qdapTools) # for lookup() https://www.rdocumentation.org/packages/qdapTools/versions/1.3.5/topics/lookup

# full payments dataset
all_years_tidy <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/ECO2408 - Econometrics/Project/all_years_tidy.csv", header=T)


# ------------------------------------------------------------------------
# 2 - load and filter data

# get list of payment types
payment_types <- unique(all_years_tidy['Nature_of_Payment_or_Transfer_of_Value'])[[1]] # double brackets to get list, not df slice

payment_types[c(1,2,4,6,7)] # filter for the 5 low-value (<$200 ave) payment types
# should be: Gift, Travel & Lodging, Food & Bev, Education, Entertainment

# payments at physician level
payments <- all_years_tidy %>%
  filter(Nature_of_Payment_or_Transfer_of_Value %in% payment_types[c(1,2,4,6,7)]) %>%
  group_by(year, Physician_Profile_ID) %>%
  summarize(phys_payment=sum(Payment_value),  # sum of all payments in given year
            n_payments=sum(Payment_count),    # number of payments in given year
            state=Recipient_State) %>%
  distinct(.keep_all=T)                       # gets rid of duplicate rows caused physicians who received multiple payment types



# identify observations where physicians list multiple states
multi_state <- payments %>%
  group_by(year,Physician_Profile_ID) %>%
  summarize(count=n(),
            phys_payment=phys_payment,
            n_payments=n_payments,
            state=state)

# get all observations where only one state was listed for a given year
single_state_index <- multi_state['count'] == 1

# subset to remove all multi-state observations
payments <- payments[single_state_index[,1],]



# convert to wide data to enable identification of movers
payments_wide <- pivot_wider(payments,
                             id_cols=Physician_Profile_ID,
                             names_from=year, values_from=c(phys_payment, state))



# ----------------------------------------------------------------------
# 3 - Identify movers 1 and 2 years after their move


# Identify year one and year 2 likely movers 
# same state for two periods, different state for next two (ex. CA, CA, NY, NY)

payments_wide['yr1_move_2014'] = 0  # can't have movers in these years according to the definition
payments_wide['yr1_move_2015'] = 0
payments_wide['yr1_move_2019'] = 0

# year one movers in 2016
# mutate applies a function in parallel to create new df
# case_when does vectorized nested if_else statements
payments_wide = mutate(payments_wide, yr1_move_2016=case_when(
  state_2014==state_2015 & state_2015!=state_2016 & state_2016==state_2017 ~ 1,
  TRUE ~ 0
))

# year one movers in 2017
payments_wide = mutate(payments_wide, yr1_move_2017=case_when(
  state_2015==state_2016 & state_2016!=state_2017 & state_2017==state_2018 ~ 1,
  TRUE ~ 0
))

# year one movers in 2018
payments_wide = mutate(payments_wide, yr1_move_2018=case_when(
  state_2016==state_2017 & state_2017!=state_2018 & state_2018==state_2019 ~ 1,
  TRUE ~ 0
))


# year two movers
# given 'likely movers' criteria, movers in year 2 is just year 1 shifted one period

payments_wide['yr2_move_2014'] = 0
payments_wide['yr2_move_2015'] = 0
payments_wide['yr2_move_2016'] = 0
payments_wide['yr2_move_2017'] = payments_wide['yr1_move_2016']
payments_wide['yr2_move_2018'] = payments_wide['yr1_move_2017']
payments_wide['yr2_move_2019'] = payments_wide['yr1_move_2018']


# year three movers
payments_wide['yr3_move_2014'] = 0
payments_wide['yr3_move_2015'] = 0
payments_wide['yr3_move_2016'] = 0
payments_wide['yr3_move_2017'] = 0

payments_wide = mutate(payments_wide, yr3_move_2018=case_when(
  state_2014==state_2015 & state_2015!=state_2016 &
  state_2016==state_2017 & state_2017==state_2018 ~ 1,
  TRUE ~ 0
))

payments_wide = mutate(payments_wide, yr3_move_2019=case_when(
  state_2015==state_2016 & state_2016!=state_2017 &
  state_2017==state_2018 & state_2018==state_2019 ~ 1,
  TRUE ~ 0
))


# year four movers
payments_wide['yr4_move_2014'] = 0
payments_wide['yr4_move_2015'] = 0
payments_wide['yr4_move_2016'] = 0
payments_wide['yr4_move_2017'] = 0
payments_wide['yr4_move_2018'] = 0

payments_wide = mutate(payments_wide, yr4_move_2019=case_when(
  state_2014==state_2015 & state_2015!=state_2016 &
  state_2016==state_2017 & state_2017==state_2018 & state_2018==state_2019 ~ 1,
  TRUE ~ 0
))

# save(payments_wide,file="payments_wide_moved.Rda")
# load('payments_wide_moved.Rda')


# ------------------------------------------------------------------------------
# 3.1 - Years before move (to get exposure estimates pre-move as validation test)

# year negative 1 (one year before move)
payments_wide['yrn1_move_2019'] = 0
payments_wide['yrn1_move_2018'] = 0
payments_wide['yrn1_move_2017'] = payments_wide['yr1_move_2018']
payments_wide['yrn1_move_2016'] = payments_wide['yr1_move_2017']
payments_wide['yrn1_move_2015'] = payments_wide['yr1_move_2016']
payments_wide['yrn1_move_2014'] = 0

# year negative 2 (two years before move)
payments_wide['yrn2_move_2019'] = 0
payments_wide['yrn2_move_2018'] = 0
payments_wide['yrn2_move_2017'] = 0
payments_wide['yrn2_move_2016'] = payments_wide['yr1_move_2018']
payments_wide['yrn2_move_2015'] = payments_wide['yr1_move_2017']
payments_wide['yrn2_move_2014'] = payments_wide['yr1_move_2016']

# year negative 3 - must confirm same state

payments_wide['yrn3_move_2019'] = 0
payments_wide['yrn3_move_2018'] = 0
payments_wide['yrn3_move_2017'] = 0
payments_wide['yrn3_move_2016'] = 0

payments_wide = mutate(payments_wide, yrn3_move_2015=case_when(
  state_2015==state_2016 & state_2016==state_2017 &
    state_2017!=state_2018 & state_2018==state_2019 ~ 1,
  TRUE ~ 0
))

payments_wide = mutate(payments_wide, yrn3_move_2014=case_when(
  state_2014==state_2015 & state_2015==state_2016 &
    state_2016!=state_2017 & state_2017==state_2018 ~ 1,
  TRUE ~ 0
))

# year negative 4 - must confirm same state
payments_wide['yrn4_move_2019'] = 0
payments_wide['yrn4_move_2018'] = 0
payments_wide['yrn4_move_2017'] = 0
payments_wide['yrn4_move_2016'] = 0
payments_wide['yrn4_move_2015'] = 0

payments_wide = mutate(payments_wide, yrn4_move_2014=case_when(
  state_2014==state_2015 & state_2015==state_2016 &
    state_2016==state_2017 & state_2017!=state_2018 & state_2018==state_2019 ~ 1,
  TRUE ~ 0
))

# save(payments_wide,file="payments_wide_moved.Rda")
# load('payments_wide_moved.Rda')


# ------------------------------------------------------------------------
# 4 - Get difference in old/new state means to assign exposure value

# get state means for each year
state_means <- payments %>%
  group_by(year, state) %>%
  summarize(state_mean=mean(phys_payment))




#--------------------------------------------------------------------------
# iteration 1.1 - year 1 movers in 2016

movers_wide_2016 <- payments_wide %>%
  filter(yr1_move_2016==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2016) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])

# get lists of current and past states
current_state <- movers_wide_2016['state_2016']  # current/new state
previous_state <- movers_wide_2016['state_2015'] # previous state

# match state names to state means
movers_wide_2016['current_state_mean_2016'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2016['previous_state_mean_2016'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2016['yr1_move_diff_2016'] = movers_wide_2016['current_state_mean_2016'] - movers_wide_2016['previous_state_mean_2016']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2016, Physician_Profile_ID, yr1_move_diff_2016),
                           by="Physician_Profile_ID")


#--------------------------------------------------------------------------
# iteration 1.2 - year 1 movers in 2017

movers_wide_2017 <- payments_wide %>%
  filter(yr1_move_2017==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2017) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2017['state_2017']  # current/new state
previous_state <- movers_wide_2017['state_2016'] # previous state

# match state names to state means
movers_wide_2017['current_state_mean_2017'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2017['previous_state_mean_2017'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2017['yr1_move_diff_2017'] = movers_wide_2017['current_state_mean_2017'] - movers_wide_2017['previous_state_mean_2017']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2017, Physician_Profile_ID, yr1_move_diff_2017),
                           by="Physician_Profile_ID")


#--------------------------------------------------------------------------
# iteration 1.3 - year 1 movers in 2018

movers_wide_2018 <- payments_wide %>%
  filter(yr1_move_2018==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2018) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2018['state_2018']  # current/new state
previous_state <- movers_wide_2018['state_2017'] # previous state

# match state names to state means
movers_wide_2018['current_state_mean_2018'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2018['previous_state_mean_2018'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2018['yr1_move_diff_2018'] = movers_wide_2018['current_state_mean_2018'] - movers_wide_2018['previous_state_mean_2018']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2018, Physician_Profile_ID, yr1_move_diff_2018),
                           by="Physician_Profile_ID")




#--------------------------------------------------------------------------
# iteration 2.1 - year 2 movers in 2017

movers_wide_2017_yr2 <- payments_wide %>%
  filter(yr2_move_2017==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2017) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2017_yr2['state_2017']  # current/new state
previous_state <- movers_wide_2017_yr2['state_2015'] # previous state (two periods ago)

# match state names to state means
movers_wide_2017_yr2['current_state_mean_2017'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2017_yr2['previous_state_mean_2017'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2017_yr2['yr2_move_diff_2017'] = movers_wide_2017_yr2['current_state_mean_2017'] - movers_wide_2017_yr2['previous_state_mean_2017']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2017_yr2, Physician_Profile_ID, yr2_move_diff_2017),
                           by="Physician_Profile_ID")


#--------------------------------------------------------------------------
# iteration 2.2 - year 2 movers in 2018

movers_wide_2018_yr2 <- payments_wide %>%
  filter(yr2_move_2018==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2018) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2018_yr2['state_2018']  # current/new state
previous_state <- movers_wide_2018_yr2['state_2016'] # previous state (two periods ago)

# match state names to state means
movers_wide_2018_yr2['current_state_mean_2018'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2018_yr2['previous_state_mean_2018'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2018_yr2['yr2_move_diff_2018'] = movers_wide_2018_yr2['current_state_mean_2018'] - movers_wide_2018_yr2['previous_state_mean_2018']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2018_yr2, Physician_Profile_ID, yr2_move_diff_2018),
                           by="Physician_Profile_ID")

#--------------------------------------------------------------------------
# iteration 2.3 - year 2 movers in 2019

movers_wide_2019_yr2 <- payments_wide %>%
  filter(yr2_move_2019==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2019) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2019_yr2['state_2019']  # current/new state
previous_state <- movers_wide_2019_yr2['state_2017'] # previous state (two periods ago)

# match state names to state means
movers_wide_2019_yr2['current_state_mean_2019'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2019_yr2['previous_state_mean_2019'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2019_yr2['yr2_move_diff_2019'] = movers_wide_2019_yr2['current_state_mean_2019'] - movers_wide_2019_yr2['previous_state_mean_2019']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2019_yr2, Physician_Profile_ID, yr2_move_diff_2019),
                           by="Physician_Profile_ID")





#--------------------------------------------------------------------------
# iteration 3.1 - year 3 movers in 2018

movers_wide_2018_yr3 <- payments_wide %>%
  filter(yr3_move_2018==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2018) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2018_yr3['state_2018']  # current/new state
previous_state <- movers_wide_2018_yr3['state_2015'] # previous state (three periods ago)

# match state names to state means
movers_wide_2018_yr3['current_state_mean_2018'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2018_yr3['previous_state_mean_2018'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2018_yr3['yr3_move_diff_2018'] = movers_wide_2018_yr3['current_state_mean_2018'] - movers_wide_2018_yr3['previous_state_mean_2018']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2018_yr3, Physician_Profile_ID, yr3_move_diff_2018),
                           by="Physician_Profile_ID")

#--------------------------------------------------------------------------
# iteration 3.2 - year 3 movers in 2019

movers_wide_2019_yr3 <- payments_wide %>%
  filter(yr3_move_2019==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2019) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2019_yr3['state_2019']  # current/new state
previous_state <- movers_wide_2019_yr3['state_2016'] # previous state (three periods ago)

# match state names to state means
movers_wide_2019_yr3['current_state_mean_2019'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2019_yr3['previous_state_mean_2019'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2019_yr3['yr3_move_diff_2019'] = movers_wide_2019_yr3['current_state_mean_2019'] - movers_wide_2019_yr3['previous_state_mean_2019']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2019_yr3, Physician_Profile_ID, yr3_move_diff_2019),
                           by="Physician_Profile_ID")


#--------------------------------------------------------------------------
# iteration 4.1 - year 4 movers in 2019

movers_wide_2019_yr4 <- payments_wide %>%
  filter(yr4_move_2019==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2019) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2019_yr4['state_2019']  # current/new state
previous_state <- movers_wide_2019_yr4['state_2015'] # previous state (three periods ago)

# match state names to state means
movers_wide_2019_yr4['current_state_mean_2019'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2019_yr4['previous_state_mean_2019'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2019_yr4['yr4_move_diff_2019'] = movers_wide_2019_yr4['current_state_mean_2019'] - movers_wide_2019_yr4['previous_state_mean_2019']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2019_yr4, Physician_Profile_ID, yr4_move_diff_2019),
                           by="Physician_Profile_ID")


# ------------------------------------------------------------------------------
# 4.2 - Repeat for prior to move exposures (for validation purposes)

# year minus one movers in 2017
movers_wide_2017_yrn1 <- payments_wide %>%
  filter(yrn1_move_2017==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2017) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2017_yrn1['state_2018']  # new state
previous_state <- movers_wide_2017_yrn1['state_2017'] # current/ previous state

# match state names to state means
movers_wide_2017_yrn1['new_state_mean_2017'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2017_yrn1['previous_state_mean_2017'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2017_yrn1['yrn1_move_diff_2017'] = movers_wide_2017_yrn1['new_state_mean_2017'] - movers_wide_2017_yrn1['previous_state_mean_2017']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2017_yrn1, Physician_Profile_ID, yrn1_move_diff_2017),
                           by="Physician_Profile_ID")


# year minus one movers in 2016
movers_wide_2016_yrn1 <- payments_wide %>%
  filter(yrn1_move_2016==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2016) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2016_yrn1['state_2017']  # new state
previous_state <- movers_wide_2016_yrn1['state_2016'] # current/ previous state

# match state names to state means
movers_wide_2016_yrn1['new_state_mean_2016'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2016_yrn1['previous_state_mean_2016'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2016_yrn1['yrn1_move_diff_2016'] = movers_wide_2016_yrn1['new_state_mean_2016'] - movers_wide_2016_yrn1['previous_state_mean_2016']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2016_yrn1, Physician_Profile_ID, yrn1_move_diff_2016),
                           by="Physician_Profile_ID")


# year minus one movers in 2015
movers_wide_2015_yrn1 <- payments_wide %>%
  filter(yrn1_move_2015==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2015) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2015_yrn1['state_2016']  # new state
previous_state <- movers_wide_2015_yrn1['state_2015'] # current/ previous state

# match state names to state means
movers_wide_2015_yrn1['new_state_mean_2015'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2015_yrn1['previous_state_mean_2015'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2015_yrn1['yrn1_move_diff_2015'] = movers_wide_2015_yrn1['new_state_mean_2015'] - movers_wide_2015_yrn1['previous_state_mean_2015']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2015_yrn1, Physician_Profile_ID, yrn1_move_diff_2015),
                           by="Physician_Profile_ID")



# year minus two movers in 2016
movers_wide_2016_yrn2 <- payments_wide %>%
  filter(yrn2_move_2016==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2016) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2016_yrn2['state_2018']  # new state
previous_state <- movers_wide_2016_yrn2['state_2016'] # current/ previous state

# match state names to state means
movers_wide_2016_yrn2['new_state_mean_2016'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2016_yrn2['previous_state_mean_2016'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2016_yrn2['yrn2_move_diff_2016'] = movers_wide_2016_yrn2['new_state_mean_2016'] - movers_wide_2016_yrn2['previous_state_mean_2016']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2016_yrn2, Physician_Profile_ID, yrn2_move_diff_2016),
                           by="Physician_Profile_ID")



# year minus two movers in 2015
movers_wide_2015_yrn2 <- payments_wide %>%
  filter(yrn2_move_2015==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2015) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2015_yrn2['state_2017']  # new state
previous_state <- movers_wide_2015_yrn2['state_2015'] # current/ previous state

# match state names to state means
movers_wide_2015_yrn2['new_state_mean_2015'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2015_yrn2['previous_state_mean_2015'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2015_yrn2['yrn2_move_diff_2015'] = movers_wide_2015_yrn2['new_state_mean_2015'] - movers_wide_2015_yrn2['previous_state_mean_2015']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2015_yrn2, Physician_Profile_ID, yrn2_move_diff_2015),
                           by="Physician_Profile_ID")


# year minus two movers in 2014
movers_wide_2014_yrn2 <- payments_wide %>%
  filter(yrn2_move_2014==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2014) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2014_yrn2['state_2016']  # new state
previous_state <- movers_wide_2014_yrn2['state_2014'] # current/ previous state

# match state names to state means
movers_wide_2014_yrn2['new_state_mean_2014'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2014_yrn2['previous_state_mean_2014'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2014_yrn2['yrn2_move_diff_2014'] = movers_wide_2014_yrn2['new_state_mean_2014'] - movers_wide_2014_yrn2['previous_state_mean_2014']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2014_yrn2, Physician_Profile_ID, yrn2_move_diff_2014),
                           by="Physician_Profile_ID")




# year minus three movers in 2015
movers_wide_2015_yrn3 <- payments_wide %>%
  filter(yrn3_move_2015==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2015) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2015_yrn3['state_2018']  # new state
previous_state <- movers_wide_2015_yrn3['state_2015'] # current/ previous state

# match state names to state means
movers_wide_2015_yrn3['new_state_mean_2015'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2015_yrn3['previous_state_mean_2015'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2015_yrn3['yrn3_move_diff_2015'] = movers_wide_2015_yrn3['new_state_mean_2015'] - movers_wide_2015_yrn3['previous_state_mean_2015']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2015_yrn3, Physician_Profile_ID, yrn3_move_diff_2015),
                           by="Physician_Profile_ID")


# year minus three movers in 2014
movers_wide_2014_yrn3 <- payments_wide %>%
  filter(yrn3_move_2014==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2014) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2014_yrn3['state_2017']  # new state
previous_state <- movers_wide_2014_yrn3['state_2014'] # current/ previous state

# match state names to state means
movers_wide_2014_yrn3['new_state_mean_2014'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2014_yrn3['previous_state_mean_2014'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2014_yrn3['yrn3_move_diff_2014'] = movers_wide_2014_yrn3['new_state_mean_2014'] - movers_wide_2014_yrn3['previous_state_mean_2014']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2014_yrn3, Physician_Profile_ID, yrn3_move_diff_2014),
                           by="Physician_Profile_ID")



# year minus four movers in 2014
movers_wide_2014_yrn4 <- payments_wide %>%
  filter(yrn4_move_2014==1)

# get state means for given year
state_means_lookup <- state_means %>%
  filter(year==2014) %>%
  ungroup(year) %>%
  select(state, state_mean)

# create new df to avoid error
state_means_lookup <- data.frame(state = state_means_lookup[,1],
                                 state_mean = state_means_lookup[,2])
# get lists of current and past states
current_state <- movers_wide_2014_yrn4['state_2018']  # new state
previous_state <- movers_wide_2014_yrn4['state_2014'] # current/ previous state

# match state names to state means
movers_wide_2014_yrn4['new_state_mean_2014'] <- lookup(current_state, key.match=state_means_lookup)   # match state means for current state in current yr
movers_wide_2014_yrn4['previous_state_mean_2014'] <- lookup(previous_state, key.match=state_means_lookup) # match state means for previous state in current yr
# get difference between old/new state in given year
movers_wide_2014_yrn4['yrn4_move_diff_2014'] = movers_wide_2014_yrn4['new_state_mean_2014'] - movers_wide_2014_yrn4['previous_state_mean_2014']

# add back to wide payments dataframe
payments_wide <- left_join(payments_wide, 
                           select(movers_wide_2014_yrn4, Physician_Profile_ID, yrn4_move_diff_2014),
                           by="Physician_Profile_ID")








# -------------------------------------------------------------------------
# 5 - Convert back to tall format and save

# convert back to tall format for analysis

payments_tall <- payments_wide %>%
  select(starts_with('phys_payment')) %>%
  pivot_longer(cols = starts_with('phys_payment_'),
               names_to = 'year',
               names_prefix = 'phys_payment_',
               values_to = 'payment') %>%
  drop_na()


states_tall <- payments_wide %>%
  select(starts_with('state_')) %>%
  pivot_longer(cols = starts_with('state_'),
               names_to = 'year',
               names_prefix = 'state_',
               values_to = 'state') %>%
  drop_na()


yr1_move_dif_tall <- payments_wide %>%
  select(starts_with('yr1_move_diff')) %>%
  pivot_longer(cols = starts_with('yr1_move_diff'),
               names_to = 'year',
               names_prefix = 'yr1_move_diff_',
               values_to = 'yr1_move_diff') %>%
  drop_na()


yr2_move_dif_tall <- payments_wide %>%
  select(starts_with('yr2_move_diff')) %>%
  pivot_longer(cols = starts_with('yr2_move_diff'),
               names_to = 'year',
               names_prefix = 'yr2_move_diff_',
               values_to = 'yr2_move_diff') %>%
  drop_na()


yr3_move_dif_tall <- payments_wide %>%
  select(starts_with('yr3_move_diff')) %>%
  pivot_longer(cols = starts_with('yr3_move_diff'),
               names_to = 'year',
               names_prefix = 'yr3_move_diff_',
               values_to = 'yr3_move_diff') %>%
  drop_na()

yr4_move_dif_tall <- payments_wide %>%
  select(starts_with('yr4_move_diff')) %>%
  pivot_longer(cols = starts_with('yr4_move_diff'),
               names_to = 'year',
               names_prefix = 'yr4_move_diff_',
               values_to = 'yr4_move_diff') %>%
  drop_na()

yrn1_move_dif_tall <- payments_wide %>%
  select(starts_with('yrn1_move_diff')) %>%
  pivot_longer(cols = starts_with('yrn1_move_diff'),
               names_to = 'year',
               names_prefix = 'yrn1_move_diff_',
               values_to = 'yrn1_move_diff') %>%
  drop_na()

yrn2_move_dif_tall <- payments_wide %>%
  select(starts_with('yrn2_move_diff')) %>%
  pivot_longer(cols = starts_with('yrn2_move_diff'),
               names_to = 'year',
               names_prefix = 'yrn2_move_diff_',
               values_to = 'yrn2_move_diff') %>%
  drop_na()

yrn3_move_dif_tall <- payments_wide %>%
  select(starts_with('yrn3_move_diff')) %>%
  pivot_longer(cols = starts_with('yrn3_move_diff'),
               names_to = 'year',
               names_prefix = 'yrn3_move_diff_',
               values_to = 'yrn3_move_diff') %>%
  drop_na()

yrn4_move_dif_tall <- payments_wide %>%
  select(starts_with('yrn4_move_diff')) %>%
  pivot_longer(cols = starts_with('yrn4_move_diff'),
               names_to = 'year',
               names_prefix = 'yrn4_move_diff_',
               values_to = 'yrn4_move_diff') %>%
  drop_na()



# combine dfs for each variable

# combine states and payments
payments_tall['state'] = states_tall['state']

# add variables for state mean differences
payments_clean <- left_join(payments_tall, 
                            yr1_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))

payments_clean <- left_join(payments_clean, 
                            yr2_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))

payments_clean <- left_join(payments_clean, 
                            yr3_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))

payments_clean <- left_join(payments_clean, 
                            yr4_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))


payments_clean <- left_join(payments_clean, 
                            yrn1_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))

payments_clean <- left_join(payments_clean, 
                            yrn2_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))

payments_clean <- left_join(payments_clean, 
                            yrn3_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))

payments_clean <- left_join(payments_clean, 
                            yrn4_move_dif_tall,
                            by=c('Physician_Profile_ID', 'year'))

# ----------------------------------------------------------------------
# 5.1 - Get first and last state for each physician 

# Initial state for each physician 
# this lists state associated with the smallest year (first) for each physician
first_last_state <- payments_clean %>%
  select(Physician_Profile_ID, year, state) %>%
  group_by(Physician_Profile_ID) %>%
  summarize(year=min(year), first_state=first(state), last_state=last(state)) %>%     # key line
  drop_na() %>%
  distinct(across(c(Physician_Profile_ID, first_state, last_state)))

payments_clean <- left_join(payments_clean,
                            select(first_last_state, Physician_Profile_ID, first_state, last_state),
                            by='Physician_Profile_ID')

# note: minor inaccuracy for physicians who move multiple times. 
# first state is accurate and is the only one used in calculations
# state mean differences do not reference these columns


# ----------------------------------------------------------------------
# save file
payments <- payments_clean
save(payments,file="payments_clean_tall.Rda")






# ----------------------------------------------------------------------
# 6 - Addressing large and numerous payments
# Removing physicians who accept very large sums of money
# If value of payments is large, threatens assumption of exogeneity
# Removed payments (and associated physicians) with any payments >$5000 or >200 payments per year (1 per working day) 

# full payments dataset
all_years_tidy <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/ECO2408 - Econometrics/Project/all_years_tidy.csv", header=T)

# get list of payment types
payment_types <- unique(all_years_tidy['Nature_of_Payment_or_Transfer_of_Value'])[[1]] # double brackets to get list, not df slice

payment_types[c(1,2,4,6,7)] # filter for the 5 low-value (<$200 ave) payment types
# should be: Gift, Travel & Lodging, Food & Bev, Education, Entertainment

large_payments <- all_years_tidy %>%
  filter((Nature_of_Payment_or_Transfer_of_Value == 'Gift' & Payment_value > 5000) |
           (Nature_of_Payment_or_Transfer_of_Value == 'Food and Beverage' & Payment_value > 5000) |
           (Nature_of_Payment_or_Transfer_of_Value == 'Education' & Payment_value > 5000) |
           (Nature_of_Payment_or_Transfer_of_Value == 'Travel and Lodging' & Payment_value > 5000) |
           (Nature_of_Payment_or_Transfer_of_Value == 'Entertainment' & Payment_value > 5000) |
           Payment_count > 200)

large_payment_id <- unique(large_payments[['Physician_Profile_ID']])

payments <- filter(payments, !(Physician_Profile_ID %in% large_payment_id))

gifts <- filter(all_years_tidy, Nature_of_Payment_or_Transfer_of_Value == 'Gift')
food <- filter(all_years_tidy, Nature_of_Payment_or_Transfer_of_Value == 'Food and Beverage')
edu <- filter(all_years_tidy, Nature_of_Payment_or_Transfer_of_Value == 'Education')
travel <- filter(all_years_tidy, Nature_of_Payment_or_Transfer_of_Value == 'Travel and Lodging')
entertain <- filter(all_years_tidy, Nature_of_Payment_or_Transfer_of_Value == 'Entertainment')

save(payments,file="payments_clean_tall.Rda")
save(gifts, food, edu, travel, entertain, file='payments_by_type.Rda')
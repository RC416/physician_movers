# create subsets of the payments dataset
# clean up all physicians dataset for analysis
# subset by no movebacks and no neighbors as more restrictive moving definition
# subset by popular starting state to allow state-level controls

library(fastDummies)
library(dplyr)
library(tidyr)

load("payments_clean_tall.Rda")

payments[is.na(payments)] <- 0 # set NA to 0 for non-movers

# ------------------------------------------------------------------------------
# 1. Set for All Physicians
# create dummies for state, region and division

# crosswalk for state, division and region
state_crosswalk <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/ECO2408 - Econometrics/Project/state_region_crosswalk.txt")
region_crosswalk <- select(state_crosswalk, State.Code, Region)
division_crosswalk <- select(state_crosswalk, State.Code, Division)

# match division and region to first_state
payments <- left_join(payments, division_crosswalk, by=c('first_state'='State.Code'))
payments <- left_join(payments, region_crosswalk, by=c('first_state'='State.Code'))

# create dummy columns
payments <- dummy_cols(payments, 
                       select_columns=c("first_state", "Region", "Division"),
                       remove_first_dummy=T)

# **Need to run twice** replace blank space in variable names
colnames(payments) <- sub(' ', '_', colnames(payments))
colnames(payments) <- sub(' ', '_', colnames(payments))

# dataset for all physician models
save(payments, file='payments_all_phys_mod_ready.Dta')


# ------------------------------------------------------------------------------
# 2. Only movers

# filter for only movers 
payments_movers <- filter(payments,
                          yr1_move_diff != 0 |
                          yr2_move_diff != 0 |
                          yr3_move_diff != 0 |
                          yr4_move_diff != 0)

save(payments_movers, file='payments_movers_mod_ready.Rda')



# ------------------------------------------------------------------------------
# 3. No move-backs
# exclude movers that list the same first and last state (move back to original state)

payments_no_movebacks <- filter(payments, 
                                (yr1_move_diff==0 & yr2_move_diff==0 &
                                   yr3_move_diff==0 & yr4_move_diff==0) | 
                                  (first_state != last_state))

save(payments_no_movebacks, file='payments_no_movebacks_mod_ready.Rda')


# ------------------------------------------------------------------------------
# 4. No neighbors 
# exclude movers that move to neighboring states

# get list of neighboring states
library(assertr)
state_neighbors <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/ECO2408 - Econometrics/Project/state_neighbors.txt")
state_neighbors['to_from'] <- col_concat(state_neighbors, sep=' to ')

payments['to_from'] = col_concat(select(ungroup(payments), first_state, last_state), sep=' to ')

payments_no_neighbors <- filter(payments,(yr1_move_diff==0 & yr2_move_diff==0 & yr3_move_diff==0 & yr4_move_diff==0) |
                                  !(to_from %in% state_neighbors[,'to_from'])) # syntax to get state_neighbors as list/vec

save(payments_no_neighbors, file='payments_no_neighbors_mod_ready.Rda')


# ------------------------------------------------------------------------------
# 5. No neighbors and no movebacks

payments_no_neighbors_no_movebacks <-  filter(payments_no_neighbors, 
                                              (yr1_move_diff==0 & yr2_move_diff==0 &
                                                 yr3_move_diff==0 & yr4_move_diff==0) | 
                                                (first_state != last_state))

save(payments_no_neighbors_no_movebacks, file='payments_no_neighbors_no_movebacks_mod_ready.Rda')



# ------------------------------------------------------------------------------
# 6. Top starting states

# load('payments_movers_mod_ready.Rda')
# load('payments_all_phys_mod_ready.Dta')

first_state_movers <- table(payments_movers['first_state']) %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  mutate(cumsum=cumsum(Freq), per=cumsum(Freq)/sum(Freq)*100)

top_5_first_state <- first_state_movers[1:5,'Var1']
top_10_first_state <- first_state_movers[1:10,'Var1']
top_20_first_state <- first_state_movers[1:20,'Var1']

payments_top_5 <- filter(payments, first_state %in% top_5_first_state)
payments_top_10 <- filter(payments, first_state %in% top_10_first_state)
payments_top_20 <- filter(payments, first_state %in% top_20_first_state)

save(payments_top_5, payments_top_10, payments_top_20,
     top_5_first_state, top_10_first_state, top_20_first_state,
     file='payments_top_first_state_mod_ready.Rda')


# get stats
# x <- filter(payments_top_20, yr1_move_diff != 0 | yr2_move_diff != 0 | yr3_move_diff != 0 | yr4_move_diff != 0)
# nrow(unique(x['Physician_Profile_ID']))


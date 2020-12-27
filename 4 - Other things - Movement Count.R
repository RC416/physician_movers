# Look at movers between specific sets of states

# state counts for all physicians
load("payments_clean_tall.Rda")

# get frequency counts of states
fs_tab <- table(payments['first_state'])
ls_tab <- table(payments['last_state'])

# create df
fs_ls_df <- as.data.frame(fs_tab)
fs_ls_df['last_state'] <- ls_tab
names(fs_ls_df) <- c('state', 'first_state_count', 'last_state_count')
fs_ls_df['net change'] = (fs_ls_df['first_state_count'] - fs_ls_df['last_state_count'])['first_state_count.Freq']

write.csv(fs_ls_df, 'first_last_state_count_allphys.csv')



# state counts for movers
load('payments_movers_clean_tall.Rda')

# get frequency counts of states
fs_mov_tab <- table(payments_movers['first_state'])
ls_mov_tab <- table(payments_movers['last_state'])

# create df
fs_ls_mov_df <- as.data.frame(fs_mov_tab)
fs_ls_mov_df['movers_last_state'] <- ls_mov_tab
names(fs_ls_mov_df) <- c('state', 'movers_first_state_count', 'movers_last_state_count')
fs_ls_mov_df['net change'] = (fs_ls_mov_df['movers_first_state_count'] - fs_ls_mov_df['movers_last_state_count'])['movers_first_state_count.Freq']

write.csv(fs_ls_mov_df, 'first_last_state_count_movers.csv')


# find most common movement to/from pairs
load('payments_movers_clean_tall.Rda')
library(assertr)

payments_movers['to/from'] = col_concat(select(ungroup(payments_movers),
                                               first_state, last_state),
                                        sep=' to ')

move_pairs <- as.data.frame(table(payments_movers['to/from']))



# no move-backs
p_m_nb <- filter(payments_movers, first_state!=last_state) #3488 "move-backs"
# nrow(p_m_nb) - nrow(payments_movers)
save(p_m_nb, file='payments_movers_no_movebacks.Rda')


# no moves to neighboring states
state_neighbors <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/ECO2408 - Econometrics/Project/state_neighbors.txt") # crosswalk of state neighbors
state_neighbors['to/from'] <- col_concat(state_neighbors, sep=' to ')

p_m_nn <- filter(payments_movers, to/from %in% state_neighbors['to/from'])


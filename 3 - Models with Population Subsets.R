# Run various models on subsets of the dataset:
# - movers-only
# - no movebacks (exclude movers who end up in the same state as they started)
# - no neighbors (exclude movers who move to neighboring states)
# - top 5/10/20 (include only the most popular starting states to allow state-level controls)

library(plm)

# load regression formulas from models with all physicians 
load('regression_formulas.Rda')


# ------------------------------------------------------------------------------
# 1. Only movers
# load data
load('payments_movers_mod_ready.Rda')

# OLS model
OLS_movers <- plm(formula_state,
                  data=payments_movers, index=c('Physician_Profile_ID', 'year'), effect='twoways',
                  model = 'pooling')

# HT model
HT_movers <- plm(formula_state,
                 data=payments_movers, index=c('Physician_Profile_ID', 'year'),
                 model = 'random', random.method='ht', inst.method='baltagi')

# FE model
FE_movers <- plm(payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff,
                 data=payments_movers,
                 index=c('Physician_Profile_ID', 'year'), effect='twoways',
                 model = 'within')

# RE model
RE_movers <- plm(formula_div, data=payments_movers,
                 index=c('Physician_Profile_ID', 'year'),
                 effect='twoways', model = 'random', random.method='walhus')

summary(OLS_movers)
summary(HT_movers)
summary(FE_movers)
summary(RE_movers)


# ------------------------------------------------------------------------------
# 2. No movebacks - exclude physicians who move but end in their initial state

# load data
load('payments_no_movebacks_mod_ready.Rda')

OLS1 <- plm(formula,
           data=payments_no_movebacks,
           index=c('Physician_Profile_ID', 'year'), effect='twoways',
           model = 'pooling') 

OLS2 <- plm(formula_div,
            data=payments_no_movebacks,
            index=c('Physician_Profile_ID', 'year'), effect='twoways',
            model = 'pooling') 

FE_no_movebacks <- plm(formula,
                       data=payments_no_movebacks,
                       index=c('Physician_Profile_ID', 'year'), effect='twoways',
                       model = 'within')

HT_no_movebacks <- plm(formula_div,
                       data=select(payments_no_movebacks, Physician_Profile_ID, year, payment, starts_with('yr'), starts_with('Division_')),
                       index=c('Physician_Profile_ID', 'year'),
                       model = 'random', random.method='ht', inst.method='baltagi')

RE_no_movebacks1 <- plm(formula, data=payments_no_movebacks,
                        index=c('Physician_Profile_ID', 'year'),
                        effect='twoways', model = 'random')

RE_no_movebacks2 <- plm(formula_div, data=payments_no_movebacks,
                        index=c('Physician_Profile_ID', 'year'),
                        effect='twoways', model = 'random', random.method = 'walhus')
summary(OLS1)
summary(OLS2)
summary(FE_no_movebacks)
summary(HT_no_movebacks)
summary(RE_no_movebacks1)
summary(RE_no_movebacks2)

# ------------------------------------------------------------------------------
# 3. No neighbors - exclude physicians who move to neighboring states

# load data
load('payments_no_neighbors_mod_ready.Rda')

OLS1 <- plm(formula,
            data=payments_no_neighbors,
            index=c('Physician_Profile_ID', 'year'), effect='twoways',
            model = 'pooling') 

OLS2 <- plm(formula_div,
            data=payments_no_neighbors,
            index=c('Physician_Profile_ID', 'year'), effect='twoways',
            model = 'pooling') 


FE_no_neighbors <- plm(payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff,
                       data=payments_no_neighbors,
                       index=c('Physician_Profile_ID', 'year'), effect='twoways',
                       model = 'within')

HT_no_neighbors <- plm(formula_div,
                       data=payments_no_neighbors, index=c('Physician_Profile_ID', 'year'),
                       model = 'random', random.method='ht', inst.method='baltagi')

RE_no_neighbors1 <- plm(formula, data=payments_no_neighbors,
                       index=c('Physician_Profile_ID', 'year'),
                       effect='twoways', model = 'random')


RE_no_neighbors2 <- plm(formula_div, data=payments_no_neighbors,
                       index=c('Physician_Profile_ID', 'year'),
                       effect='twoways', model = 'random', random.method='walhus')

summary(OLS1)
summary(OLS2)
summary(FE_no_neighbors)
summary(HT_no_neighbors)
summary(RE_no_neighbors1)
summary(RE_no_neighbors2)

# ------------------------------------------------------------------------------
# 4. No neighbors & No movebacks - exclude both groups

# load data
load('payments_no_neighbors_no_movebacks_mod_ready.Rda')

FE_no_neighbors_no_movebacks <- plm(payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff,
                                    data=payments_no_neighbors_no_movebacks,
                                    index=c('Physician_Profile_ID', 'year'), effect='twoways',
                                    model = 'within')

HT_no_neighbors_no_movebacks <- plm(formula_div,
                                    data=payments_no_neighbors_no_movebacks,
                                    index=c('Physician_Profile_ID', 'year'),
                                    model = 'random', random.method='ht', inst.method='baltagi')

RE_no_neighbors_no_movebacks <- plm(formula, data=payments_no_neighbors_no_movebacks,
                                    index=c('Physician_Profile_ID', 'year'),
                                    effect='twoways', model = 'random')

summary(FE_no_neighbors_no_movebacks)
summary(HT_no_neighbors_no_movebacks)
summary(RE_no_neighbors_no_movebacks)


# ------------------------------------------------------------------------------
# 5. Top first_states

# load data
load('payments_top_first_state_mod_ready.Rda')

# write regression formulas
formula_top_5 <- paste('payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff',
                       paste('first_state', top_5_first_state, collapse=' + ', sep='_'),
                       sep=' + ')
formula_top_10 <- paste('payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff',
                        paste('first_state', top_10_first_state, collapse=' + ', sep='_'),
                        sep=' + ')
formula_top_20 <- paste('payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff',
                        paste('first_state', top_20_first_state, collapse=' + ', sep='_'),
                        sep=' + ')


# OLS models with no controls
OLS_top_5 <- plm(formula_top_5, data=payments_top_5,
                index=c('Physician_Profile_ID', 'year'), effect='twoways',
                model = 'pooling')
OLS_top_10 <- plm(formula_top_10, data=payments_top_10,
                 index=c('Physician_Profile_ID', 'year'), effect='twoways',
                 model = 'pooling')
OLS_top_20 <- plm(formula_top_20, data=payments_top_20,
                 index=c('Physician_Profile_ID', 'year'), effect='twoways',
                 model = 'pooling')

summary(OLS_top_5)
summary(OLS_top_10)
summary(OLS_top_20)


# RE models with state-level controls
RE_top_5 <- plm(formula_top_5, data=payments_top_5,
                index=c('Physician_Profile_ID', 'year'), effect='twoways',
                model = 'random', random.method = 'walhus')
RE_top_10 <- plm(formula_top_10, data=payments_top_10,
                 index=c('Physician_Profile_ID', 'year'), effect='twoways',
                 model = 'random', random.method = 'walhus')
RE_top_20 <- plm(formula_top_20, data=payments_top_20,
                 index=c('Physician_Profile_ID', 'year'), effect='twoways',
                 model = 'random', random.method = 'walhus')

summary(RE_top_5)
summary(RE_top_10)
summary(RE_top_20)


# HT models with state-level controls
HT_top_5 <- plm(formula_top_5, data=payments_top_5,
                index=c('Physician_Profile_ID', 'year'),
                model = 'random', random.method='ht', inst.method='baltagi')

HT_top_10 <- plm(formula_top_10, data=payments_top_10,
                index=c('Physician_Profile_ID', 'year'),
                model = 'random', random.method='ht', inst.method='baltagi')

HT_top_20 <- plm(formula_top_20, data=payments_top_20,
                index=c('Physician_Profile_ID', 'year'),
                model = 'random', random.method='ht', inst.method='baltagi')

summary(HT_top_5)
summary(HT_top_10)
summary(HT_top_20)


# FE models with no controls
FE_top_5 <- plm(formula, data=payments_top_5,
                index=c('Physician_Profile_ID', 'year'), effect='twoways',
                model = 'within')
FE_top_10 <- plm(formula, data=payments_top_10,
                index=c('Physician_Profile_ID', 'year'), effect='twoways',
                model = 'within')
FE_top_20 <- plm(formula, data=payments_top_20,
                index=c('Physician_Profile_ID', 'year'), effect='twoways',
                model = 'within')

summary(FE_top_5)
summary(FE_top_10)
summary(FE_top_20)

save(RE_top_5, RE_top_10, RE_top_20,
     FE_top_5, FE_top_10, FE_top_20,
     HT_top_5, HT_top_10, HT_top_20,
     file='models_top_5_10_20.Rda')


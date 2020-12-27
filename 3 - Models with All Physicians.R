# Models with All Physicians
# relies on payments dataset with all dummy variables
# relies on formulas written in other files

# Specifications:
# all physicians, no first_state (0 dummies)
# all physicians, first division dummies (9 divisions + Puerto Rico)
# all physicians, pre-move controls, no first state controls
# all physicians, pre-move controls, first division dummies

# run FE, OLS and RE for each specification

library(plm)

# load data
load('payments_all_phys_mod_ready.Dta')
load('regression_formulas.Rda')


# ------------------------------------------------------------------------------
# 1. All physicians, no first_state

# FE model
FE_all_no_state <- plm(payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff,
                       data=payments, index=c('Physician_Profile_ID', 'year'), effect='twoways',
                       model = 'within')

# OLS model
OLS_all_no_state <- plm(payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff,
                        data=payments, index=c('Physician_Profile_ID', 'year'), effect='twoways',
                        model = 'pooling')

# HT model
HT_all_no_state <- plm(payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff,
                       data=payments, index=c('Physician_Profile_ID', 'year'),
                       model = 'random', random.method='ht', inst.method='baltagi', effect='twoways')

# RE model
RE_all_no_state <- plm(payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff,
                       data=payments,
                       index=c('Physician_Profile_ID', 'year'),
                       effect='twoways',
                       model = 'random', random.method = 'walhus')


# ------------------------------------------------------------------------------
# 2. All physicians, using starting division

OLS_all_div <- plm(formula_div, data=payments,
                   index=c('Physician_Profile_ID', 'year'),
                   effect='twoways', model = 'pooling')

RE_all_div <- plm(formula_div, data=payments,
                  index=c('Physician_Profile_ID', 'year'),
                  effect='twoways', model = 'random', random.method='walhus')

HT_all_div <- plm(formula_div, data=payments,
                  index=c('Physician_Profile_ID', 'year'),
                  model = 'random', random.method='ht', inst.method='baltagi')


# ------------------------------------------------------------------------------
# 3. All physicians, pre-move controls, no first state controls


f_pre_move <- 'payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff + yrn1_move_diff + yrn2_move_diff + yrn3_move_diff + yrn4_move_diff'

# FE model
FE_all_no_state <- plm(f_pre_move, data=payments,
                       index=c('Physician_Profile_ID', 'year'), effect='twoways',
                       model = 'within')

# OLS model
OLS_all_no_state <- plm(f_pre_move, data=payments,
                        index=c('Physician_Profile_ID', 'year'), effect='twoways',
                        model = 'pooling')

# RE model
RE_all_no_state <- plm(f_pre_move, data=filter(payments, payment<5000),
                       index=c('Physician_Profile_ID', 'year'),
                       effect='twoways',
                       model = 'random', random.method = 'walhus')


# ------------------------------------------------------------------------------
# 4. All physicians, pre-move controls, first div controls

f_pre_move_div <- 'payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff + yrn1_move_diff + yrn2_move_diff + yrn3_move_diff + yrn4_move_diff + Division_East_South_Central + Division_Middle_Atlantic + Division_Mountain + Division_New_England + Division_Pacific + Division_PR + Division_South_Atlantic + Division_West_North_Central + Division_West_South_Central'
f_pre_move <- 'payment ~ yr1_move_diff + yr2_move_diff + yr3_move_diff + yr4_move_diff + yrn1_move_diff + yrn2_move_diff + yrn3_move_diff + yrn4_move_diff'


# FE model
FE <- plm(f_pre_move_div, data=payments,
                       index=c('Physician_Profile_ID', 'year'), effect='twoways',
                       model = 'within')

# OLS model
OLS <- plm(f_pre_move_div, data=payments,
                        index=c('Physician_Profile_ID', 'year'), effect='twoways',
                        model = 'pooling')

# RE model
RE <- plm(f_pre_move_div, data=payments,
                       index=c('Physician_Profile_ID', 'year'),
                       effect='twoways',
                       model = 'random', random.method = 'walhus')


# ------------------------------------------------------------------------------
# A. Statistical tests

phtest(FE_all_no_state, RE_all_no_state)
phtest(FE_all_no_state, OLS_all_no_state)
phtest(RE_all_no_state, OLS_all_no_state)

phtest(OLS_all_div, RE_all_div)
phtest(HT_all_div, RE_all_div)
phtest(OLS_all_div, HT_all_div)

phtest(FE_all_no_state, FE_no_neighbors)
phtest(RE_all_div, RE_no_neighbors2)
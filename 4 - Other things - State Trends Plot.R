# uses dataset of all payments by physician and payment type for each year
# filters out large payments
# looks at state-specific trends in small payments

library(dplyr)
library(ggplot2)

all_years_tidy <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/ECO2408 - Econometrics/Project/all_years_tidy.csv", header=T)

# ----------------------------------------------------------------------
# 1 - prep data
# filter to get only the desired payments (small payments)
# sum by annual total payments by physician. then look at state level trends.

# get list of payment types
payment_types <- unique(all_years_tidy['Nature_of_Payment_or_Transfer_of_Value'])[[1]] # double brackets to get list not df slice

# payment_types[c(1,2,4,6,7)] # filter for the 5 low-value (<$200 ave) payment types
# Gift, Travel & Lodging, Food & Bev, Education, Entertainment

# payments at physician level
phys_full_yr_grouped_lowval <- all_years_tidy %>%
  filter(Nature_of_Payment_or_Transfer_of_Value %in% payment_types[c(1,2,4,6,7)]) %>%
  group_by(year, Physician_Profile_ID) %>%
  summarize(phys_payment=sum(Payment_value),  # sum of all payments in given year
            n_payments=n(),                   # number of payments in given year
            state=Recipient_State) %>%
  distinct(.keep_all=T) # gets rid of duplicate rows produced by group_by

# payments at state level
state_means <- phys_full_yr_grouped_lowval %>%
  group_by(year, state) %>%
  summarize(state_mean=mean(phys_payment))

#---------------------------------------------------------------------------
# 2 - create plots

# plot state means
mean_plot <- ggplot(state_means, aes(x=year, y=state_mean))+
  # geom_line()+
  geom_point(size=1.5, alpha=0.4)+
  geom_quantile(quantiles=seq(0,1,by=0.2),          # quantiles = (0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
                color='black', alpha=0.8, size=0.3)+ 
  #geom_errorbar(aes(ymin=state_means_mean-state_mean_sd,
  #                  ymax=state_means_mean+state_mean_sd), width=0.2)+
  ylim(0,1500)+
  labs(x='Year', y='Mean annual payment to physician ($)')+
  theme_classic()
mean_plot





# ----------------------------------------------------------------------
# other useful code
nrow(unique(phys_full_yr_grouped_lowval['Physician_Profile_ID'])) # unique physicians
# Load datasets 2014 - 2019 and collects summary statistics.
# works with the very large individual payments files. 
# aggregate ~10M yearly patients by doctor and
# collect summary statistics on individual payments
# output is sum & count of payment by physician ID

library(dplyr)
library(ggplot2)

# load payments for each year - must do one at a time given size
payments_2013 <- read.csv("C:/Users/Ray/OneDrive/Economics/Economics datasets/CMS Open Payments/2013/OP_DTL_GNRL_PGYR2013_P06302020.csv", header=T)
payments_2014 <- read.csv("C:/Users/Ray/OneDrive/Economics/Economics datasets/CMS Open Payments/2014/OP_DTL_GNRL_PGYR2014_P06302020.csv", header=T)
payments_2015 <- read.csv("C:/Users/Ray/OneDrive/Economics/Economics datasets/CMS Open Payments/2015/OP_DTL_GNRL_PGYR2015_P06302020.csv", header=T)
payments_2016 <- read.csv("C:/Users/Ray/OneDrive/Economics/Economics datasets/CMS Open Payments/2016/OP_DTL_GNRL_PGYR2016_P06302020.csv", header=T)
payments_2017 <- read.csv("C:/Users/Ray/OneDrive/Economics/Economics datasets/CMS Open Payments/2017/OP_DTL_GNRL_PGYR2017_P06302020.csv", header=T)
payments_2018 <- read.csv("C:/Users/Ray/OneDrive/Economics/Economics datasets/CMS Open Payments/2018/OP_DTL_GNRL_PGYR2018_P06302020.csv", header=T)
payments_2019 <- read.csv("C:/Users/Ray/OneDrive/Economics/Economics datasets/CMS Open Payments/2019/OP_DTL_GNRL_PGYR2019_P06302020.csv", header=T)


# --------------------------------------------------------------------
# collapse by Physician_Profile_ID and Nature_of_Payment for each year

payments <- payments_2015

# select relevant columns, group by unique physician and payment type
# repeat for each year of observations
docs_payments <- payments %>%
  filter(!is.na(Physician_Profile_ID)) %>% # filter blank physician id's
  select(Physician_Profile_ID, Recipient_State, Recipient_Zip_Code, Physician_Primary_Type,
         Physician_Specialty, Total_Amount_of_Payment_USDollars, Nature_of_Payment_or_Transfer_of_Value) %>%
  group_by(Physician_Profile_ID, Nature_of_Payment_or_Transfer_of_Value) %>%
  summarize(Payment_value=sum(Total_Amount_of_Payment_USDollars),
            Payment_count=n(),
            Recipient_State=first(Recipient_State), Recipient_Zip_Code=first(Recipient_Zip_Code),
            Physician_Primary_Type=first(Physician_Primary_Type), Physician_Specialty=first(Physician_Specialty))

# save to file to prevent having to load the large payment datasets
write.csv(docs_payments, 'docs_2015.csv', row.names=F)


# ---------------------------------------------------------------------
# Descriptive statistics for payment data
# get payment types, total payments, mean, median, count, st.dev

payments <- payments_2015

docs_payment_types <- payments %>%
  select(Physician_Profile_ID, Recipient_State, Recipient_Zip_Code, Physician_Primary_Type,
         Physician_Specialty, Total_Amount_of_Payment_USDollars, Nature_of_Payment_or_Transfer_of_Value) %>%
  group_by(Nature_of_Payment_or_Transfer_of_Value) %>%
  summarize(payment_sum=sum(Total_Amount_of_Payment_USDollars),
            payment_mean=mean(Total_Amount_of_Payment_USDollars),
            payment_median=median(Total_Amount_of_Payment_USDollars),
            payment_sd=sd(Total_Amount_of_Payment_USDollars),
            payment_count=n())

write.csv(docs_payment_types, '2015_count.csv', row.names=F)


# ----------------------------------------------------------------------
# plot distribution of payments by payment type for each year 
# update payment_types and year for save file
# for testing: mini_payments <- slice(payments, 1:1000)

payment_types <- docs_payment_types[['Nature_of_Payment_or_Transfer_of_Value']] # double brackets to get list not df slice

# create graph for each payment type
for (payment_type in payment_types) {
  
  # get plot data and exclude the extreme outliers
  xlimit <- payments %>%
    filter(Nature_of_Payment_or_Transfer_of_Value==payment_type) %>%
    pull(var='Total_Amount_of_Payment_USDollars') %>%
    quantile(0.99)
  
  plot_data <- payments %>%
    filter(Nature_of_Payment_or_Transfer_of_Value==payment_type,
           Total_Amount_of_Payment_USDollars < xlimit)
  
  # create plot
  temp_plot <- ggplot(data=plot_data, aes(Total_Amount_of_Payment_USDollars)) +
    geom_density()+
    labs(x=payment_type)+
    theme_classic()
  
  # save plot
  ggsave(plot=temp_plot, filename=paste('2015_',payment_type,'.png',sep=""), device='png',
         path='C:/Users/Ray/OneDrive/Economics/Course Material/ECO2408 - Econometrics/Project/Data tables/Plots')
}


# ---------------------------------------------------------------------
# other useful code

# count unique physicians
nrow(unique(docs_2013['Physician_Profile_ID'])) # unique physicians

# confirm nature of payment values has not changed
unique(docs_2013['Nature_of_Payment_or_Transfer_of_Value'])
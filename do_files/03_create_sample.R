# Top commands ----
# Create empty R application (no figures, data frames, packages, etc.)
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/GitHub/latner_2018/")

data_files = "data_files/"

# LIBRARY
library(tidyverse)
library(broom) # augment

# LOAD DATA ----

df_psid <- readRDS(file = paste0(data_files,"psid_clean.rds"))

# keep only src sample after 1969
df_psid <- df_psid %>% 
        arrange(pid, year) %>%
        filter(src == 1) %>%
        filter(year>1969)

# Create study period ----

test <- c(seq(1970, 1987, 1))
test2 <- c(seq(1989, 2003, 2))
year <- as.vector(append(test, test2))
rm(test,test2)

for(c in year) {
        z <- filter(df_psid, year >= c & year <= c + 10)
        
        #keep if positive earnings
        z <- filter(z, wages_hd >= 100)
        #keep if positive hours worked
        z <- filter(z, hours_hd > 0)
        
        #keep if employed or looking for work
        z <- filter(z, emp == 1) %>% select(-emp)
        
        z <- filter(z, age <= 64)
        
        #keep if in the sample each year in the study period
        z <- group_by(z, pid) %>%
                mutate(test = first(age)) %>%
                ungroup() %>%
                filter(test >= 25 & test <= 54) %>%
                select(-test)
        
        #transform wages into log form
        z <- select(z, -wages_hd)
        z <- rename(z, wages_hd = wages_hd_adj)
        z <- mutate(z, wages_hd = log(wages_hd))
        
        #keep if household head
        z <- filter(z, head == 1)
        z <- select(z, -head)
        
        #keep if in the sample each year in the study period
        z <- group_by(z, pid) %>%
                mutate(count = row_number(), count = last(count), study_period = min(year)) %>%
                ungroup() %>%
                mutate(max = max(count)) %>%
                filter(max == count) %>%
                select(-count)
        z <- arrange(z, pid, year)
        assign(paste0("yr_", c), z)
}

rm(df_period,z)        


# Append data sets ----

df_sample = data.frame()
for(c in year) {
        df_test <- get(paste0("yr_", c))
        df_sample <- rbind(df_sample,df_test)
        rm(df_test)
}

for(c in year) {
        rm(list=paste0("yr_", c))
}

# DEMOGRAPHICS (education, race, age, cohort weight) ----
 
df_sample <- df_sample %>% 
        arrange(year, pid, year)

#replace education with education in first year of the study period
#replace race with race in first year of the study period
#replace age_cat with age_cat in first year of the study period
##this variable is a bit messed up because age is not always sequential (pid == 840001)
##group_by(age_cat) %>% summarize(min = min(age), max = max(age))
#replace cohort group with cohort group in first year of the study period
#replace weight group with weight in first year of the study period
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(education = first(education),
               white = first(white),
               age_cat = first(age_cat),
               age = first(age),
               cohgr = first(cohgr),
               weight_ind_long = first(weight_ind_long),
               weight_fam = first(weight_fam)) %>%
        ungroup()

# FAMILY ----
#single/married
df_sample <- df_sample %>% 
        mutate(married =(ifelse(marital >= 2, yes = 0, no = 1)))

#always married == 3
#sometimes married == 2
#never married (i.e. always single) == 1
#always married
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(test = sum(married), marr_chng = ifelse(test == max, yes = 3,
                                                       ifelse(test == 0, yes = 1, no = 2))) %>%
        ungroup()

#kids/no kids
df_sample <- df_sample %>% 
        mutate(kids =ifelse(kids>= 1, yes = 1, no = 0))

#always kids == 3
#sometimes kids == 2
#never kids == 1
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(test = sum(kids), 
               kids_chng = ifelse(test == max, yes = 3, 
                                  ifelse(test == 0, yes = 2, no = 1))) %>%
        ungroup()

# drops non-relevant family variables
df_sample <- df_sample %>% 
        select(-kids, -married, -marital)

# WORK ----

#ever employed
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(test = sum(unemp_lastyr_hd), unemp = ifelse(test == 0, yes = 0, no = 1)) %>%
        ungroup()

#ever self employed
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(test = sum(selfemp_lastyr_hd), self = ifelse(test == 0, yes = 0, no = 1)) %>%
        ungroup()

# drops non-relevant work variables
df_sample <- df_sample %>% 
        select(-selfemp_lastyr_hd, -unemp_lastyr_hd, -student, -retired, -weeks_hd, -weeks_wf, -hours_hd, -hours_wf, -cpi_u_rs)

# VOLATILITY ----

df_sample <- arrange(df_sample, study_period, pid, year)

#AGE-EARNINGS PROFILE
regression <- df_sample %>%
        group_by(study_period) %>%
        do(augment(lm(wages_hd ~ factor(year), data=.), data = .)) %>%
        ungroup()
df_sample$uhat <- as.numeric(regression$.resid)
summary(df_sample$uhat)

#YEAR-ADJUSTED TREND LINE
regression <- df_sample %>%
        group_by(study_period,pid) %>%
        do(augment(lm(uhat ~ year, data = .), data = .)) %>%
        ungroup()
df_sample$yhat <- as.numeric(regression$.fitted)
summary(df_sample$yhat)

df_sample_test <- select(df_sample,study_period,pid,year,wages_hd,uhat,yhat)
df_sample_test <- merge(df_sample_test,df_test)

#YEAR^2-ADJUSTED TREND LINE
regression <- df_sample %>%
        group_by(study_period,pid) %>%
        do(augment(lm(uhat ~ poly(year, degree = 2, raw = TRUE), data=.), data = .)) %>%
        ungroup()
df_sample$yhat_2 <- as.numeric(regression$.fitted)

#MEAN
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(mline = mean(uhat)) %>%
        ungroup()

#VOLATILITY
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(std = sd(uhat), 
               volatility = sd(uhat - yhat), 
               volatility_2 = sd(uhat - yhat_2),
               sd_wages_hd = sd(wages_hd),
        ) %>%
        ungroup()

# MOBILITY ----

#YEAR-ADJUSTED TREND LINE
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(slope = last(yhat) - first(yhat)) %>%
        ungroup()

#YEAR^2-ADJUSTED TREND LINE
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(slope_2 = last(yhat_2) - first(yhat_2)) %>%
        ungroup()

#INCOME AT START
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(income_beg = (nth(uhat, 1) + nth(uhat, 2))/2,
               income_beg_unadj = (nth(wages_hd, 1) + nth(wages_hd, 2))/2) %>%
        ungroup()

#INCOME AT END
df_sample <- df_sample %>% 
        group_by(study_period, pid) %>%
        mutate(income_end_1 = (last(uhat)), income_end_2 = lag(uhat, n = 1), income_end_2 = last(income_end_2), income_end = (income_end_1 + income_end_2)/2) %>%
        select(-income_end_1, -income_end_2) %>%
        ungroup()

#INCOME MOBILITY (UNADJUSTED)
df_sample <- df_sample %>% 
        mutate(income_diff = (income_end - income_beg)*100,
               quartile_beg = ntile(income_beg, 4),
               quartile_end = ntile(income_end, 4))

vars <- c("slope", "slope_2", "std", "volatility", "volatility_2", "income_beg", "income_end")
for(i in vars){
        df_sample[[i]] <- df_sample[[i]]*100
}

df_sample <- df_sample %>% 
        select(-max, yhat, -yhat_2, -mline)

#YEAR-ADJUSTED TREND LINE
regression <- df_sample %>%
        group_by(study_period, pid) %>%
        do(augment(lm(wages_hd ~ year, data = .), data = .))
df_sample$yhat_unadjusted <- as.numeric(regression$.fitted)

#Save ----

# for HLM analysis
saveRDS(df_sample, file = paste0(data_files, "psid_clean_sample_mixed.rds"))

# Make cross-sectional for cross-sectional analysis

df_sample_xs <- df_sample %>% 
        group_by(study_period, pid) %>%
        filter(row_number()==1) %>%
        ungroup()

saveRDS(df_sample_xs, file = paste0(data_files, "psid_clean_sample.rds"))


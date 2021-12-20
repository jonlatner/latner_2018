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
tables = "tables/"

# LIBRARY
library(tidyverse)
library(stargazer)
library(lme4) # HLM
library(numDeriv) #hessian

# NO SCIENTIFIC NOTATION
options(scipen=999)

# MANUAL FUNCTION
insertrow <- function(existingDF, newrow, r) {
        existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
        existingDF[r,] <- newrow
        existingDF
}

# LOAD DATA ----

x <- readRDS(file = paste0(data_files,"psid_clean_sample_mixed.rds"))
x <- select(x, pid, year, study_period, uhat, wages_hd, slope_2)


# CLEAN DATA ----

# CENTER YEAR
x <- group_by(x, study_period, pid) %>%
        mutate(center = mean(year), cyear = year - center, cyear_2 = cyear*cyear) %>%
        ungroup()

x <- arrange(x, study_period, pid, year)

x <- mutate(x, up = ifelse(slope_2 > 0, yes = cyear, no = NA))
x <- mutate(x, dn = ifelse(slope_2 < 0, yes = cyear, no = NA))
x <- mutate(x, up_2 = ifelse(slope_2 > 0, yes = cyear_2, no = NA))
x <- mutate(x, dn_2 = ifelse(slope_2 < 0, yes = cyear_2, no = NA))

# HLM REGRESSION ----

#standard
summary(M1 <- lmer(uhat ~ 1 + (1|study_period) + (1|study_period:pid), data = x, REML = FALSE))
summary(M2 <- lmer(uhat ~ 1 + (1|study_period) + (1|study_period:pid) + (0 + cyear|study_period:pid), data = x, REML = FALSE))
summary(M3 <- lmer(uhat ~ 1 + (1|study_period) + (1|study_period:pid) + (0 + cyear|study_period:pid) + (0 + cyear_2|study_period:pid), data = x, REML = FALSE))

#downward
summary(M4 <- lmer(uhat ~ 1 + (1|study_period) + (1|study_period:pid) + (0 + dn|study_period:pid), data = x, REML = FALSE))
summary(M5 <- lmer(uhat ~ 1 + (1|study_period) + (1|study_period:pid) + (0 + dn|study_period:pid) + (0 + dn_2|study_period:pid), data = x, REML = FALSE))

#updward
summary(M6 <- lmer(uhat ~ 1 + (1|study_period) + (1|study_period:pid) + (0 + up|study_period:pid), data = x, REML = FALSE))
summary(M7 <- lmer(uhat ~ 1 + (1|study_period) + (1|study_period:pid) + (0 + up|study_period:pid) + (0 + up_2|study_period:pid), data = x, REML = FALSE))

# POST ESTIMATION ----

for(i in 1:7) {
        y <- get(paste0("M",i))
        dd.ML <- lme4:::devfun2(y,useSc=TRUE,signames=FALSE)
        vv <- as.data.frame(VarCorr(y)) ## need ML estimates!
        pars <- vv[,"sdcor"]
        hh1 <- hessian(dd.ML,pars)
        vv2 <- 2*solve(hh1)  ## 2* converts from log-likelihood to deviance scale
        se <- sqrt(diag(vv2))  ## get standard errors
        assign(paste0("M", i, "_se"), se)
        
        #Beta and SE for grand mean
        z <- fixef(y)
        assign(paste0("M", i, "_grand_mean_b"), z)
        z <- vcov(y)
        assign(paste0("M", i, "_grand_mean_se"), z)
        
        #Beta and SE for study period mean
        z <- attributes(VarCorr(y)$"study_period")$stddev
        assign(paste0("M", i, "_study_period_b"), z)
        z <- se[0 + i]
        assign(paste0("M", i, "_study_period_se"), z)
        
        #Beta and SE for residual
        z <- attr(VarCorr(y), "sc")
        assign(paste0("M", i, "_residual_b"), z)
        z <- se[2 + i]
        assign(paste0("M", i, "_residual_se"), z)
        
        rm(hh1, vv, vv2, se, z, y)
}

# R-SQUARED
# The R2 is the ratio between the variance of the residual in the subsequent model(s) compared to the variance of the residual in the unconditional model with no additional explanatory variables.
# (Bryk and Raudenbush, 1992, pg. 72).
r2_year <- (M1_residual_b^2 - M2_residual_b^2)/(M1_residual_b^2)
r2_year_2 <- (M1_residual_b^2 - M3_residual_b^2)/(M1_residual_b^2)

r2_year_dn <- (M1_residual_b^2 - M4_residual_b^2)/(M1_residual_b^2)
r2_year_2_dn <- (M1_residual_b^2 - M5_residual_b^2)/(M1_residual_b^2)

r2_year_up <- (M1_residual_b^2 - M6_residual_b^2)/(M1_residual_b^2)
r2_year_2_up <- (M1_residual_b^2 - M7_residual_b^2)/(M1_residual_b^2)

#Beta and SE for other variables not captured by the loop function
#Individual mean
M1_study_period.pid_b <- attributes(VarCorr(M1)$"study_period:pid")$stddev
M1_study_period.pid_se <- M1_se[1]

M2_study_period.pid_b <- attributes(VarCorr(M2)$"study_period.pid.1")$stddev
M2_study_period.pid_se <- M2_se[2]

M3_study_period.pid_b <- attributes(VarCorr(M3)$"study_period.pid.2")$stddev
M3_study_period.pid_se <- M3_se[3]

#Year
M2_study_period.pid.cyear_b <- attributes(VarCorr(M2)$"study_period.pid")$stddev
M2_study_period.pid.cyear_se <- M2_se[1]

M3_study_period.pid.cyear_b <- attributes(VarCorr(M3)$"study_period.pid.1")$stddev
M3_study_period.pid.cyear_se <- M2_se[2]

#Year^2 adjusted
M3_study_period.pid.cyear_2_b <- attributes(VarCorr(M3)$"study_period.pid")$stddev
M3_study_period.pid.cyear_2_se <- M3_se[1]

# TABLES ----

Tables <- stargazer(M1, M2, M3,
                    notes.align = "l",
                    column.labels = c("Average", "Year-adjusted", "Year$^2$-adjusted"),
                    notes = "Standard errors in parenthesis",
                    notes.append = FALSE,
                    column.sep.width = "0pt",
                    header = FALSE,
                    float = FALSE,
                    align = TRUE, digits = 3, digits.extra = 0,
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    model.numbers = TRUE,
                    single.row = TRUE,
                    star.cutoffs = NA,
                    keep.stat = "n",
                    covariate.labels = "\\hspace{10mm}Grand mean"
)

Tables <- as.data.frame(Tables)
Tables$Tables <- as.character(Tables$Tables)
Tables

#Categories
residualeffect <- "\\emph{Residual} & & & \\\\"
randomeffect <- "\\emph{Random effect} & & & \\\\"
fixedeffect <- "\\emph{Fixed effect} & & & \\\\"
newline <- "\\\\"
hline <- "\\hline"

#Compile the rows
individual_obs <- paste0("\\hspace{10mm}Individual observation & ",
                         sprintf("%.3f", round(M1_residual_b, 3)), "$ $(", sprintf("%.3f", round(M1_residual_se, 3)), ") ", "&", 
                         sprintf("%.3f", round(M2_residual_b, 3)), "$ $(", sprintf("%.3f", round(M2_residual_se, 3)), ") ", "&", 
                         sprintf("%.3f", round(M3_residual_b, 3)), "$ $(", sprintf("%.3f", round(M3_residual_se, 3)), ") ",
                         "\\\\")
individual_obs

study_period <- paste0("\\hspace{10mm}Study period mean & ",
                       sprintf("%.3f", round(M1_study_period_b, 3)), "$ $(", sprintf("%.3f", round(M1_study_period_se, 3)), ") ", "&", 
                       sprintf("%.3f", round(M2_study_period_b, 3)), "$ $(", sprintf("%.3f", round(M2_study_period_se, 3)), ") ", "&", 
                       sprintf("%.3f", round(M3_study_period_b, 3)), "$ $(", sprintf("%.3f", round(M3_study_period_se, 3)), ") ",
                       "\\\\")

study_period.pid <- paste0("\\hspace{10mm}Individual mean & ", 
                           sprintf("%.3f", round(M1_study_period.pid_b, 3)), "$ $(", sprintf("%.3f", round(M1_study_period.pid_se, 3)), ") ", "&", 
                           sprintf("%.3f", round(M2_study_period.pid_b, 3)), "$ $(", sprintf("%.3f", round(M2_study_period.pid_se, 3)), ") ", "&", 
                           sprintf("%.3f", round(M3_study_period.pid_b, 3)), "$ $(", sprintf("%.3f", round(M3_study_period.pid_se, 3)), ") ",
                           "\\\\")

study_period.pid.cyear <- paste0("\\hspace{10mm}In rate of change (year) & &", 
                                 sprintf("%.3f", round(M2_study_period.pid.cyear_b, 3)), "$ $(", sprintf("%.3f", round(M2_study_period.pid.cyear_se, 3)), ") ", "&", 
                                 sprintf("%.3f", round(M3_study_period.pid.cyear_b, 3)), "$ $(", sprintf("%.3f", round(M3_study_period.pid.cyear_se, 3)), ") ", 
                                 "\\\\")

study_period.pid.cyear_2 <- paste0("\\hspace{10mm}In rate of change (year$^2$) & & &", 
                                   sprintf("%.3f", round(M3_study_period.pid.cyear_2_b, 3)), "$ $(", sprintf("%.3f", round(M3_study_period.pid.cyear_2_se, 3)), ") ", 
                                   "\\\\")

rsquared <- paste0("$ R^2_{\\epsilon}$ & &", 
                   sprintf("%.3f", round(r2_year, 3)), " & ", sprintf("%.3f", round(r2_year_2, 3)), "", 
                   "\\\\")

rsquared_dn <- paste0("$ R^2_{\\epsilon} (\\Delta \\hat{y}_{0pi} < 0)$ & &",
                      sprintf("%.3f", round(r2_year_dn, 3)), " & ", sprintf("%.3f", round(r2_year_2_dn, 3)), "",
                      "\\\\")

rsquared_up <- paste0("$ R^2_{\\epsilon} (\\Delta \\hat{y}_{0pi} > 0)$ & &",
                      sprintf("%.3f", round(r2_year_up, 3)), " & ", sprintf("%.3f", round(r2_year_2_up, 3)), "",
                      "\\\\")

# Find where you want to put in the user specific rows.  In our case, this is right after the last fixed effect.
r <- 8
Tables <- insertrow(Tables, fixedeffect, r)
Tables <- insertrow(Tables, newline, r+2)
Tables <- insertrow(Tables, randomeffect, r+3)
Tables <- insertrow(Tables, study_period, r+4)
Tables <- insertrow(Tables, study_period.pid, r+5)
Tables <- insertrow(Tables, study_period.pid.cyear, r+6)
Tables <- insertrow(Tables, study_period.pid.cyear_2, r+7)

Tables <- insertrow(Tables, newline, r+8)
Tables <- insertrow(Tables, residualeffect, r+9)
Tables <- insertrow(Tables, individual_obs, r+10)
Tables <- insertrow(Tables, rsquared, r+13)
# Tables <- insertrow(Tables, rsquared_dn, r+14)
# Tables <- insertrow(Tables, rsquared_up, r+15)

write.table(Tables,file=paste0(tables,"mixed_ind_src.tex"),
            row.names= FALSE, 
            na="", 
            quote = FALSE, 
            col.names = FALSE)


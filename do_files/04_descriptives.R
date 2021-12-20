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

# NO SCIENTIFIC NOTATION
options(scipen=999)

# MANUAL FUNCTION
insertrow <- function(existingDF, newrow, r) {
        existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
        existingDF[r,] <- newrow
        existingDF
}

#########################################
# LOAD DATA
#########################################

# Note: stargazer v. 5.1 does not play nicely with dplyr's tbl_df class. As a temporary work-around I pipe the dataset to data.frame.
x <- readRDS(file = paste0(data_files,"psid_clean_sample.rds"))
x <- data.frame(select(x, pid, year, income_beg, income_beg_unadj, std, volatility, volatility_2, slope_2, age))

#########################################
# CLEAN DATA
#########################################

x <- mutate(x, up = ifelse(slope_2 > 0, yes = slope_2, no = NA))
x <- mutate(x, dn = ifelse(slope_2 < 0, yes = slope_2, no = NA))
x <- mutate(x, income_beg_unadj = exp(income_beg_unadj))

x <- arrange(x, pid, year)

# Average number of study windows
x <- group_by(x, pid) %>% mutate(count = n()) %>% ungroup()
windows <- round(mean(x$count), 2)

table(x$count)
table(x$year)

# Unique observations
x <- group_by(x, pid) %>% mutate(unique = ifelse(row_number()==1, yes = 1, no = 0)) %>% ungroup()
unique <- format(sum(x$unique), big.mark = ",")

# Total observations
obs <- format(nrow(x), big.mark = ",")

#########################################
# TABLES
#########################################
y <- data.frame(select(x, income_beg_unadj, income_beg, std, volatility, volatility_2, slope_2, up, dn, age))

# VARIABLE LABLES

inc_u <- paste0("\\multicolumn{6}{l}{\\emph{Income characteristics}} \\\\ 
                \\hspace{5mm} $^{1}$Income at start (Unadj.)")
inc_a <- paste0("\\hspace{5mm} $^{2}$Income at start (Adj.)")

std <- paste0("& & & & & \\\\ 
              \\multicolumn{6}{l}{\\emph{Volatility characteristics}} \\\\ 
              \\hspace{5mm} SD")
volatility <- paste0("\\hspace{5mm} Year-trend")
volatility_2 <- paste0("\\hspace{5mm} Year$^2$ trend")

slope_2 <- paste0("& & & & & \\\\ 
                  \\multicolumn{6}{l}{\\emph{Mobility characteristics$^{3}$}} \\\\ 
                  \\hspace{5mm}100 x change in LN income ($\\Delta \\hat{y}_{0pi}$)")

up <- paste0("\\hspace{5mm} $\\Delta \\hat{y}_{0pi} > 0$")
dn <- paste0("\\hspace{5mm} $\\Delta \\hat{y}_{0pi} < 0$")

age <- paste0("& & & & & \\\\ 
              \\hspace{2mm} Age at start")

t <- stargazer(y, 
               out = paste0(tables,"descriptives.tex"), 
               column.sep.width = "0pt",
               notes = c("$^1$ The average of an individual's real income in the first two periods in a study period.",
                         "$^2$ Income is defined as the residual of log income after taking out year fixed effects in a given study period.",
                         "$^3$ Where $\\Delta \\hat{y}_{pi} = \\hat{y}_{pi,t=N} - \\hat{y}_{pi,t=1}$ if $\\hat{y}_{pit} = \\beta_{0i} + \\beta_{1i} T + \\beta_{2i} T^2$"
               ),
               header = FALSE,
               float = FALSE,
               align = TRUE, digits = 3, digits.extra = 0,
               model.numbers = TRUE,
               column.labels = c("Mean", "SD", "P25", "P50", "P75"),
               summary.stat = c("mean", "sd", "p25", "median", "P75"),
               dep.var.labels.include = FALSE,
               dep.var.caption = "",
               covariate.labels = c(inc_u,
                                    inc_a,
                                    std,
                                    volatility,
                                    volatility_2,
                                    slope_2,
                                    up,
                                    dn,
                                    age
               ),
               star.cutoffs = NA,
               single.row = TRUE
)

Tables <- as.data.frame(t)
Tables$t <- as.character(Tables$t)

hline <- "\\hline"
totalobs <- paste0("\\hspace{3mm}Total N & \\multicolumn{1}{r}{", obs,"} & & & & \\\\")
windowsobs <- paste0("\\hspace{3mm}Avg. number of study period & \\multicolumn{1}{r}{", windows,"} & & & & \\\\")
uniqueobs <- paste0("\\hspace{3mm}Unique N & \\multicolumn{1}{r}{", unique,"} & & & & \\\\")

# Find where you want to put in the user specific rows.  In our case, this is right after the last fixed effect.
r <- 23

Tables <- insertrow(Tables, totalobs, r)
Tables <- insertrow(Tables, windowsobs, r+1)
Tables <- insertrow(Tables, uniqueobs, r+2)
Tables <- insertrow(Tables, hline, r+3)


write.table(Tables,file=paste0(tables,"descriptives_ind_src.tex"),
            row.names= FALSE, 
            na="", 
            quote = FALSE, 
            col.names = FALSE)


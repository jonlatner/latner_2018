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
graphs = "graphs/"

# PACKAGES
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages("ggplot2")
# install.packages("plm")
# install.packages("beepr")
# install.packages("reshape2")
# install.packages("robumeta") # group.center

# LIBRARY
library(tidyverse)
library(stargazer)
library(plm)
library(beepr)
library(reshape2)
library(robumeta) # group.center

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

x <- readRDS(file = paste0(data_files,"psid_clean_sample.rds"))
x <- select(x, pid, year, std, volatility, volatility_2, slope_2, income_beg, age) # control variables

# y <- readRDS("/Users/jonathanlatner/Google Drive/volatility/data_files/volatility_data_src_ind.rds")
# y <- select(y, pid, year, std, volatility, volatility_2, slope_2, income_beg, age) # control variables
# y <- mutate(y, std_y = std) %>%
#         select(pid, year, std_y)

# x <- readRDS("/Users/jonathanlatner/Google Drive/volatility/data_files/volatility_data_src_ind.rds")
# x <- select(x, pid, year, std, volatility, volatility_2, slope_2, income_beg, age) # control variables

# x <- merge(x, y)

#########################################
# CLEAN DATA
#########################################

x <- mutate(x, 
            up = ifelse(slope_2 > 0, yes = slope_2, no = 0),
            dn = ifelse(slope_2 < 0, yes = -1*slope_2, no = 0))

x <- arrange(x, pid, year)

#########################################
# FIXED EFFECTS REGRESSION
#########################################

# VOLATILITY FROM AVERAGE
avg_fe_base <- plm(formula = std ~ income_beg + age + factor(year), data = x, index = c("pid", "year"), model = "within")
avg_fe_mob <- plm(formula = std ~ dn + up + income_beg + factor(year), data = x, index = c("pid", "year"), model = "within")
avg_fe <- plm(formula = std ~ dn + up + income_beg + age + factor(year), data = x, index = c("pid", "year"), model = "within")

# VOLATILITY FROM TREND LINE
lin_fe_base <- plm(formula = volatility ~ income_beg + age + factor(year), data = x, index = c("pid", "year"), model = "within")
lin_fe_mob <- plm(formula = volatility ~ dn + up + income_beg + factor(year), data = x, index = c("pid", "year"), model = "within")
lin_fe <- plm(formula = volatility ~ dn + up + income_beg + age + factor(year), data = x, index = c("pid", "year"), model = "within")

# VOLATILITY FROM TREND LINE^2
clin_fe_base <- plm(formula = volatility_2 ~ income_beg + age + factor(year), data = x, index = c("pid", "year"), model = "within")
clin_fe_mob <- plm(formula = volatility_2 ~ dn + up + income_beg + factor(year), data = x, index = c("pid", "year"), model = "within")
clin_fe <- plm(formula = volatility_2 ~ dn + up + income_beg + age + factor(year), data = x, index = c("pid", "year"), model = "within")

# stargazer(avg_fe_base, avg_fe_mob, avg_fe, lin_fe_base, lin_fe_mob, lin_fe, clin_fe_base, clin_fe_mob, clin_fe,            keep = c("dn", "up", "income_beg", "age"),
#           type = "text")

# stargazer(avg_fe, lin_fe, clin_fe,            
#           keep = c("dn", "up", "income_beg", "age"),
#           type = "text")

# stargazer(avg_fe_base, avg_fe_mob, avg_fe, lin_fe_base, lin_fe_mob, lin_fe, clin_fe_base, clin_fe_mob, clin_fe,            keep = c("dn", "up", "income_beg", "age"),
#           single.row = FALSE,
#           out = paste0(tables,"test.tex"), 
#           omit.table.layout = "n",  #drop notes
#           column.sep.width = "0pt",
#           header = FALSE,
#           float = FALSE,
#           align = TRUE, digits = 3, digits.extra = 0,
#           model.numbers = TRUE,
#           keep.stat = c("n", "rsq"),
#           dep.var.labels.include = FALSE,
#           dep.var.caption = "")

#########################################
# MARGINAL EFFECTS - BY MOBILITY
#########################################

me_up <- expand.grid(direction = c(seq(0, 100, 10)))
me_dn <- expand.grid(direction = c(seq(0, 100, 10)))

# get the coefficient for each variable
# average
vars = c("dn", "up", "income_beg", "age")
for (v in vars) {
        me_up[[v]] <- coef(avg_fe)[v]
        me_up$intercept <- mean(fixef(avg_fe))
        me_dn[[v]] <- coef(avg_fe)[v]
        me_dn$intercept <- mean(fixef(avg_fe))
        me_up$mean_inc <- mean(x$income_beg)
        me_dn$mean_inc <- mean(x$income_beg)
        me_up$mean_age <- mean(x$age)
        me_dn$mean_age <- mean(x$age)
}        

me_up <- mutate(me_up, std = direction*up + intercept)
me_dn <- mutate(me_dn, std = direction*dn + intercept)

# linear
for (v in vars) {
        me_up[[v]] <- coef(lin_fe)[v]
        me_up$intercept <- mean(fixef(lin_fe))
        me_dn[[v]] <- coef(lin_fe)[v]
        me_dn$intercept <- mean(fixef(lin_fe))
}        
me_up <- mutate(me_up, lin = direction*up + intercept)
me_dn <- mutate(me_dn, lin = direction*dn + intercept)

# curvilinear
for (v in vars) {
        me_up[[v]] <- coef(clin_fe)[v]
        me_up$intercept <- mean(fixef(clin_fe))
        me_dn[[v]] <- coef(clin_fe)[v]
        me_dn$intercept <- mean(fixef(clin_fe))
}        

me_up <- mutate(me_up, clin = direction*up + intercept) %>% select(direction, std, lin, clin)
me_dn <- mutate(me_dn, clin = direction*dn + intercept) %>% select(direction, std, lin, clin)
me_dn$direction <- me_dn$direction*-1

me <- rbind(me_up, me_dn)
me <- arrange(me, direction)
me <- unique(me)
rm(me_up, me_dn)

# RESHAPE
level <- melt(me, id.vars = "direction")

# GRAPH MARGINAL EFFECTS
ggplot(data = level, aes(x = direction, y = value, linetype = variable)) +
        geom_line(size = 2) +
        scale_linetype_manual(values = c("solid",
                                         "dotted",
                                         "dashed"),
                              labels = c("SD(Average)",
                                         "SD(Year trend)",
                                         expression(SD(Year^2~trend)))) +
        ylab("Predicted volatility") +
        xlab("Income mobility") +
        # xlab(expression(paste("Pr(Income change from ",year^2, " adjusted trend line)"))) + 
        scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0, 60)) +
        scale_x_continuous(breaks = seq(-100, 100, by = 20)) +
        guides(col = guide_legend(nrow = 2)) +
        theme_bw() +
        theme(legend.key.width = unit(1,"cm"),
              panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size = .5),
              axis.line.x = element_line(color = "black", size = .5),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text.align = 0,
              legend.key = element_blank()
        )

ggsave(filename = paste0(graphs,"margins_ind_src_apc_w_o_cube.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

#########################################
# TABLES
#########################################


# VARIABLE LABLES
dn <- paste0("\\hspace{2mm}Downward mobility ($|\\Delta \\hat{y}_{0pi} < 0|$)")

up <- paste0("\\multicolumn{10}{l}{\\phantom{empty}} \\\\ 
             \\hspace{2mm}Upward mobility ($\\Delta \\hat{y}_{0pi} > 0$)")

inc <- paste0("\\multicolumn{10}{l}{\\phantom{empty}} \\\\ 
              \\hspace{2mm}Income at start")

age <- paste0("\\multicolumn{10}{l}{\\phantom{empty}} \\\\ 
              \\hspace{2mm}Age")

# stargazer(avg_fe_base, avg_fe_mob, avg_fe, lin_fe_base, lin_fe_mob, lin_fe, clin_fe_base, clin_fe_mob, clin_fe,            keep = c("dn", "up", "income_beg", "age"),

t <- stargazer(avg_fe_base, avg_fe_mob, avg_fe, lin_fe_base, lin_fe_mob, lin_fe, clin_fe_base, clin_fe_mob, clin_fe,
               out = paste0(tables,"regression_src_v3.tex"), 
               keep = c("dn", "up", "inc", "age"),
               omit.table.layout = "n",  #drop notes
               column.sep.width = "0pt",
               header = FALSE,
               float = FALSE,
               align = TRUE, digits = 3, digits.extra = 0,
               model.numbers = FALSE,
               keep.stat = c("n", "rsq"),
               dep.var.labels.include = FALSE,
               dep.var.caption = "",
               covariate.labels = c(
                       dn,
                       up,
                       inc,
                       age),
               star.cutoffs = NA,
               single.row = FALSE
)

Tables <- as.data.frame(t)
Tables$t <- as.character(Tables$t)

header <- "\\\\ [-1.8ex] & \\multicolumn{3}{l}{(1) Average} & \\multicolumn{3}{l}{(2) Year-adjusted} & \\multicolumn{3}{l}{(3) Year$^2$-adjusted} \\\\ \\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule{8-10}"
numbers <- "\\\\ [-1.8ex] 
& \\multicolumn{1}{c}{(A)} & \\multicolumn{1}{c}{(B)} & \\multicolumn{1}{c}{(C)} 
& \\multicolumn{1}{c}{(A)} & \\multicolumn{1}{c}{(B)} & \\multicolumn{1}{c}{(C)} 
& \\multicolumn{1}{c}{(A)} & \\multicolumn{1}{c}{(B)} & \\multicolumn{1}{c}{(C)} 
\\\\ \\hline \\\\[-1.8ex]"
notes <- "\\emph{Notes} & \\multicolumn{9}{l}{Standard errors in parenthesis} \\\\
& \\multicolumn{9}{l}{Year fixed effects for each 11-year study period (1970 - 1980, 1971 - 1981, \\dots, 2003 - 2013) not shown.} \\\\
& \\multicolumn{9}{l}{$\\Delta \\hat{y}_{0pi} = \\hat{y}_{pi,t=N} - \\hat{y}_{pi,t=1}$ if $\\hat{y}_{pit} = \\beta_0 + \\beta_{1} T + \\beta_{2} T^2$ and $p$ is study period, $i$ is individual, and $ t $ is year.}"

# Find where you want to put in the user specific rows.  In our case, this is right after the last fixed effect.
r <- 5
Tables <- insertrow(Tables, header, r)
Tables <- insertrow(Tables, numbers, r+1)
Tables <- insertrow(Tables, notes, r+22)

write.table(Tables,file=paste0(tables,"fe_regression_src.tex"),
            row.names= FALSE,
            na="",
            quote = FALSE,
            col.names = FALSE)

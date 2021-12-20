# TOP COMMANDS ----
rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/GitHub/latner_2018/")

data = "data_files/"
tables = "tables/"
graphs = "graphs/"

# PACKAGES
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("broom")

# LIBRARY
library(dplyr)
library(ggplot2)
library(reshape2)
library(broom)

# NO SCIENTIFIC NOTATION
options(scipen=999)

# LOAD DATA ----

for(i in 1:4) {
        id <- c(i)
        year <- c(seq(1, 10, 1))
        income <- c(100)
        x <- cbind(id, year, income)
        assign(paste0("x_", i), x)
}
x <- data.frame(rbind(x_1, x_2, x_3, x_4))
rm(list=ls(pattern = "x_"))

# CLEAN DATA - EXAMPLE ----

#EXAMPLE 1 - FLAT
#EXAMPLE 2 - MEAN-REVERTING

x[which(x$year %in% seq(2, 8, 2) & x$id == 2),]$income <- 125
x[which(x$year %in% seq(3, 9, 2) & x$id == 2),]$income <- 75

#EXAMPLE 3 - LINEAR

x[which(x$year == 1 & x$id == 3),]$income <- 50

for(i in seq(2, 10, 1)) {
        x <- mutate(x, income = ifelse(id == 3 & year == i, lag(income, 1) + 11.1111, income))
}
x$income <- ceiling(x$income)

#EXAMPLE 4 - ASYMETRIC
        
x[which(x$year == 1 & x$id == 4),]$income <- 54
for(i in seq(2, 10, 1)) {
        x <- mutate(x, income = ifelse(id == 4 & year == i, lag(income, 1) + 10, income))
}

for(i in seq(2, 10, 2)) {
        x <- mutate(x, income = ifelse(id == 4 & year == i, income + 10, income))
}

for(i in seq(3, 9, 2)) {
        x <- mutate(x, income = ifelse(id == 4 & year == i, income - 10, income))
}

# EXAMPLE 4 - REAL
# PSID: pid == 1221003 & (year >= 1986 & year <= 1995)

income <- c(54, 63, 117, 120, 119, 118, 142, 146, 143, 123)
id = 5
psid_x <- data.frame(income)
psid_x$id = 5
psid_x <- psid_x %>%
        mutate(year = row_number())

# APPEND DATA
x <- rbind(x, psid_x)
x <- mutate(x, ln_income = log(income))
rm(psid_x)

#VOLATILITY ----

#YEAR-ADJUSTED TREND LINE
regression <- group_by(x, id) %>%
        do(augment(lm(income ~ year, data = .), data = .))
x$yhat <- round(as.numeric(regression$.fitted), 0)

#SMALL ISSUE WITH THE FIRST OBSERVATION IN EXAMPLE 3 DUE TO ROUNDING
x <- mutate(x, yhat = ifelse(id == 3 & year == 1, yes = 50, no = yhat))

#YEAR^2-ADJUSTED TREND LINE
regression <- group_by(x, id) %>%
        do(augment(lm(income ~ poly(year, degree = 2, raw = TRUE), data=.), data = .))
x$yhat_2 <- round(as.numeric(regression$.fitted), 0)
rm(regression)

#MEAN
x <- group_by(x, id) %>%
        mutate(mline = mean(income)) %>%
        ungroup()

#VOLATILITY
x <- group_by(x, id) %>%
        mutate(std = sd(income), volatility = sd(income - yhat), volatility_2 = sd(income - yhat_2)) %>%
        ungroup()

#MOBILITY ----

#YEAR-ADJUSTED TREND LINE
x <- group_by(x, id) %>%
        mutate(slope = last(yhat)/first(yhat)*100) %>%
        ungroup()

#YEAR^2-ADJUSTED TREND LINE
x <- group_by(x, id) %>%
        mutate(slope_2 = last(yhat_2)/first(yhat_2)*100) %>%
        ungroup()

#RAW INCOME DIFFERENCE
x <- group_by(x, id) %>%
        mutate(income_diff = last(income)/first(income)*100) %>%
        ungroup()

#ROUND
vars <- c("std", "volatility", "volatility_2", "slope", "slope_2", "income_diff")
for(i in vars){
        x[[i]] <- round(x[[i]], 0)
}

#GRAPH - PAPER ----

#EXAMPLE 1 - FLAT
std <- mean(subset(x, id == 1)$std)
diff_unadj <- mean(subset(x, id == 1)$income_diff)

lb0 <- paste("Volatility~(upsilon[i])~'\u003A'")
lb1 <- paste("(1)~SD~from~mean ==",std)

ub0 <- paste("Mobility~(Delta~y[i])~'\u003A'")
ub1 <- paste("(1)~Delta~y[i]~from~raw~income ==",diff_unadj,"~'%'")

ggplot(data = subset(x, id == 1), aes(year)) +
        geom_point(aes(y = income, colour = "income"), size = 4, shape = 21, fill = "white") +
        geom_line(aes(y = income), size = 1) + 
        geom_line(aes(y = mline, colour = "mline"), size = 1) +
        ylab("Individual (i) income ($1,000)") +
        xlab("Time (t)") + 
        annotate("text", x = 1, y = 170, label = lb0, size = 4, hjust = 0, parse = TRUE) + 
        annotate("text", x = 1, y = 160, label = lb1, size = 4, hjust = 0, parse = TRUE) + 
#        annotate("text", x = 7, y = 90, label = ub0, size = 4, hjust = 0, parse = TRUE) + 
#        annotate("text", x = 7, y = 80, label = ub1, size = 4, hjust = 0, parse = TRUE) + 
        scale_y_continuous(breaks = seq(50, 175, by = 25), limits = c(50, 175)) +
        scale_x_continuous(breaks = seq(1, 10, by = 1)) +
        scale_color_manual(values = c("income"="black", "mline"="gray50"), labels = c("Income", "Average")) + 
        guides(colour = guide_legend(override.aes = list(linetype = c(1, 1), shape = c(21, NA)), nrow = 1)) +
        theme_grey() +
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                legend.key.width = unit(1,"cm"),
                axis.line.y = element_line(color="black", size = .5),
                axis.line.x = element_line(color = "black", size = .5),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text.align = 0,
                legend.key = element_rect(fill = "transparent")
        )

ggsave(filename = paste0(graphs,"example_1.png"), plot = last_plot(), height = 2.75, width = 3.5, units = "in", scale = 1.5)

#EXAMPLE 2 - MEAN-REVERTING
std <- mean(subset(x, id == 2)$std)
lin <- mean(subset(x, id == 2)$volatility)
clin <- mean(subset(x, id == 2)$volatility_2)

diff_unadj <- mean(subset(x, id == 2)$income_diff)
diff_slope <- mean(subset(x, id == 2)$slope)
diff_slope_2 <- mean(subset(x, id == 2)$slope_2)

lb0 <- paste("Volatility~(upsilon[i])~'\u003A'")
lb1 <- paste("(1)~SD~from~mean ==",std)
lb2 <- paste("(2)~SD~from~year~trend~line ==", lin)
lb3 <- paste("(3)~SD~from~year^2~trend~line ==", clin)

ub0 <- paste("Mobility~(Delta~y[i])~'\u003A'")
ub1 <- paste("(1)~Delta~y[i]~from~raw~income ==",diff_unadj,"~'%'")
ub2 <- paste("(2)~Delta~y[i]~from~trend~line ==",diff_slope,"~'%'")
ub3 <- paste("(3)~Delta~y[i]~from~year^2~trend~line ==", diff_slope_2,"~'%'")

ggplot(data = subset(x, id == 2), aes(year)) +
        geom_point(aes(y = income, colour = "income"), size = 4, shape = 21, fill = "white") +
        geom_line(aes(y = income), size = 1) + 
        geom_line(aes(y = mline, colour = "mline"), size = 1) +
        ylab("Individual (i) income ($1,000)") +
        xlab("Time (t)") + 
        scale_y_continuous(breaks = seq(50, 175, by = 25), limits = c(50, 175)) +
        scale_x_continuous(breaks = seq(1, 10, by = 1)) +
        scale_color_manual(values = c("income"="black", "mline"="gray50"), labels = c("Income", "Average")) + 
        annotate("text", x = 1, y = 170, label = lb0, size = 4, hjust = 0, parse = TRUE) + 
        annotate("text", x = 1, y = 160, label = lb1, size = 4, hjust = 0, parse = TRUE) + 
#        annotate("text", x = 5.5, y = 65, label = ub0, size = 4, hjust = 0, parse = TRUE) + 
#        annotate("text", x = 5.5, y = 55, label = ub1, size = 4, hjust = 0, parse = TRUE) + 
        guides(colour = guide_legend(override.aes = list(linetype = c(1, 1), shape = c(21, NA)), nrow = 1)) +
        theme_grey() +
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                legend.key.width = unit(1,"cm"),
                axis.line.y = element_line(color="black", size = .5),
                axis.line.x = element_line(color = "black", size = .5),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text.align = 0,
                legend.key = element_rect(fill = "transparent")
        )

ggsave(filename = paste0(graphs,"example_2.png"), plot = last_plot(), height = 2.75, width = 3.5, units = "in", scale = 1.5)

#EXAMPLE 3 - LINEAR
std <- mean(subset(x, id == 3)$std)
lin <- mean(subset(x, id == 3)$volatility)
clin <- mean(subset(x, id == 3)$volatility_2)

diff_unadj <- mean(subset(x, id == 3)$income_diff)
diff_slope <- mean(subset(x, id == 3)$slope)
diff_slope_2 <- mean(subset(x, id == 3)$slope_2)

lb0 <- paste("Volatility~(upsilon[i])~'\u003A'")
lb1 <- paste("(1)~SD~from~mean ==",std)
lb2 <- paste("(2)~SD~from~year~trend~line ==", lin)
lb3 <- paste("(3)~SD~from~year^2~trend~line ==", clin)

ub0 <- paste("Mobility~(Delta~y[i])~'\u003A'")
ub1 <- paste("(1)~Delta~y[i]~from~raw~income ==",diff_unadj,"~'%'")
ub2 <- paste("(2)~Delta~y[i]~from~trend~line ==",diff_slope,"~'%'")
ub3 <- paste("(3)~Delta~y[i]~from~year^2~trend~line ==", diff_slope_2,"~'%'")

ggplot(data = subset(x, id == 3), aes(year)) +
        geom_point(aes(y = income, colour = "income"), size = 4, shape = 21, fill = "white") +
        geom_line(aes(y = income), size = 1) + 
        geom_line(aes(y = mline, colour = "mline"), size = 1) +
        geom_line(aes(y = yhat, colour = "yhat"), size = 1) +
        ylab("Individual (i) income ($1,000)") +
        xlab("Time (t)") + 
        scale_y_continuous(breaks = seq(50, 175, by = 25), limits = c(50, 175)) +
        scale_x_continuous(breaks = seq(1, 10, by = 1)) +
        scale_color_manual(values = c("income"="black", "mline"="gray50", "yhat"="black"), labels = c("Income", "Average", "SD(Year trend)")) + 
        annotate("text", x = 1, y = 170, label = lb0, size = 4, hjust = 0, parse = TRUE) + 
        annotate("text", x = 1, y = 160, label = lb1, size = 4, hjust = 0, parse = TRUE) + 
        annotate("text", x = 1, y = 150, label = lb2, size = 4, hjust = 0, parse = TRUE) + 
       annotate("text", x = 5.5, y = 90, label = ub0, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5.5, y = 80, label = ub1, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5.5, y = 70, label = ub2, size = 4, hjust = 0, parse = TRUE) +
        guides(colour = guide_legend(override.aes = list(linetype = c(1, 1, 1), shape = c(21, NA, NA)), nrow = 1)) +
        theme_grey() +
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                legend.key.width = unit(1,"cm"),
                legend.position = "bottom",
                axis.line.y = element_line(color="black", size = .5),
                axis.line.x = element_line(color = "black", size = .5),
                legend.title = element_blank(),
                legend.text.align = 0,
                legend.key = element_rect(fill = "transparent")
        )

ggsave(filename = paste0(graphs,"example_3.png"), plot = last_plot(), height = 2.75, width = 3.5, units = "in", scale = 1.5)

#EXAMPLE 4 - ASYMETRIC
std <- mean(subset(x, id == 4)$std)
lin <- mean(subset(x, id == 4)$volatility)
clin <- mean(subset(x, id == 4)$volatility_2)

diff_unadj <- mean(subset(x, id == 4)$income_diff)
diff_slope <- mean(subset(x, id == 4)$slope)
diff_slope_2 <- mean(subset(x, id == 4)$slope_2)

lb0 <- paste("Volatility~(upsilon[i])~'\u003A'")
lb1 <- paste("(1)~SD~from~mean ==",std)
lb2 <- paste("(2)~SD~from~year~trend~line ==", lin)
lb3 <- paste("(3)~SD~from~year^2~trend~line ==", clin)

ub0 <- paste("Mobility~(Delta~y[i])~'\u003A'")
ub1 <- paste("(1)~Delta~y[i]~from~raw~income ==",diff_unadj,"~'%'")
ub2 <- paste("(2)~Delta~y[i]~from~trend~line ==",diff_slope,"~'%'")
ub3 <- paste("(3)~Delta~y[i]~from~year^2~trend~line ==", diff_slope_2,"~'%'")

ggplot(data = subset(x, id == 4), aes(year)) +
        geom_point(aes(y = income, colour = "income"), size = 4, shape = 21, fill = "white") +
        geom_line(aes(y = income), size = 1) + 
        geom_line(aes(y = mline, colour = "mline"), size = 1) +
        geom_line(aes(y = yhat, colour = "yhat"), size = 1) +
        ylab("Individual (i) income ($1,000)") +
        xlab("Time (t)") + 
        scale_y_continuous(breaks = seq(50, 175, by = 25), limits = c(50, 175)) +
        scale_x_continuous(breaks = seq(1, 10, by = 1)) +
        scale_color_manual(values = c("income"="black", "mline"="gray50", "yhat"="black"), labels = c("Income", "Average", "SD(Year trend)")) + 
        annotate("text", x = 1, y = 170, label = lb0, size = 4, hjust = 0, parse = TRUE) + 
        annotate("text", x = 1, y = 160, label = lb1, size = 4, hjust = 0, parse = TRUE) + 
        annotate("text", x = 1, y = 150, label = lb2, size = 4, hjust = 0, parse = TRUE) + 
       annotate("text", x = 5.5, y = 90, label = ub0, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5.5, y = 80, label = ub1, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5.5, y = 70, label = ub2, size = 4, hjust = 0, parse = TRUE) +
        guides(colour = guide_legend(override.aes = list(linetype = c(1, 1, 1), shape = c(21, NA, NA)), nrow = 1)) +
        theme_grey() +
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                legend.position = "bottom",
                legend.key.width = unit(1,"cm"),
                axis.line.y = element_line(color="black", size = .5),
                axis.line.x = element_line(color = "black", size = .5),
                legend.title = element_blank(),
                legend.text.align = 0,
                legend.key = element_rect(fill = "transparent")
        )

ggsave(filename = paste0(graphs,"example_4.png"), plot = last_plot(), height = 2.75, width = 3.5, units = "in", scale = 1.5)

#EXAMPLE 5 - REAL
std <- mean(subset(x, id == 5)$std)
lin <- mean(subset(x, id == 5)$volatility)
clin <- mean(subset(x, id == 5)$volatility_2)

diff_unadj <- mean(subset(x, id == 5)$income_diff)
diff_slope <- mean(subset(x, id == 5)$slope)
diff_slope_2 <- mean(subset(x, id == 5)$slope_2)

lb0 <- paste("Volatility~(upsilon[i])~'\u003A'")
lb1 <- paste("(1)~SD~from~mean ==",std)
lb2 <- paste("(2)~SD~from~year~trend~line ==", lin)
lb3 <- paste("(3)~SD~from~year^2~trend~line ==", clin)

ub0 <- paste("Mobility~(Delta~y[i])~'\u003A'")
ub1 <- paste("(1)~Delta~y[i]~from~raw~income ==",diff_unadj,"~'%'")
ub2 <- paste("(2)~Delta~y[i]~from~trend~line ==",diff_slope,"~'%'")
ub3 <- paste("(3)~Delta~y[i]~from~year^2~trend~line ==", diff_slope_2,"~'%'")

ggplot(data = subset(x, id == 5), aes(year)) +
        geom_point(aes(y = income, colour = "income"), size = 4, shape = 21, fill = "white") +
        geom_line(aes(y = income), size = 1) + 
        geom_line(aes(y = mline, colour = "mline"), size = 1) +
        geom_line(aes(y = yhat, colour = "yhat"), size = 1) +
        geom_line(aes(y = yhat_2, colour = "yhat_2"), size = 1, linetype = 2) +
        ylab("Individual (i) income ($1,000)") +
        xlab("Time (t)") + 
        scale_y_continuous(breaks = seq(50, 175, by = 25), limits = c(50, 175)) +
        scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1, 10)) +
        scale_color_manual(values = c("income"="black", "mline"="gray50", "yhat"="black", "yhat_2"="blue"), labels = c("Income", "Average", "SD(Year trend)  ", expression(SD(Year^2~trend)))) + 
        annotate("text", x = 1, y = 170, label = lb0, size = 4, hjust = 0, parse = TRUE) +
        annotate("text", x = 1, y = 160, label = lb1, size = 4, hjust = 0, parse = TRUE) +
        annotate("text", x = 1, y = 150, label = lb2, size = 4, hjust = 0, parse = TRUE) +
        annotate("text", x = 1, y = 140, label = lb3, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5, y = 90, label = ub0, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5, y = 80, label = ub1, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5, y = 70, label = ub2, size = 4, hjust = 0, parse = TRUE) +
       annotate("text", x = 5, y = 60, label = ub3, size = 4, hjust = 0, parse = TRUE) +
        guides(colour = guide_legend(override.aes = list(linetype = c(1, 1, 1, 2), shape = c(21, NA, NA, NA)), nrow = 1), size = 4) +
        theme_grey() +
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                legend.key.width = unit(1,"cm"),
                axis.line.y = element_line(color="black", size = .5),
                axis.line.x = element_line(color = "black", size = .5),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text.align = 0,
                legend.key = element_rect(fill = "transparent")
        )

ggsave(filename = paste0(graphs,"example_psid.png"), plot = last_plot(), height = 2.75, width = 3.5, units = "in", scale = 1.5)


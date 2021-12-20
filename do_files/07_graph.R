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

# LIBRARY
library(tidyverse)

# Load data ----

x <- readRDS(file = paste0(data_files,"psid_clean_sample.rds"))

x <- select(x, study_period, std, volatility, volatility_2)

# Clean data ----

x <- group_by(x, study_period) %>%
        summarise_each(funs(mean)) %>%
        ungroup() %>%
        rename(year = study_period)

#reshape

level <- x %>%
        pivot_longer(cols=!year, names_to = "variable", values_to = "value")

# Graph data ----

ggplot(data = level, aes(x = year, y = value, linetype = variable)) +
        geom_line(size = 2) +
        ylab("Income volatility") +
        xlab("First year of study period") + 
        scale_linetype_manual(values = c("solid",
                                         "dotted",
                                         "dashed"),
                              labels = c("SD(Average)",
                                         "SD(Year trend)",
                                         expression(SD(Year^2~trend)))) +
        scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0,30)) +
        scale_x_continuous(breaks = seq(1970, 2005, by = 5), limits = c(1970, 2005)) + 
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

ggsave(filename = paste0(graphs,"volatility_graph_src.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)


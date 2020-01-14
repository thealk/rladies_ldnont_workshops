################################################### ###
# Helper script for analyzing Starwars data
################################################### ###

# R-Ladies LdnOnt Workshop on Using R Markdown to generate reports and manuscripts
# January 14, 2020
# Thea Knowles
# theaknow@buffalo.edu

# Setup ----

# Run this line if you you've never installed these packages:
#    install.packages(c("ggplot2", "dplyr", "captioner"))
# Note: you can also check if you have them installed, and even install them directly, in your RStudio Packages panel or by clicking Tools >> Install Packages

library(ggplot2)
library(dplyr)


# Load data ----
starwars <- dplyr::starwars

# Note: if you were reading in your own file, you could, for example, do:
#starbucks <- read.csv("my_data.csv")

# View data
# View(starwars)
names(starwars)
starwars %>% select(1:10) %>% summary()
starwars %>% select(1:10) %>% str()

starwars <- starwars %>% 
        mutate(human = ifelse(species=="Human",1,0),
               human = ifelse(is.na(human),0,human),
               human = factor(human))

summary(starwars$human)

# Summarise data ----
sw_summary_species <- starwars %>% group_by(species) %>%
        summarize(height = mean(height, na.rm=TRUE),
                  mass = mean(mass, na.rm=TRUE),
                  n = n()) %>%
        ungroup()

sw_summary_human <- starwars %>% group_by(human) %>%
        summarize(height = mean(height, na.rm=TRUE),
                  mass = mean(mass, na.rm=TRUE),
                  n = n()) %>%
        ungroup()

# Data exploration
starwars %>% ggplot(aes(x=height,y=mass)) + 
        geom_point() +
        facet_wrap(~human)

# Which species is the outlier?
starwars %>% filter(mass>1000) %>% select(name, species) # jabba the hut

# Models ----
# Does height vary as a function of mass?
mod1 <- lm(mass ~ height, data=starwars) # not significant
summary(mod1)

# Does removing Jabba the Hut make a difference?
mod2 <- lm(mass ~ height, data=subset(starwars,mass<500)) 
summary(mod2) # significant with outlier removed

# Does adding human improve model fit?
mod3 <- lm(mass ~ height+human, data=subset(starwars,mass<500)) 
anova(mod2,mod3) # adding human doesn't improve model fit

# IMPORTAT NOTE! This is just an example. We are missing some important steps/best practices in modelling! See Jaky's R-Ladies intro to regression to see how you'd check model assumptions, etc.!


# Plots ----
sw_plot <- starwars %>% 
        filter(mass < 1000) %>%
        ggplot(aes(x=height,y=mass)) + 
        geom_point(aes(color=species)) +
        geom_smooth(method="lm") +
        labs(x = "Height (cm)",
             y = "Mass (kg)")

sw_plot_human <- sw_plot +
        facet_wrap(~human)+
        guides(color=FALSE)
        



### -- Introduction and refreshers for R
### -- https://github.com/aurielfournier/INHS_R_Workshop_5_2020/

#######################################
### -- Necessary packages
#######################################

# A package is like a book full of information that you want R to be able to use
# Before you use a package for the first time, you have to install it. This is like buying the book and having it in your library.
install.packages("dplyr")
# Then ever future time you want to use it, you need to use the library command to grab the package off the shelf. 

library(dplyr)
library(tidyr)
library(ggplot2)


###################
### -- Loading In The Data
####################

# set working directory
## a working director is the folder where R goes to look for files, and puts new files
## Session drop down menu > Set Working Directory > Choose Directory
## Then navigate to the folder you want to work out of

# discuss eBird data

ebird <- read.csv("eBird_workshop.csv")

# to read in excel files, you can use the read_excel() function from the readxl package. 

# Explain how to leave comments 

#########################
### -- Filtering
#########################

ebird %>%
        filter(state=='IL',
               year==2008)

# Explain What Pipes are %>% 

# explain assignment operators
a = 100
a <- 100

# filter is for rows
# select is for columns

## With pipes
ebird %>%
  filter(state=='IL',
         year==2008) %>% 
        select(state, samplesize, presence)

## Without pipes

ebird_filter <- filter(ebird, state=="IL",year==2008)

select(ebird_filter, state, samplesize, presence)


# the "|" means 'or' in R
ebird %>%
      filter(state=="IL"|state=="IA") %>%
      # comments here 
      distinct(state)

# the "&" means "and" in R
ebird %>%
          filter(year>=2014&year<=2018) %>% 
          distinct(year)

#########################
### -- Match %in%   
#########################

i_states <- c("IL","IA","IN","ID")

ebird %>%
          filter(state %in% i_states) %>% 
          distinct(state)


#########################
### -- GROUPING
#########################

ebird %>%
  group_by(state) %>%
  summarize(mean = mean(samplesize),
            median = median(samplesize))


ebird %>% 
          group_by(state, year) %>%
          summarize(mean=mean(presence))

#########################################
### -- CHALLENGE
#########################################

# What is the median samplesize for 
# the four I states after 2014?

new_data <- ebird %>% 
  filter(state %in% i_states,
         year > 2014) %>% 
  group_by(state) %>%
  summarise(medianS = median(samplesize))


#note to self talk about Kiwi vs Us spelling


#########################
## MUTATE
#########################

colors <- c("red","green")

mebird <- ebird %>%  
  mutate(a_state = ifelse(state %in% i_states, 1, 0),
         state_year = paste0(state,"_",year)) 

mebird %>%
  tail()
  
########################
## Separate
########################

mebird %>% 
  separate(state_year, 
           sep="_", 
           into=c("state",
                  "year"),
           remove=FALSE) %>%
  head()

# or

mebird %>% 
  separate(year, sep=c(2), 
           into=c("century","endpart"),
           remove=FALSE) %>%
  head()

########################
## Joins
########################

cool_birds <- c("Sora","Virginia Rail","Yellow Rail")

ebird1 <- ebird %>% 
            filter(state %in% i_states,
                   species %in% cool_birds) %>%
            select(species, state, year, samplesize) %>%
            filter(year >= 2014)

# point out that you can use multiple filter statements if you want, or you can put them all in one statement, same result. 

years_to_keep <- c(2008:2012, 2015)

ebird2 <- ebird %>%
            filter(state %in% i_states,
                  species %in% cool_birds) %>%
            select(species, state, year, presence) %>%
            filter(year %in% years_to_keep)

unique(ebird1$year)
unique(ebird2$year)

# 
full_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

full_join(ebird1, ebird2, by=c("year","species","state")) %>% head()

# 

right_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

right_join(ebird1, ebird2, by=c("year","species","state")) %>% head()

# 

left_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

left_join(ebird1, ebird2, by=c("year","species","state")) %>% head()

# 

inner_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

inner_join(ebird1, ebird2, by=c("year","species","state")) %>% head()


#####################################
## CHALLENGE
#####################################

# Calculate the mean presence in 2010
# of 2 randomly selected a_states 
# Hint: Use the dplyr functions sample_n(), 
# they have similar syntax to other dplyr functions.
# ?sample_n for help

ebird %>%
  filter(year==2010,
         state %in% i_states) %>%
  group_by(state) %>%
  sample_n(2) %>%
  summarize(mean=mean(presence))

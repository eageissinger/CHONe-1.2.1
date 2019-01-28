# ---- set working directory -----
setwd("C:/Users/USER/Documents/Research/CHONe-1.2.1/")

# load data ----
tank_survival<-read.csv("./data/data-working/tank-survival-exp.csv",header=TRUE)

# ---- load packages ----
library(tidyverse)

# data manimpulation
glimpse(tank_survival)
tank_survival<- tank_survival%>%
  rename(year=ï..year)
tank_survival$ID<-seq.int(nrow(tank_survival))

df1<-tank_survival%>% 
  uncount(number) %>%
  mutate(status = 0)

# create dataframe with the dead fish


# Add a fish_id column that identifies individuals in each sample
df2<-tank_survival %>% 
  group_by(ID) %>% 
  mutate(fish_id = row_number()) %>% 
  ungroup()


# Identify combinations that don't exist (i.e. sample c only had 2 fish so itdoesn't have fish_id 3) and then create those
# combinations and fill the status column in with 1 for dead
df3 <- complete(df2, sample, fish, fill = list(status = 1))



# From here the tricky part is just filling in the blanks
# Since the output is sorted by sample you should be able to say that all the NAs can be filled by whatever value is above them,
# from top to bottom...
# If that's the case, you can use fill()

myData3 %>% fill(info)
sample   fish info   status
1   a         1    value1     0
2   a         2    value1     1
3   a         3    value1     1
4   b         1    value2     0
5   b         2    value2     0
6   b         3    value2     0
7   c         1    value3     0
8   c         2    value3     0
9   c         3    value3     1

library(data.table)
library(ddplyr)
library(lubridate)

S8 <- fread("../RawDataProcessing/Data/CleanedCaptures/S8_cleaned.csv")
head(S8)

S8 %<>% mutate(., dt = ymd_hms(newtime))
S8 %>% group_by(., invalid) %>% summarise(., n())

# Check if more than 1 invalid type per capture
capsummary <- S8 %>% 
  group_by(., season, site, roll, capture) %>%
  summarise(., typecount = n_distinct(invalid), images = n(), inv = mean(invalid)) 
    # inv lets me see in the summaries if there is > 1 type of invalid count

InvalidsPerCap <- capsummary %>% filter(., typecount >1)
ImsPerCap <- capsummary %>% filter(., images >3) # this includes captures with > 1 invalid type, but inv column lets me see this

filter(ImsPerCap, inv <=1) # looks like most of the big captures are completely or partly invalids

# double checking: only investigating invalid == 0
ImsPerCap2 <- S8 %>% filter(., invalid == 0) %>%
  group_by(., season, site, roll, capture) %>%
  summarise(., images = n()) %>%
  filter(., images > 3)


#how long does any given duration span? - perhaps spot check these
capduration <- S8 %>% filter(., invalid == 0) %>% group_by(., season, site, roll, capture) %>% 
  summarise(., ims = max(image), dur = max(dt) - min(dt)) %>%
  mutate(duration = as.numeric(dur))

View(capduration %>% filter(., duration > 2)) # 178 with > 2 second duration

### generally are valid dates within S8? Sep 2013 thru July 2014?
S8 %>% filter(., invalid == 0) %>% summarise(., min = min(dt), max = max(dt))

## check roll duration -- all look within reason
rollsum <- S8 %>% filter(., invalid == 0) %>% group_by(., season, site, roll) %>% 
  summarise(., minD = min(dt), maxD = max(dt), dur = max(dt) - min(dt))
filter(rollsum, dur > 100) # all rolls < 3.5 months or so

# just looking at distribution of start/end times
ggplot(data = rollsum, aes(x = minD)) + geom_bar()
ggplot(data = rollsum, aes(x = maxD)) + geom_bar()




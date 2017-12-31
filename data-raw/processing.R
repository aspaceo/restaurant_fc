## libraries  -----------------------------------------------------------------
library(dplyr)
library(magrittr)

## load data files  -----------------------------------------------------------
## air  -----------------------------------------
air_reserve <- read.csv(file = 'data-raw/air_reserve.csv', stringsAsFactors = F)
air_store_info <- read.csv(file = 'data-raw/air_store_info.csv', stringsAsFactors = F)
air_visit <- read.csv(file = 'data-raw/air_visit_data.csv', stringsAsFactors = F)
## hpg  -----------------------------------------
hpg_reserve <- read.csv(file = 'data-raw/hpg_reserve.csv', stringsAsFactors = F)
hpg_store_info <- read.csv(file = 'data-raw/hpg_store_info.csv', stringsAsFactors = F)
## meta  ----------------------------------------
date <- read.csv(file = 'data-raw/date_info.csv', stringsAsFactors = F)
store_id <- read.csv(file = 'data-raw/store_id_relation.csv', stringsAsFactors = F)
sample_sub <- read.csv(file = 'data-raw/sample_submission.csv', stringsAsFactors = F)

## setting data types and building single data set  ---------------------------
## setting dates  -------------------------------------------------------------
air_reserve$visit_datetime <- as.Date(air_reserve$visit_datetime)
air_reserve$reserve_datetime <- as.Date(air_reserve$reserve_datetime)
air_visit$visit_date <- as.Date(air_visit$visit_date)
hpg_reserve$visit_datetime <- as.Date(hpg_reserve$visit_datetime)
hpg_reserve$reserve_datetime <- as.Date(hpg_reserve$reserve_datetime)
date$calendar_date <- as.Date(date$calendar_date)

## processing sample submission  ----------------------------------------------
## split date from key  -------------------------------------------------------
sample_sub$calendar_date <- as.Date(substr(sample_sub[, 1], start = 22, stop = 31))
sample_sub$air_store_id <- substr(sample_sub[, 1], start = 1, stop = 20)
sample_sub$forecast <- 1


## building store dataset  ----------------------------------------------------
store_id$joiner <- 1
date$joiner <- 1
ts <- inner_join(store_id, date, by = 'joiner') %>%
  select(., air_store_id, hpg_store_id, calendar_date, day_of_week, holiday_flg)
summary(ts)
## determine whether a forecast is required for each row  ---------------------
head(ts)
ts <- select(sample_sub, air_store_id, calendar_date, forecast) %>%
  left_join(x = ts, y = ., by = c('air_store_id', 'calendar_date'))
## replace forecast NAs with 0  -----------------------------------------------
ts$forecast[is.na(ts$forecast)] <- 0

## attach sales  --------------------------------------------------------------
hpg_visits <- hpg_reserve %>%
  group_by(hpg_store_id, visit_datetime) %>%
  summarize(hpg_visits = sum(reserve_visitors))
names(hpg_visits)[2] <- 'calendar_date'
ts <- left_join(ts, hpg_visits, by = c('hpg_store_id', 'calendar_date'))
rm(hpg_visits)


## source vists from air system  ----------------------------------------------
names(air_visit)[2] <- 'calendar_date'
ts <- left_join(ts, air_visit, by = c('air_store_id', 'calendar_date'))
rm(tst)



## questions to answer  -------------------------------------------------------
  ## which stores require forecasting  ??
  ## which stores are i) present in both files, ii) only one  ??

##  ============================================================================
## scratch  -------------------------------------------------------------------
##  ============================================================================

head(ts)
## frequency of each air store  -----------------------------------------------
table(ts$air_store_id)
## ts looks complete, but we know stores are missing from each dataset,
## presuamably we have store ids but no corresponding data; ie. there must
unique(ts$air_store_id) %in% unique(air_reserve$air_store_id)
dim(store_id)

## sample submission   --------------------------------------------------------
head(sample_sub)
air_00a91d 4 2 b 0 8 b 0 8 d 9 _ 2017
123456789101112131415161718192021


## structure of hpg_reserve dataset  ------------------------------------------
tst <- hpg_reserve %>%
  filter(., hpg_store_id == 'hpg_de24ea49dc25d6b8')
table(tst$visit_datetime)
plot(tst$reserve_visitors, type = 'l', col = 'green')

## structure of air visit info   ----------------------------------------------
filter(store_id, hpg_store_id == 'hpg_de24ea49dc25d6b8')
tst_a <- air_visit %>%
  filter(., air_store_id == 'air_947eb2cae4f3e8f2')
plot(tst_a$visitors, type = 'l', col = 'blue')





head(store_id)
head(hpg_reserve)
head(date)
str(air_reserve)
tail(air_store_info)
table(air_reserve$air_store_id)

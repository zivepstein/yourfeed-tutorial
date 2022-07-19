#yourfeed tutorial 7/19/2022 IC2S2
#https://www.yourfeed.social?experiment_id=e3pG6d&user_id=enter_your_username
library(tidyverse)
source("utils.R")

src <- "../data/e3pG6d_engagement.csv"
dat <- fread(src)
dat <- get_data(src = src, n_entities = 50)
useritem <- make_dwell_data(dat, output = output, max_dwell_ms = 30000, min_dwell_ms = 250, exclude_start_end = c(3, 3))
# clean userform data
src <- "../data/e3pG6d_user_form.csv"
user_form <- make_user_form(src)

plot_user(useritem, sample(unique(useritem$user_id),1))

item_level  <- fread( "../data/oct9_news_stimulus_set.csv")
useritem <- useritem %>% left_join(item_level, by='entity_id')
top_dwells <- useritem %>% group_by(entity_id) %>% summarize(
  ldwell = mean(ldwell, na.rm=T),
  url = max(entity_url_x)
) %>% arrange(desc(ldwell)); head(top_dwells$url)

# yourfeed-tutorial
This repo contains code to interact with the Yourfeed tutorial. 

First, we will start with the [Create Study page](https://www.yourfeed.social/create-study) to play with the various settings.

Then, we go the Yourfeed link to generated some data. [Here](https://www.yourfeed.social?experiment_id=e3pG6d&user_id=backup) is a example link. 

Then, we go the [Download Data page](https://www.yourfeed.social/download) to download the data so we can interact with it locally. 

```
src <- "../data/e3pG6d_engagement.csv"
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
```


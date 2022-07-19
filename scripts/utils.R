library(data.table); library(dplyr)

mean_error <- function(data){
  n <- sum(!is.na(data))
  s <- sd(data,na.rm = T)
  return(1.96*s/sqrt(n))
}

get_data <- function(src, output = NULL, n_entities = 120) {
  eng <- fread(src)
  message(paste0(n_distinct(eng$user_id)), " users")
  
  # recode time columns to seconds since first event 
  eng[time_stamp >= 0, timestamp := as.numeric((time_stamp - min(time_stamp, na.rm = T))) / 1000]
  eng[time_engagement >= 0, time_engage := as.numeric(time_engagement - min(time_engagement, na.rm = T)) / 1000]
  eng[time_content_viewed >= 0, time_view := as.numeric(time_content_viewed - min(time_content_viewed, na.rm = T)) / 1000]
  eng[, `:=` (time_stamp = NULL, time_engagement = NULL, time_content_viewed = NULL)]
  
  # mobile/desktop
  eng[, mobile := 0]
  eng[grepl("[Mm]obile", user_agent), mobile := 1]
  eng$user_agent <- NULL
  
  # headline veracity
  eng[grepl("/t_", entity_url), veracity := 1]
  eng[grepl("/f_", entity_url), veracity := 0]
  eng[, is_true := ifelse(veracity == 1, 1, 0)]
  eng[, is_false := ifelse(veracity == 0, 1, 0)]
  eng$entity_url <- NULL
  
  # new entity-cat column
  eng[, entity_category4 := entity_category]
  eng[entity_category4 == "news", 
      entity_category4 := ifelse(veracity == 1, 'news_true', 'news_false')]
  
  # remove users without n_entities
  to_exclude <- eng[, .(unique_entities = n_distinct(entity_id)), user_id]
  to_exclude <- to_exclude[unique_entities < n_entities][order(-unique_entities)]
  message(paste0("Remove ", nrow(to_exclude),  " users without ", n_entities, " unique entities"))
  print(to_exclude)
  eng <- eng[!user_id %in% to_exclude$user_id]
  message(paste0(n_distinct(eng$user_id)), " users with ", n_entities, " unique entities")
  
  # convert all int columns to numeric
  cols2convert <- colnames(eng)[sapply(eng, class) %in% c("integer64", "integer")]
  for (c in cols2convert) {
    eng[, (c) := as.numeric(get(c))]
  }
  eng[, condition := as.numeric(condition)]
  
  # sort
  eng <- eng[order(user_id, item_order, time_view, time_engage, timestamp)]
  
  return(data.table(eng))
}


make_dwell_data <- function(dat, output = NULL, max_dwell_ms = 30000, min_dwell_ms = NULL, exclude_start_end = c(3, 3)) {
  message("Note: Multiple dwells for a single entity will be summed.")
  message(paste0("Note: Dwell times for first ", exclude_start_end[1], " and last ", exclude_start_end[2], " entities convert to NA."))
  
  dt_dwell <- dat[engagement_type == "dwell"]
  dt_nondwell <- dat[engagement_type != "dwell"]
  dt_dwell[, response := as.numeric(response)]
  # rename
  setnames(dt_dwell, c("response"), c("dwell"))
  
  n_long_dwells <- dt_dwell[dwell >= max_dwell_ms, .N]
  perc_long_dwells <- round(n_long_dwells / nrow(dt_dwell) * 100, 2)
  message(paste0(n_long_dwells, " (", perc_long_dwells, "%) dwells >", max_dwell_ms, " ms"))
  
  # compute dwell for each user and entity
  dt_dwell2 <- dt_dwell[, .(dwell = sum(dwell, na.rm = T), 
                            n_dwells = .N,
                            mobile = mean(mobile, na.rm = T),
                            btn_likes = mean(likes, na.rm = T),
                            btn_shares = mean(shares, na.rm = T),
                            condition = mean(condition, na.rm = T),
                            item_order = mean(item_order, na.rm = T),
                            is_true = mean(is_true, na.rm = T),
                            is_false = mean(is_false, na.rm = T)), 
                    keyby = .(user_id, entity_id, veracity, entity_category, entity_category4, entity_set, experiment_id)]
  dt_dwell2 <- dt_dwell2[order(user_id, item_order)]
  
  # exclude outliers after summing (or before summing?)
  dt_dwell2[dwell > max_dwell_ms, dwell := NA]
  # convert all dwells < min_dwell_ms to min_dwell_ms
  if (!is.null(min_dwell_ms)) {
    dt_dwell2[!is.na(dwell) & dwell < min_dwell_ms, dwell := min_dwell_ms]
  }
  
  # remove first/last n entities
  item_orders <- dt_dwell2[, unique(item_order)]
  trim1 <- head(item_orders, exclude_start_end[1])
  trim2 <- tail(item_orders, exclude_start_end[2])
  dt_dwell2[item_order %in% c(trim1, trim2), dwell := NA]
  
  # add 1ms before taking log to avoid log(0) later
  dt_dwell2[, dwell := dwell + 1]
  dt_dwell2[, ldwell := log(dwell)]
  
  # calculate engaements (e.g., likes, shares, unlikes, unshares)
  nondwell_count <- dt_nondwell[, .(n = .N), keyby = .(user_id, entity_id, engagement_type, response)]
  nondwell_wide <- dcast(nondwell_count, user_id + entity_id ~ engagement_type + response, value.var = c("n"), fill = 0)
  
  eng3 <- data.table(dplyr::left_join(dt_dwell2, nondwell_wide))
  replace_na_columns <- names(select(eng3, -(user_id:ldwell)))
  eng3[, (replace_na_columns) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = replace_na_columns]
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(eng3, output)
  }
  return(data.table(eng3))
}


get_all_dwells <- function(dat, output = NULL, max_dwell_ms = 30000, min_dwell_ms = NULL, exclude_start_end = c(3, 3)) {
  message("Note: Multiple dwells for a single entity will NOT be summed.")
  message(paste0("Note: Dwell times for first ", exclude_start_end[1], " and last ", exclude_start_end[2], " entities convert to NA."))
  dt_dwell <- dat[engagement_type == "dwell"]
  dt_nondwell <- dat[engagement_type != "dwell"]
  dt_dwell[, response := as.numeric(response)]
  # rename
  setnames(dt_dwell, c("response", "likes", "read_later", "shares", "accuracy_score"),
                     c("dwell", "btn_likes", "btn_read_later", "btn_shares", "btn_accuracy_score"))
  
  
  n_long_dwells <- dt_dwell[dwell >= max_dwell_ms, .N]
  perc_long_dwells <- round(n_long_dwells / nrow(dt_dwell) * 100, 2)
  message(paste0(n_long_dwells, " (", perc_long_dwells, "%) dwells >", max_dwell_ms, " ms"))
  
  # exclude outliers after summing (or before summing?)
  dt_dwell[dwell > max_dwell_ms, dwell := NA]
  # convert all dwells < min_dwell_ms to min_dwell_ms
  if (!is.null(min_dwell_ms)) {
    dt_dwell[!is.na(dwell) & dwell < min_dwell_ms, dwell := min_dwell_ms]
  }
  
  # remove first/last n entities
  item_orders <- dt_dwell[, unique(item_order)]
  trim1 <- head(item_orders, exclude_start_end[1])
  trim2 <- tail(item_orders, exclude_start_end[2])
  dt_dwell[item_order %in% c(trim1, trim2), dwell := NA]
  
  # add 1ms before taking log to avoid log(0) later
  dt_dwell[, dwell := dwell + 1]
  dt_dwell[, ldwell := log(dwell)]
  
  # calculate engaements (e.g., likes, shares, unlikes, unshares)
  nondwell_count <- dt_nondwell[, .(n = .N), keyby = .(user_id, entity_id, time_view, engagement_type, response)]
  nondwell_wide <- dcast(nondwell_count, user_id + entity_id + time_view ~ engagement_type + response, value.var = c("n"), fill = 0)
  
  eng3 <- data.table(dplyr::left_join(dt_dwell, nondwell_wide))
  
  # replace NAs with 0 in the engagement columns
  replace_na_columns <- names(select(eng3, -(user_id:ldwell)))
  eng3[, (replace_na_columns) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = replace_na_columns]
  
  # remove columns and unusable events
  eng3 <- eng3[!is.na(time_view)]
  eng3$time_engage <- NULL
  
  # sort
  eng3 <- eng3[order(user_id, time_view, entity_id)]
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(eng3, output)
  }
  return(data.table(eng3))
}

make_user_form <- function(src, output = NULL) {
  d <- fread(src)
  d <- d[!is.na(response) & response != ""]
  d <- d[order(user_id, field, time_stamp)]
  message(paste0(n_distinct(d$user_id), " users"))
  
  checkpoints <- d[field == "checkpoint"]
  responses <- d[field != "checkpoint"]
  responses <- distinct(responses)
  responses[, field := tolower(gsub("-", "_", field))]
  
  # select final/last response for each user_id/field (in case people responded multiple times)
  responses <- responses[, .SD[.N], keyby = .(user_id, field)]
  form_data <- dcast(responses, user_id + experiment_id ~ field, value.var = c("response"))
  
  # # sum crt responses
  # form_data$crt <- (form_data$crt_10_50_printer == '10') + grepl("emily", tolower(form_data$crt_emily_april_may)) + (form_data$crt_mark_adam_28 == '4') + (form_data$crt_mold_bread_40 == '39') + (form_data$crt_pass_second_place == '2') + (form_data$crt_sheep_15_8_died == '8')
  # n_crt <- sum(grepl("crt_", names(form_data)))
  # setDT(form_data)
  # form_data[, crt := crt / n_crt]
  # 
  # # convert type
  # form_data[, `:=` (age = as.numeric(age),
  #                   ses_ladder = as.numeric(ses_ladder), 
  #                   ses_education_years = as.numeric(ses_education_years), 
  #                   social_media_followers = as.numeric(social_media_followers),
  #                   social_media_following = as.numeric(social_media_following))]
  # 
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(form_data, output)
  }
  return(data.table(form_data))
}



make_itemlevel <- function(src, output = NULL, dwell_data = NULL) {
  covs <- fread(src)
  setnames(covs, tolower(names(covs)))  # lower case col names
  setnames(covs, gsub(".", "_", names(covs), fixed = TRUE))  # convert . to _
  
  covs$response <- NULL
  covs$experiment_id <- NULL
  covs <- covs[, lapply(.SD, function(x) ifelse(x == 'null', NA, x))]  # convert nulls to NA
  
  # convert to numeric
  char_cols <- c("entity_id", "entity_url", "entity_category", "entity_set")
  num_cols <- names(covs)[!names(covs) %in% char_cols]
  covs[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  # veracity
  covs[grepl("/t_", entity_url), veracity := 1]
  covs[grepl("/f_", entity_url), veracity := 0]
  covs$time_stamp <- NULL
  
  # new entity-cat column
  covs[, entity_category4 := entity_category]
  covs[entity_category4 == "news", 
      entity_category4 := ifelse(veracity == 1, 'news_true', 'news_false')]
  
  if (!is.null(dwell_data)) {
    dwell_avg <- dwell_data[, .(times_shown = .N,  # assumes single dwell per entity per user
                                dwells_total = sum(n_dwells, na.rm = T),  # account for multiple dwells per entity
                                likes_0_sum = sum(likes_0, na.rm = T),
                                likes_1_sum = sum(likes_1, na.rm = T),
                                shares_0_sum = sum(shares_0, na.rm = T),
                                shares_1_sum = sum(shares_1, na.rm = T),
                                mean_order = mean(item_order, na.rm = T),
                                dwell = mean(dwell, na.rm = T)), 
                            keyby = .(entity_id)]
    dwell_avg[, ldwell := log(dwell)]
    covs <- data.table(left_join(covs, dwell_avg))
  }
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(covs, output)
  }
  return(data.table(covs))
}


entity_political_concordance <- function(dwell_data, user_form, item_level, scale_range = c(1, 6), output = NULL) {
  dwell_entity <- dwell_data[, .(user_id, entity_id, veracity, entity_category, entity_category4)]
  
  user_politicalpref <- user_form[!is.na(politicalpref), .(user_id, politicalpref)]
  user_politicalpref[, sort(unique(politicalpref))]
  user_politicalpref[, politicalpref := as.numeric(substring(politicalpref, 1, 1))]
  
  entity_partisan <- item_level[!is.na(partisanship_combined), .(entity_id, partisanship_combined)]
  
  partisan_recode <- left_join(dwell_entity, user_politicalpref) %>% left_join(entity_partisan) %>% data.table()
  # reverse the score for democrats (beacuse higher = favor republicans)
  partisan_recode[, partisanship_concordance := partisanship_combined]
  partisan_recode[politicalpref <= 3, partisanship_concordance := scale_range[2] - partisanship_combined + scale_range[1]]
  partisan_recode[is.na(politicalpref), partisanship_concordance := NA]
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(partisan_recode, output)
  }
  return(data.table(partisan_recode))
}



id2url <- function(eid, item_level = NULL) {
  if ("item_level" %in% ls(envir = .GlobalEnv)) {
    item_level <- get("item_level", envir = .GlobalEnv)
  } else {
    stop("item_level variabl/object not found in global environment. Provide item_level argument")
  }
  return(data.table(item_level)[entity_id == eid, entity_url])
}


plot_user <- function (useritem, userid){
  data <- useritem %>% filter(user_id == userid)
  plot(data$item_order, data$dwell, main = userid)
  lines(data$item_order, data$dwell, lty=2)
  points(data[data$shares_1 == 1,]$item_order, data[data$shares_1 == 1,"dwell"]$dwell, col='red',pch=16)
  points(data[data$likes_1 == 1,]$item_order, data[data$likes_1 == 1,"dwell"]$dwell, col='yellow',pch=16)
}


# make_eng_data <- function(){
#   eng <- read.csv("../data/prolific2/eNKqay_engagement.csv")
#   eng <- eng %>% mutate(
#     mobile = grepl("Mobile",eng$user_agent),
#     is_true = grepl("/t_",eng$entity_url),
#     is_false = grepl("/f_",eng$entity_url)
#   )
#   return(eng)
# }
#
# 
# make_dwell_data <- function(eng){
#   eng_useritem <- eng  %>% filter(engagement_type =='dwell')  %>% group_by(entity_id, user_id) %>% dplyr::summarize(
#     dwell = sum(response),
#     mobile = mean(mobile),
#     likes = mean(likes),
#     shares = mean(shares),
#     condition = mean(as.numeric(condition)),
#     item_order = mean(item_order),
#     is_true = mean(as.numeric(is_true)),
#     is_false = mean(as.numeric(is_false))
#   )
#   
#   dt0 <- data.table(eng)
#   dt0[, platform := "desktop"]
#   dt0[grepl("[Mm]obile", user_agent), platform := "mobile"]
#   dt0 <- dt0[condition != "null"]
#   ids <- dt0[, n_distinct(entity_id), user_id][V1 == 120, user_id]
#   dt0 <- dt0[user_id %in% ids]
#   dt0$dwell <- as.numeric()
#   dt0[engagement_type == "dwell", dwell := response + 1]
#   dt0[dwell > 30000, dwell := NA]
#   dt0[, ldwell := log(dwellClean)]
#   return(dt0)
# }
# 
# make_user_level <- function(){
#   d <- read.csv("../data/prolific2/eNKqay_user_form.csv")
#   fields <- unique(d$field)
#   users <- unique(d$user_id)
#   z <- matrix(nrow=length(users),ncol=length(fields))
#   rownames(z) <- users
#   colnames(z) <- fields
#   for (i in 1:length(users)){
#     for (j in 1:length(fields)){
#       r <-  d[d$user_id == users[i] & d$field ==fields[j],]$response
#       if(length(r) ==1 && fields[j] != 'checkpoint'){
#         z[i,j] <- r  
#       } else if(length(r) >1 && fields[j] != 'checkpoint'){
#         z[i,j] <- r[1]
#       }else{
#         z[i,j] <- -1
#       }
#     }
#   }
#   form_data <- data.frame(z)
#   form_data$user_id <- rownames(form_data)
#   form_data$crt <- (form_data$crt_10.50.printer == '10') +  grepl("emily", tolower(form_data$crt_emily.april.may)) + (form_data$crt_mark.adam.28 == '4') + (form_data$crt_mold.bread.40 == '39') + (form_data$crt_pass.second.place == '2') + (form_data$crt_sheep.15.8.died == '8')
#   return(form_data)
# }




# NOTE: probably not correct way to count likes/shares because unshares/unlikes not accounted for
# make_useritem<- function(eng){
#   
#   dwell_data <-make_dwell_data(eng)
#   form_data <- make_user_level()
#   user_level_covs <- dwell_data %>% group_by(user_id) %>% dplyr::summarize(
#     pfake = mean(is_false),
#     mobile = mean(mobile),
#     n_dwell = n()
#   )
#   
#   item_level_covs <- distinct(eng, entity_id, .keep_all = T) %>% select(entity_id, entity_category)
#   
#   share <- eng  %>% filter(engagement_type =='shares') %>% group_by(entity_id, user_id) %>% dplyr::summarize(
#     item_order = mean(item_order),
#     did_share=1
#   )
#   like <- eng  %>% filter(engagement_type =='likes') %>% group_by(entity_id, user_id) %>% dplyr::summarize(
#     item_order = mean(item_order),
#     did_like=1
#   )
#   
#   useritem <- dwell_data %>% left_join(share)  %>% left_join(like)  %>% left_join(user_level_covs)%>%left_join(form_data)%>% left_join(item_level_covs) %>% mutate(
#     did_share = replace_na(did_share, 0),
#     did_like = replace_na(did_like, 0)
#   )
#   return(useritem)
# }


# 
# make_itemlevel <- function(useritem){
#   covs <- read.csv("../data/prolific2/eNKqay_entity_info.csv")
#   item_level <- useritem %>% group_by(entity_id)  %>% dplyr::summarize(
#     adwell  = mean(ldwell, na.rm=T),
#     num_likes = sum(did_like),
#     alikes = mean(did_like),
#     avg_order = mean(item_order),
#     ashares = mean(did_share),
#     num_shares = sum(did_share)
#   ) %>% mutate(
#     entity_category = case_when(
#       substring(entity_id,1,1) == 'o' ~ "news",
#       substring(entity_id,1,2) == 'ns' ~ "sensational",
#       substring(entity_id,1,2) == 'no' ~ "opinion",
#     )
#   ) %>% arrange(entity_category, desc(adwell)) %>% left_join(covs)
# return(item_level)
# }








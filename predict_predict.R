library(tidyverse)
library(ggrepel)
library(nycflights13)
library(data.table)

options(scipen = 999)

slope <-
    function(y2, y1, x2, x1){
        (y2-y1)/(x2-x1)
    }


distance <-
    function(x2, x1, y2, y1){
        sqrt((x2 - x1)^2 + (y2 - y1)^2)
    }

round_down <-
    function (x, accuracy, integer = F) {
        x_sign <- sign(x)
        x_int <- abs(as.integer(x))
        x_dec <- abs(x) - x_int

        if (integer == F) {
            get_round <-(x_dec%/%accuracy) * accuracy
            final <- (x_int + get_round) * x_sign
        } else {
            get_round <-(x_int%/%accuracy) * accuracy
            final <- (get_round) * x_sign
        }
        
        return(final)
    }

#relative difference: takes value into account 
    #15 would be 50% below 30 (15/30) 
    #65 would be 50% above than 30 (30 + (100-30)/2)
rel_diff <- 
    function(val, ref){
        ifelse(val < ref, 
               (val-ref)/ref,
               (val-ref)/(100-ref))
    }



df <- msleep; set_dv <- "sleep_total > 12"; set_ignore <- c('name', 'genus');
#df <- mpg; set_dv <- "ntile(cty, 4) > 3"; set_ignore <- "cty";
#df <- iris; set_dv <- "Species != 'versicolor'"; set_ignore <- "Species";
#df <- mtcars; set_dv <- "vs == 1"; set_ignore <- "vs";
#df <- flights %>% mutate(wday = weekdays(time_hour)); set_dv <- "dep_delay > 40"; set_ignore <- c("arr_delay", "dep_delay", "dep_time", "arr_time","year", "time_hour", "sched_arr_time", "hour");

#df <- read.csv("../../../Desktop/kaggle/No-show-Issue-Comma-300k.csv", stringsAsFactors = F);set_dv <- "Status == 'No-Show'";set_ignore <- c("Status", "AppointmentRegistration", "ApointmentData")
#df <- read.csv("../../../Desktop/kaggle/code4philly_cognoma.csv", stringsAsFactors = F);set_dv <- "dead == 1";set_ignore <- c("dead", "acronym", "organ_of_origin", "days_survived")
#df <- read.csv("../../../Desktop/kaggle/HR_comma_sep.csv", stringsAsFactors = F);set_dv <- "left == 1";set_ignore <- c("left")
#df <- read.csv("J:/data scanner/COGNOMA/samples.csv", stringsAsFactors = F) %>% filter(complete.cases(.));set_dv <- "dead == 0";set_ignore <- c("dead")

#df <- fread("https://raw.githubusercontent.com/Microsoft/r-server-hospital-length-of-stay/master/Data/LengthOfStay.csv", stringsAsFactors = F);set_dv <- "lengthofstay > fivenum(df$lengthofstay)[3]";set_ignore <- c("lengthofstay")

set.seed(1234)
base_data <-
    df %>% sample_frac(0.1) %>% 
    mutate_(var = set_dv) %>% 
    filter(!is.na(var)) %>% 
    select(-one_of(set_ignore)) %>%
    mutate_if(.predicate = 
                  (function(x)
                      (is.numeric(x) | is.integer(x)) &
                       n_distinct(x) > 15),
              .funs = funs(ntile(., 10))
    ) %>% 
    select_if(function(x) n_distinct(x) < 100) %>% 
    #filter(complete.cases(.)) %>%
    #mutate_at(vars(-var), funs(as.character)) #%>% 
    mutate(id = row_number())

df_train <- sample_frac(base_data, 0.8)
df_test <- setdiff(base_data, df_train)

var_freq <- mean(df_train$var*100) 

get_vars <- names(df_test %>% select(-c(id, var)))

agg_fields <-
    function(i){
        df_train %>% 
        select(value = i, var) %>% 
        mutate(field = names(df_train)[i],
               value = as.character(value)) %>% 
        group_by(field, value, var) %>% 
        summarise(n = n()) %>% 
        ungroup() %>% 
        mutate(var = n * var) %>% 
        group_by(field, value) %>% 
        summarise(n = sum(n),
                  sum_var = sum(var)) %>% 
        ungroup()
    }

get_fields <- 
    agg_fields(1)


for(i in 2:length(get_vars)){
    get_fields <-
        bind_rows(get_fields,
                  agg_fields(i))
    
    print(i)
}

get_values <-
    get_fields %>% 
    group_by(field) %>% 
    mutate(rep_field = n/sum(n)*100,
           n_field = sum(n),
           sum_field = sum(sum_var)) %>% 
    ungroup() %>% 
    mutate(#p_field = sum_field/n_field*100,
           p_value = sum_var/n*100,
           d_value = rel_diff(p_value, var_freq),
           abs_d_value = abs(d_value),
           d_rep = d_value * rep_field) %>% 
    group_by(field) %>% 
    mutate(v_field = #max(p_value) - min(p_value)) %>% #
               var(p_value)) %>% 
    ungroup() %>% 
    mutate(d_x_v = d_value * v_field,
           d_x_sqv = d_value * sqrt(v_field),
           slope_value = slope(v_field, 0, d_value, 0),
           dist_value = distance(d_value, 0, v_field, 0)*sign(d_value),
           s_x_d = slope_value * d_value) %>% #filter(abs_d_value >= 1)
    mutate(field = fct_reorder(field, d_value, .fun = max, .desc = T))

compare_values <-
    df_test %>% 
    gather(field, value, -c(id, var)) %>%
    mutate(value = as.character(value)) %>% 
    left_join(get_values) %>% 
    filter(complete.cases(.))

final_estimate <-
    compare_values %>% 
    group_by(var, id) %>% 
    summarise(#sum_diff = sum(d_value),
              #side_diff = sum_diff > 0,
              
              mean_diff = mean(d_value),
              side_mdiff = mean_diff > 0#,
              
              #sum_sxd = sum(s_x_d),
              #side_sum_sxd = sum_sxd > 0,
              
              #sum_slope = sum(slope_value),
              #side_sum_slope = sum_slope > 0,
              
              #sum_dist = sum(dist_value),
              #side_sum_dist = sum_dist > 0,
              #mean_dist = mean(dist_value),
              #side_mean_dist = mean_dist > 0,
              
              #sum_dxv = sum(d_x_v),
              #side_dxv = sum_dxv > 0,
              
              #mean_p_value = mean(p_value),
              #side_mean = mean_p_value > mean(base_data$var)*100,
              
              #sum_dxsqv = sum(d_x_sqv),
              #side_dxsqv = sum_dxv > 0
    ) %>% 
    ungroup() %>% 
    mutate(#sum_sxd_correct = side_sum_sxd == var,
           #sum_slope_correct = side_sum_slope == var,
           #sum_dist_correct = side_sum_dist == var,
           #mean_dist_correct = side_mean_dist == var,
           #diff_correct = side_diff == var,
           mdiff_correct = side_mdiff == var#,
           #dxv_correct = side_dxv == var,
           #dxsqv_correct = side_dxsqv == var,
           #mean_correct = side_mean == var
        )




final_estimate %>% 
    summarise_at(vars(contains("_correct")), funs(mean(.)*100)) %>% 
    t()

final_estimate %>% 
    group_by(var, side_mdiff) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(pct = n/sum(n) %>% round(3)*100) %>% 
    select(-n) %>% 
    spread(var, pct)
    

ggplot(final_estimate) + 
    geom_jitter(aes(var, mean_diff, color = mdiff_correct), alpha = .4, width = .1) +
    theme_minimal()


get_id <- 1 #max(id)

one_obs_profile <- 
    compare_values %>% 
    filter(id == get_id) %>% 
    mutate(est_x = #min(d_value) + (max(d_value)-min(d_value))/2, #
               median(d_value),
           est_y = #min(v_field) + (max(v_field)-min(v_field))/2) #
               mean(v_field))
    #left_join(one_obs_est %>% select(id, ))


ggplot(get_values, aes(x = p_value, y = v_field, color = reorder(field, -v_field, FUN = max))) +
    geom_vline(xintercept = mean(base_data$var), color = "grey50", size = 4, alpha = .5) +
    geom_line(aes(group = field), color = "grey60", alpha = .2) +
    #geom_segment(aes(xend = 0, yend = 0), alpha = .2) +
    geom_point(aes(size = n), alpha = .9) +
    geom_segment(data = one_obs_profile, 
                 aes(xend = est_x, yend = est_y), 
                 color = "black", size = 1) +
    geom_point(data = one_obs_profile, color = "black", shape = 21, size = 3, stroke = 1.5) +
    geom_point(data = one_obs_profile, aes(est_x, est_y),
               color = "black",
               #color = "blue", fill = "white", shape = 21, 
               size = 4) +
    geom_text(data = one_obs_profile, aes(est_x, est_y), label = "?", color = "white", size = 4) +
    theme(panel.background = element_rect(fill = "white")) +
    guides(size = F) +
    labs(x = paste0("group distance from pop avg (", floor(var_freq), ")"),
         y = "variance of x",
         color = "variable")

ggplot(compare_values %>% filter(id == get_id), 
       aes(p_value, v_field, label = paste(field, "\n",value), color = value)) +
    geom_vline(xintercept = var_freq, color = "grey50", size = 4, alpha = .5) +
    geom_label_repel(size = 4) +
    geom_point(aes(color = value), size = 3) +
    geom_segment(aes(x = p_value, y = v_field, xend = var_freq, yend = var_freq)) + 
    #xlim(-1,101) + 
    #ylim(0,1) +
    theme(legend.position = "none") +
    labs(x = paste0("group distance from pop proportion (", floor(var_freq), ")"),
         y = "variance of x",
         color = "variable")

ggplot(get_values, aes(d_value, field, color = field)) +
    geom_vline(xintercept = var_freq) +
    geom_line(size = 6, alpha = 0.2) +
    geom_point(aes(size = n)) +
    geom_point(data = one_obs_profile, aes(size = n), color = "black") +
    #xlim(-1,101) + 
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white")) +
    labs(x = "Avg")

ggplot(get_values, 
       aes(n, p_value, label = value, color = value)) +
    facet_wrap(~field, scales = "free_y") +
    geom_hline(yintercept = var_freq) +
    geom_point(aes(size = n)) +
    geom_label_repel(size = 5) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white")) +
    labs(title = "Distribution by field by value",
         subtitle = "The size of the point represents the # of obs.\nContinuous variables are shown as deciles, 1-10")



all_obs <-
    compare_values %>% 
    #filter(id == 1) %>% 
    select(id, field, d_value, v_field, n) %>% 
    mutate(x_n = d_value * n) %>% 
    group_by(id) %>%
    mutate(est_x = mean(d_value)) %>% #min(d_value) + (max(d_value)-min(d_value))/2) %>% 
    #arrange(desc(sign(est_x) * d_value)) %>% slice(1:2) %>% 
    mutate(side_same = ifelse(d_value > mean(base_data$var),#sign(est_x) == sign(d_value), 
                              v_field, NA),
           side_diff = ifelse(is.na(side_same), v_field, NA)) %>% 
    summarise(est_x = min(est_x),# %>% round_down(0.01),
              sum_same = sum(side_same, na.rm = T),
              sum_diff = sum(side_diff, na.rm = T),
              #est_y = sum(n, na.rm = T) #%>% round_down(2000, T)
              #est_y = mean(n, na.rm = T) #%>% round_down(2000, T)
              #est_y = sum(x_n, na.rm = T) #%>% round_down(2000, T)
              #est_y = mean(sum_same, na.rm = T) %>% round_down(2000, T)
              est_y = mean(sum_same, sum_diff, na.rm = T) %>% round_down(2000, T)
              ) %>% 
    ungroup()

hist(all_obs$est_y, breaks = 100)
hist(all_obs$est_x, breaks = 100)

ggplot() +
    theme(panel.background = element_rect(fill = "white")) +
    #geom_point(data = get_values, aes(x = d_value, y = v_field, color = reorder(field, -v_field), size = n)) +
    #geom_line(data = get_values, aes(x = d_value, y = n, color = reorder(field, -n))) +
    #geom_vline(xintercept = 0, color = "grey50", size = 4, alpha = .5) +
    geom_point(data = all_obs, aes(est_x, est_y), color = "black", alpha = 0.2) +
    labs(x = paste0("group distance from pop avg (", floor(var_freq), ")"),
         y = "",
         #color = "variable",
         title = "mean of outline (max, min of Y on each side) in each group")
    




{
    dat <- all_obs %>% select(d_value, v_field)
    ch <- 
        chull(dat)
    
    coords <- dat[c(ch, ch[1]), ]  # closed polygon
    
    plot(dat, pch=19)
    lines(coords, col="red")
    
    sf::
        
        plot(mean(coords$d_value), mean(coords$v_field), pch=20)
    
    }


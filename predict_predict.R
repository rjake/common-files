library(tidyverse)
library(ggrepel)


#df <- msleep; set_dv <- "sleep_total > 12"; set_ignore <- c('name', 'genus');
#df <- mpg; set_dv <- "ntile(cty, 4) > 2"; set_ignore <- "cty";
#df <- iris; set_dv <- "Species != 'versicolor'"; set_ignore <- "Species";
#df <- mtcars; set_dv <- "vs == 1"; set_ignore <- "vs";

#df <- read.csv("../../../Desktop/kaggle/No-show-Issue-Comma-300k.csv", stringsAsFactors = F);set_dv <- "Status == 'No-Show'";set_ignore <- c("Status", "AppointmentRegistration", "ApointmentData")
#df <- read.csv("../../../Desktop/kaggle/code4philly_cognoma.csv", stringsAsFactors = F);set_dv <- "dead == 1";set_ignore <- c("dead", "acronym", "organ_of_origin", "days_survived")

base_data <-
    df %>% 
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
    filter(complete.cases(.)) %>% 
    mutate(id = row_number())

df_train <- sample_frac(base_data, 0.9)
df_test <- setdiff(base_data, df_train)

var_freq <- mean(df_train$var*100) 

slope <-
    function(y2, y1, x2, x1){
        (y2-y1)/(x2-x1)
    }


distance <-
    function(x2, x1, y2, y1){
        sqrt((x2 - x1)^2 + (y2 - y1)^2)
    }

#slope(.25,0,-.3,0)

get_vars <- names(df_test %>% select(-c(id)))

df_train %>% group_by(gender) %>%  summarise(p = mean(var), n = n(), sum = sum(var), p2 = sum/n)



get_values <-
    df_train %>% 
    group_by_at(get_vars) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    gather(field, value, -c(var, n)) %>%
    #filter(field == "gender") %>% 
    mutate(var = var * n) %>% 
    #filter(!is.na(value)) %>% 
    group_by(field, value) %>% 
    #filter(max(row_number()) > 10) %>% 
    summarise(n = sum(n),
              sum_value = sum(var)) %>% 
    group_by(field) %>% 
    mutate(rep_field = n/sum(n)*100,
           n_field = sum(n),
           sum_field = sum(sum_value)) %>% 
    ungroup() %>% 
    mutate(#p_field = sum_field/n_field*100,
           p_value = sum_value/n*100,
           d_value = p_value - var_freq,#p_field,
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
           s_x_d = slope_value * d_value) #%>% filter(abs_d_value >= 1)


compare_values <-
    df_test %>% 
    gather(field, value, -c(id, var)) %>% 
    left_join(get_values) %>% 
    filter(complete.cases(.))

final_estimate <-
    compare_values %>% 
    group_by(var, id) %>% 
    summarise(sum_diff = sum(d_value),
              side_diff = sum_diff > 0,
              
              mean_diff = mean(d_value),
              side_mdiff = mean_diff > 0,
              
              sum_sxd = sum(s_x_d),
              side_sum_sxd = sum_sxd > 0,
              
              sum_slope = sum(slope_value),
              side_sum_slope = sum_slope > 0,
              
              sum_dist = sum(dist_value),
              side_sum_dist = sum_dist > 0,
              mean_dist = mean(dist_value),
              side_mean_dist = mean_dist > 0,
              
              sum_dxv = sum(d_x_v),
              side_dxv = sum_dxv > 0,
              
              mean_p_value = mean(p_value),
              side_mean = mean_p_value > mean(base_data$var)*100,
              
              sum_dxsqv = sum(d_x_sqv),
              side_dxsqv = sum_dxv > 0
    ) %>% 
    ungroup() %>% 
    mutate(sum_sxd_correct = side_sum_sxd == var,
           sum_slope_correct = side_sum_slope == var,
           sum_dist_correct = side_sum_dist == var,
           mean_dist_correct = side_mean_dist == var,
           diff_correct = side_diff == var,
           mdiff_correct = side_mdiff == var,
           dxv_correct = side_dxv == var,
           dxsqv_correct = side_dxsqv == var,
           mean_correct = side_mean == var
        )




final_estimate %>% 
    summarise_at(vars(contains("_correct")), funs(mean(.)*100)) %>% 
    t()

ggplot(final_estimate) + geom_count(aes(var, sum_diff, color = diff_correct))

one_obs_profile <- 
    compare_values %>% 
    filter(id == 13) %>% 
    mutate(est_x = median(d_value),
           est_y = median(v_field))
    #left_join(one_obs_est %>% select(id, ))
    

ggplot(get_values, aes(x = d_value, y = v_field, color = reorder(field, -v_field))) +
    geom_vline(xintercept = 0, color = "grey50", size = 4, alpha = .5) +
    geom_line(aes(group = field), color = "grey60", alpha = .2) +
    geom_segment(aes(xend = 0, yend = 0), alpha = .2) +
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
    labs(x = paste0("group distance from pop proportion (", floor(var_freq), "%)"),
         y = "variance of x",
         color = "variable")

ggplot(compare_values %>% filter(id == 38), 
       aes(p_value, v_field, label = paste(field, "\n",value), color = value)) +
    geom_vline(xintercept = var_freq, color = "grey50", size = 4, alpha = .5) +
    geom_label_repel(size = 4) +
    geom_point(aes(color = value)) +
    geom_segment(aes(x = p_value, y = v_field, xend = var_freq, yend = var_freq)) + 
    xlim(-1,101) + 
    #ylim(0,1) +
    theme(legend.position = "none")

ggplot(get_values, aes(p_value, field)) +
    geom_vline(aes(xintercept = var_freq)) +
    geom_line(aes(color = field), size = 6, alpha = 0.2) +
    geom_point(aes(size = n)) +
    xlim(-1,101) + 
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"))



fdata %>% rowwise() %>% 
    # insert list column of single row of sdata based on conditions
    mutate(s = list(sdata %>% filter(fyear >= byear, fyear < eyear))) %>% 
    # unnest list column
    tidyr::unnest()

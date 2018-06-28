library(tidyverse)
library(ggrepel)


df <- msleep; set_dv <- "sleep_total > 12"; set_ignore <- c('name', 'genus');

df <- mpg; set_dv <- "ntile(cty, 4) > 2"; set_ignore <- "cty";

df <- iris; set_dv <- "Species != 'versicolor'"; set_ignore <- "Species";

df <- mtcars; set_dv <- "vs == 1"; set_ignore <- "vs";


base_data <-
    df %>% 
    mutate_(var = set_dv) %>% 
    select(-one_of(set_ignore)) %>% 
    #filter(complete.cases(.)) %>% 
    mutate_if(is.numeric, funs(ntile(., 10))) %>% 
    mutate(id = row_number())

slope <-
    function(y2, y1, x2, x1){
        (y2-y1)/(x2-x1)
    }

slope(.25,0,-.3,0)

get_values <-
    base_data %>% 
    select(-id) %>% 
    gather(field, value, -var) %>% 
    filter(!is.na(value)) %>% 
    group_by(field, value) %>% 
    #filter(max(row_number())>5) %>% 
    summarise(n = n(),
              sum_value = sum(var)) %>% 
    group_by(field) %>% 
    mutate(rep_field = n/sum(n),
           n_field = sum(n),
           sum_field = sum(sum_value)) %>% 
    ungroup() %>% 
    mutate(p_field = sum_field/n_field,
           p_value = sum_value/n,
           d_value = p_value - p_field,
           abs_d_value = abs(d_value),
           d_rep = d_value * rep_field) %>% 
    group_by(field) %>% 
    mutate(v_field = var(p_value)) %>% 
    ungroup() %>% 
    mutate(d_x_v = d_value * v_field,
           slope_value = abs(slope(v_field, 0, d_value, 0)),
           s_x_d = slope_value * d_value)


compare_values <-
    base_data %>% 
    gather(field, value, -c(id, var)) %>% 
    left_join(get_values) %>% 
    filter(complete.cases(.))



final_estimate <-
    compare_values %>% 
    group_by(var, id) %>% 
    summarise(sum_diff = sum(d_value),
              side_diff = sum_diff > 0,
              sum_sxd = sum(s_x_d),
              side_sum_sxd = sum_sxd > 0,
              #mean_slope = mean(slope_value),
              #side_mean_slope = mean_slope > 0,
              sum_dxv = sum(d_x_v),
              side_dxv = sum_dxv > 0,
              mean_p_value = mean(p_value),
              side_mean = mean_p_value > mean(base_data$var)) %>% 
    ungroup() %>% 
    mutate(sum_sxd_correct = side_sum_sxd == var,
           #mean_slope_correct = side_mean_slope == var,
           diff_correct = side_diff == var,
           dxv_correct = side_dxv == var,
           mean_correct = side_mean == var)


ggplot(final_estimate) + geom_count(aes(var, sum_diff, color = diff_correct))
table(final_estimate$diff_correct)
mean(final_estimate$diff_correct)

ggplot(final_estimate) + geom_count(aes(var, sum_sxd, color = sum_sxd_correct))
table(final_estimate$sum_sxd_correct)
mean(final_estimate$sum_sxd_correct)

ggplot(final_estimate) + geom_count(aes(var, sum_diff, color = diff_correct))
table(final_estimate$dxv_correct)
mean(final_estimate$dxv_correct)

ggplot(final_estimate) + geom_count(aes(var, mean_p_value, color = mean_correct))
table(final_estimate$mean_correct)
mean(final_estimate$mean_correct)


one_obs_est <- final_estimate %>% filter(id == 2)

one_obs_profile <- 
    compare_values %>% filter(id == 2) %>% 
    mutate(est_x = median(d_value),
           est_y = median(v_field))
    #left_join(one_obs_est %>% select(id, ))
    


ggplot(get_values, aes(x = d_value, y = v_field, color = reorder(field, -v_field))) +
    geom_line(aes(group = field), color = "grey60", alpha = .2) +
    geom_segment(aes(xend = 0, yend = 0), alpha = .2) +
    geom_segment(data = one_obs_profile, 
                 aes(xend = est_x, yend = est_y), 
                 color = "black", size = 1) +
    geom_point(data = one_obs_profile, color = "black", shape = 21, size = 6, stroke = 1.5) +
    geom_point(data = one_obs_profile, aes(est_x, est_y),
               color = "black",
               #color = "blue", fill = "white", shape = 21, 
               size = 7) +
    geom_text(data = one_obs_profile, aes(est_x, est_y), label = "?", color = "white", size = 4) +
    geom_point(aes(size = n), alpha = .5) +
    theme(panel.background = element_rect(fill = "white")) +
    guides(size = F) +
    labs(x = "group distance from pop proportion",
         y = "variance of x",
         color = "variable")




filter(get_values, field == "vore") %>% 
    group_by(field) %>% 
    mutate(v_field = var(p_value)) %>% 
    ungroup()

ggplot(compare_values %>% filter(id == 61), 
       aes(d_value, v_field, label = paste(field, "\n",value), color = value)) +
    #geom_vline(aes(xintercept = 0)) +
    geom_label_repel(size = 2) +
    geom_point(aes(color = value)) +
    geom_segment(aes(x = d_value, y = v_field, xend = 0, yend = 0)) + 
    #xlim(0,1) + 
    #ylim(0,1) +
    theme(legend.position = "none")


ggplot(get_values, aes(d_x_v, p_value, label = value, color = value)) +
    facet_wrap(~field) +
    geom_hline(aes(yintercept = p_field)) +
    geom_label_repel(size = 2) +
    #geom_point(aes(size = n, color = value)) +
    theme(legend.position = "none")

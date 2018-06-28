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
           d_rep = d_value * rep_field)

compare_values <-
    base_data %>% 
    gather(field, value, -c(id, var)) %>% 
    left_join(get_values %>% 
              select(field, value, n, rep_field, p_value, d_value, abs_d_value, d_rep)
              ) %>% 
    filter(complete.cases(.))

final_estimate <-
    compare_values %>% 
    group_by(var, id) %>% 
    summarise(sum_diff = sum(d_value),
              sum_diff_side = sum_diff > 0,
              mean_p_value = mean(p_value),
              mean_p_side = mean_p_value > mean(base_data$var)) %>% 
    ungroup() %>% 
    mutate(diff_correct = sum_diff_side == var,
           mean_correct = mean_p_side == var)


ggplot(final_estimate) + geom_count(aes(var, sum_diff, color = diff_correct))
table(final_estimate$diff_correct)
mean(final_estimate$diff_correct)

ggplot(final_estimate) + geom_count(aes(var, mean_p_value, color = mean_correct))
table(final_estimate$mean_correct)
mean(final_estimate$mean_correct)


ggplot(compare_values %>% filter(id == 2), 
       aes(abs_d_value, p_value, label = value, color = value)) +
    geom_hline(aes(yintercept = mean(base_data$var))) +
    #geom_label_repel(size = 2) +
    xlim(0,1) + ylim(0,1) +
    geom_point(aes(color = value)) +
    theme(legend.position = "none")


ggplot(get_values, aes(abs_d_value, p_value, label = value, color = value)) +
    facet_wrap(~field) +
    geom_hline(aes(yintercept = p_field)) +
    geom_label_repel(size = 2) +
    #geom_point(aes(size = n, color = value)) +
    theme(legend.position = "none")
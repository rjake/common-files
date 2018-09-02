library(tidyverse)

initial_df <-
    iris %>%
    mutate(join_val = Petal.Length,
         get_value = "",
         Species = as.character(Species))


df_to_join <- 
    initial_df %>% 
    group_by(guess_species = Species) %>% 
    summarise(mean_val = mean(join_val)) %>% 
    ungroup()

get_join <-
    function(df_left, df_right, formula){
        try(
          expr = {
            # df_left = initial_df
            # df_right = df_to_join
            # i = 110
            # formula = "mean_val < initial_df$join_val"
        
            formula = paste0(formula, "[i]")
        
            temp_df <-
                tibble(ord = seq_len(nrow(df_left)))
        
        
            filter_df <-
                df_right %>% 
                mutate(ord = "") %>% 
                slice(0)
        
            for(i in seq_len(nrow(df_left))) {
                try(filter_df <-
                        rbind(filter_df, 
                        df_right %>% 
                        mutate_(filter_val = str_extract(formula, "\\w+")) %>% 
                        mutate(ord = i) %>% 
                        filter_((formula)) %>% 
                        top_n(1, filter_val) %>% 
                        select(-filter_val))
                    )
            }
        
            return(temp_df %>% left_join(filter_df) %>% select(-ord))
        }, 
      silent = T)
}


get_join(initial_df, df_to_join, "mean_val < initial_df$join_val")

final_df <-
    initial_df %>% 
    cbind(get_join(., df_to_join, "mean_val + 0.1 < initial_df$join_val") %>% select(guess_species))

final_df %>%
    group_by(Species, guess_species) %>% 
    count() %>% 
    spread(guess_species, n)







final_df <-
    initial_df

for(i in seq_len(nrow(initial_df))) {
    try(expr =
            #i = 110
            final_df$get_value[i] <-
                df_to_join %>% 
                filter(mean_val < initial_df$join_val[i]) %>%
                top_n(1, mean_val) %>%
                .$guess_species,
        silent = T)
}

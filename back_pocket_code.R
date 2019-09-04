
library(tidyverse)
library(forcats)

# Data Wrangling
  head_tail <- 
      function(x, n){
        slice(x, c(1:n, (n()-(n-1)):n()))
      }

# snake case column names 
collapse_name <- function(x,
                          preserve_case = FALSE, 
                          sep = "_") {
  x <- make.unique(x)
  sep <- ifelse(sep == ".", "\\.", sep)
  
  if (!preserve_case) {
    x <- tolower(x)
  }
  
  x <- 
    gsub("[^[:alnum:]]+", sep, x) %>% 
    sub(paste0("^", sep, "|", sep, "$"), "", .)
  
  if (sep == "") {
    x <- sub("(^.)", "\\L\\1", x, perl = TRUE)
  }
  
  x
}

test_names <- c(
  " Phone Number",
  "N Times Called",
  "Min Date Called",
  "First Visit",
  "Distance To First Visit (Days)"
)

collapse_name(test_names) # snake_case
collapse_name(test_names, sep = ".") # orig.r.names
collapse_name(test_names, preserve_case = T, sep = "") # CamelCase

# rescale data
    change_range <- 
        function(x, new_min, new_max){
            (x - min(x))/(max(x)-min(x)) * (new_max - new_min) + new_min 
        }
    
    change_range(c(0:10), -10, 10)
    change_range(c(0:10), 0, 255) %>% as.integer()

# round to any increment
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

    round_down(5.343, 0.08)
    round_down(seq(0, 20, 2), 5, T)
    round_down(seq(0, 20, 2), 3, T)
    
# add commas to big #s
    nice_int <- 
        function(x){
          format(x, big.mark = ",", scientific = F)
        }
    
    nice_int(1e6)

# format percents
    nice_pct <- 
        function(fraction, enclose = F, sign = T, d = 0, keep_numeric = F, x100 = T){
            #decimals default to zero unless specified, if convert == F, push it out to 2 places
              decimals <- ifelse((x100 == F & d == 0), 2, d)
            #if number user doesn't want fraction multiplied by 100, then keep orig (*1)
              multiplier <- ifelse(x100 == F, 1, 100)
            #here's rounding result
              x <- round(fraction*multiplier, decimals)
            #add in formatting if  
              if(keep_numeric == F){
                if(sign == T & x100 == T) x <- paste0(x, "%")
                if(enclose == T)          x <- paste0("(", x, ")")
              }
            return(x)
        }
  
    nice_pct(2/7)  
    nice_pct(2/7, enclose = T)
    nice_pct(2/7, keep_numeric = T, d = 4)
    nice_pct(2/7, d = 3, keep_numeric = T, x100 = F)
    nice_pct(2/7, x100 = F)
    nice_pct(2/7, d = 4, x100 = F)

# quickly enclose right, left, or both sides of an object with a symbol  
    enclose <- 
        function(x, symbol = "{_}", side = "b"){
            find_break <- str_locate(symbol, "_")[1]
            start <- str_sub(symbol, 1, find_break-1)
            end <- str_sub(symbol, find_break+1)
            return(paste0(start, x, end))    
        }
    
    enclose("this is a test sentence", "{_}")
    enclose(seq(0,10,2), "_%")
    enclose(seq(0,10,2), "$_ each")

# binary data
    is_binary <- function(x) {
      sort(unique(x)) %>% paste0(collapse = "") == "01"
    }


    replace_01 <- function(x) {
      val <- gsub('0', "No", x) %>% gsub('1', "Yes", .)

      ifelse(is.na(val), "-", val)
    }


    df <-
      tibble(
        not_binary = letters[1:5],
        binary = c(1, 0, 1, NA, 1),
        expected = replace_01(binary)
      )


    df %>% 
      mutate_if(is_binary, replace_01)


# convert comma-separated values to indicators
convert_to_ind <- function(df, field){
    df %>% 
    mutate_(var = field) %>% 
    distinct(id, var) %>% 
    unnest(split = str_split(var, ",")) %>%
    select(-var) %>% 
    filter(!is.na(split)) %>% 
    mutate(n = 1,
           split = 
               str_replace_all(split, "-", ".") %>% 
               paste0(field, "_", ., "_ind")) %>%
    distinct() %>% 
    spread(split, n, fill = 0)
}

# compare data objects of two environments
data.frame(df = unlist(eapply(.GlobalEnv, is.data.frame))) %>% 
  rownames_to_column() %>% 
  filter(df) %>% 
  mutate(nrow = unlist(eapply(.GlobalEnv, nrow)))

# list functions of a package
ls("package:dplyr")

# list packages loaded
devtools::session_info()$packages$package

#### ggplot extras----
# elipses
    ggplot(iris, aes(Petal.Width, Petal.Length, color = Species)) +
        geom_point() +
        stat_ellipse()

# lollipop chart
    ggplot(mpg, aes(fl, displ, color = fl, group = fl)) +
        facet_grid(class~., scales = "free", space = "free") +
        coord_flip() +
        stat_summary(ymin = 0, fun.ymax = max, geom = "linerange", size = 1.5) +
        stat_summary(fun.y = max, geom = "point", size = 3)

# dumbbells
    mpg %>% 
    group_by(manufacturer) %>% 
    filter(row_number() > 5, 
           n_distinct(model) > 2) %>% 
    ggplot(aes(fct_reorder(model, hwy, fun = min), hwy)) +
        facet_grid(manufacturer~., scales = "free", space = "free") +
        coord_flip() +
        #bars
        stat_summary(fun.ymin = min, fun.ymax = max, color = "grey50", geom = "linerange") +
        #interesting ladder effect
        #stat_summary(aes(group = 1), fun.ymin = min, fun.ymax = max, geom = "ribbon", alpha = .1, fill = "blue") +
        #connect mins
        stat_summary(aes(group = 1), fun.y = min, alpha = .5, color = "grey", size = 1.2, geom = "line") +
        #points
        stat_summary(fun.y = max, color = "grey50", geom = "point", size = 1.5) +
        stat_summary(fun.y = min, shape = 21, fill = "orange", color = "black", stroke = 1, geom = "point", size = 1.5) +
        #extra
        expand_limits(y = 0) + 
        xlab("") +
        theme(strip.text.y = element_text(angle = 0), 
              panel.grid = element_blank(), panel.spacing = unit(.05, "lines"))

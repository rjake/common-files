
library(tidyverse)

#Data Wrangling
  head_tail <- 
      function(x, n){
        slice(x, c(1:n, (n()-(n-1)):n()))
      }

#rescale data
    change_range <- 
        function(x, new_min, new_max){
            (x - min(x))/(max(x)-min(x)) * (new_max - new_min) + new_min 
        }
    
    change_range(c(0:10), -10, 10)
    change_range(c(0:10), 0, 255) %>% as.integer()

#round to any increment
    round_down_dec <- 
        function (x, accuracy) {
          x_sign <- sign(x)
          x_int <- abs(as.integer(x))
          x_dec <- abs(x) - x_int
          
          dec_round <- round(x_dec/accuracy) * accuracy
          
          return((x_int * x_sign) + dec_round)
        } 

    round_down_dec(5.343, 0.08)
    round_down_dec(seq(0, 20, 2), 5)
    round_down_dec(seq(0, 20, 2), test_round, 3)
    
#add commas to big #s
    nice_int <- 
        function(x){
          format(x, big.mark = ",", scientific = F)
        }
    
    nice_int(1e6)

#format percents
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

#quickly enclose right, left, or both sides of an object with a symbol  
    enclose <- 
        function(x, symbol = "p", side = "b"){
            symbol <-
                case_when(
                  symbol == "p"       ~ "()",
                  symbol == "dq"      ~ '""',
                  symbol == "sq"      ~ "''",
                  symbol == "b"       ~ "[]",
                  symbol == "curlyb"  ~ "{}"
                )
            start <- ifelse(side == "r", "", str_sub(symbol, 1, 1))
            end <- ifelse(side == "l", "", str_sub(symbol, 2, 2))
            
            return(paste0(start, x, end))    
        }
  
    enclose("this is a test sentence", "curlyb")
    enclose(nice_pct(2/7), "p", "r")

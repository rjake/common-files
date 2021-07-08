library(lubridate)
library(dplyr)


check_nth_day <- function(x, month, weekday, nth = 1) {
  month(x) == month &
  weekdays(x) == weekday &
    (
      (day(x) - 1) %/% 7
    ) + 1 == nth
}


calendar <-
  tibble(
    date =
      seq(
        from = as.Date("2010-01-01"),
        to = ceiling_date(today() %m+% years(3), "year"),
        by = "1 day"
      ),
    month_day = format(date, "%m/%d"),
    # month = month(date),
    weekday = weekdays(date)
  ) %>%
  mutate(
    is_holiday_date = month_day %in% c("01/01", "07/04", "12/25"),
    is_eve_date = month_day %in% c("12/31", "12/24"),
    is_nth_date = (
      check_nth_day(date, month = 1, weekday = "Monday", nth = 3) # mlk
      | check_nth_day(date, month = 9, weekday = "Monday", nth = 1) # labor day
      | check_nth_day(date, month = 11, weekday = "Thursday", nth = 4) # thanksgiving
      | check_nth_day(date, month = 11, weekday = "Friday", nth = 4) # thanksgiving
      | (# memeorial day
        date == ceiling_date(date, "month") %>% floor_date("week", week_start = 1)
        & month(date) == 5
      )
    ),
    is_holiday = (
      is_holiday_date
      | is_eve_date
      | is_nth_date
      | (
        lag(is_holiday_date) == TRUE
        & weekdays(date) == "Monday"
      )
    )
  ) %>%
  filter(!is_holiday) %>%
  # filter(month(date) == 1) %>%
  filter(!weekday %in% c("Saturday", "Sunday")) %>%
  print()


business_days <- function(start, end) {
  map2_dbl(
    .x = as.Date(start),
    .y = as.Date(end),
    .f = ~sum(between(calendar$date, .x, .y))
  )
  #n_days <- sum(between(calendar$date, as.Date(start), as.Date(end))) - 1
}


#
#
# tibble(
#   start = as.Date("2020-12-21"),
#   end = start %m+% days(c(0:14)),
#   start_day = weekdays(start),
#   end_day = weekdays(end)
# ) %>%
#   mutate(n_business_days = business_days(start, end))
#
#
#    start      end        start_day end_day   n_business_days
#    ========== ========== ========= ========= ==========
#    2020-12-21 2020-12-21 Monday    Monday             1
#    2020-12-21 2020-12-22 Monday    Tuesday            2
#    2020-12-21 2020-12-23 Monday    Wednesday          3
# #  2020-12-21 2020-12-24 Monday    Thursday           3  christmas eve
# #  2020-12-21 2020-12-25 Monday    Friday             3  christmas
# #  2020-12-21 2020-12-26 Monday    Saturday           3  weekend
# #  2020-12-21 2020-12-27 Monday    Sunday             3  weekend
#    2020-12-21 2020-12-28 Monday    Monday             4
#    2020-12-21 2020-12-29 Monday    Tuesday            5
#    2020-12-21 2020-12-30 Monday    Wednesday          6
# #  2020-12-21 2020-12-31 Monday    Thursday           6 new years eve
# #  2020-12-21 2021-01-01 Monday    Friday             6 new years
# #  2020-12-21 2021-01-02 Monday    Saturday           6 weekend
# #  2020-12-21 2021-01-03 Monday    Sunday             6 weekend
#    2020-12-21 2021-01-04 Monday    Monday             7
#

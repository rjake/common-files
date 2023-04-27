# workspace ----
library(tidyverse)
library(tidymodels)
library(rlang)

binomial_by_numeric <- function(y, x, data = NULL, return_model = FALSE) {
  res <-
    glm(
      formula = y ~ x,
      data = data,
      family = binomial(link = "logit")
    )

  if (return_model) return(res)
  # else make pretty

  res |>
    broom::tidy(exponentiate = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(
      significant = ifelse(p.value < 0.1, "significant", "not significant"),
      effect = case_when(
        estimate > 1 ~ "increase",
        TRUE ~ "decrease"
      )
    )
}

pivot_then_nest <- function(data, dv, fn) { # faster
  # dv <- as.symbol("attrition")
  data |>
  # whereiation::employee_attrition |>
   select_if(is.numeric) |>
    rename(.dv = {{dv}}) |>
    pivot_longer(-.dv, names_to = "variable") |>
    nest_by(variable) |>
    summarise(
      model = list(
        fn(
          y = data$.dv,
          x = data$value
        )
      ),
      .groups = "keep"
    ) |>
    unnest(cols = model) |>
    ungroup() |>
    select(-term) |>
    arrange(p.value) #|> print(n = Inf)
}

nest_then_pivot <- function(data, dv, fn) { #slower
  data |>
  # whereiation::employee_attrition |>
    select_if(is.numeric) |>
    #select(1:3) |>
    rename(.dv = {{dv}}) %>%
    summarise(
      across(
        .cols = -.dv,
        .fns = ~list( fn(y = .dv, x = .x) )
      )
    ) |>
    pivot_longer(cols = everything(), names_to = "variable") |>
    unnest_wider(value) |>
    select(-term) |>
    arrange(p.value) #|> print(n = Inf)
}



whereiation::employee_attrition |>
  drop_na() |>
  pivot_then_nest(
    dv = attrition,
    fn = binomial_by_numeric
  ) |>
  print(n = Inf)



microbenchmark::microbenchmark(
  pivot_then_nest(whereiation::employee_attrition, dv = attrition, fn = binomial_by_numeric),
  nest_then_pivot(whereiation::employee_attrition, dv = attrition, fn = binomial_by_numeric),
  times = 15
)



# tidymodels ----

library(tidymodels)

linear_reg() |>
  set_engine("lm") |>
  fit(hwy ~ ., data = mpg |> select_if(is.numeric)) |>
  #performance::check_model()
  broom::tidy()


whereiation::employee_attrition |>
  glm(formula = attrition ~ hourly_rate, family = "binomial") |>
  broom::tidy(exponentiate = TRUE)

whereiation::employee_attrition |>
  glm(formula = attrition ~ hourly_rate, family = "binomial") |>
  performance::check_model()



# DALEX ----
# install.packages("DALEX")
# install.packages("mltools")
# install.packages("breakDown")

local({
  df <-
    whereiation::employee_attrition |>
    rename(dv = attrition) |>
    mltools::one_hot()

  model <- ranger::ranger(data = df, dv ~ .)
  DALEX::explain(model, data = df, y = df$dv) |>
    DALEX::model_parts() |>
    plot()
})

local({
  df <-
    whereiation::employee_attrition |>
    rename(dv = attrition) |>
    combined_data |>
    rename(dv = attrition) |>
    drop_na() |>
    mltools::one_hot()

  model <- ranger::ranger(data = df, formula = dv ~ .)
  explainer <- DALEX::explain(model, data = df, y = df$dv)

  explainer |>
    DALEX::model_parts(type = "difference") |> # variable_importance  difference ratio
    plot()

  explainer |>
    DALEX::feature_importance() |>
    plot()

  explainer |>
    DALEX::model_parts() |>
    plot()


  explainer |>
    DALEX::model_profile(variables = "cyl") |>
    plot()

  explainer |>
    DALEX::model_profile(variables = "manufacturer") |>
    plot()

  explainer |>
    DALEX::model_profile(variables = "displ") |>
    plot()

  DALEX::model_performance(explainer)
  DALEX::model_performance(explainer) |> plot()
})



local({
  df <-
    mpg |>
    rename(dv = cty) |>
    mltools::one_hot()

  model <- ranger::ranger(data = df, dv ~ .)
  explainer <- DALEX::explain(model, data = df, y = df$dv)

  DALEX::explain(model, data = df, y = df$dv) |>
    DALEX::model_parts() |>
    plot()

  DALEX::model_profile(explainer, variables = "cyl") |> plot()
  DALEX::model_profile(explainer, variables = "hwy") |> plot()
})



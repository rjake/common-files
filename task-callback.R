"
These examples show how to do something each time a expression is evaluated
some key words to find: task expression callback handler
"

# add beep + popup to every call
addTaskCallback(
  name = "beep",
  function(...) {
    beepr::beep(5)
    system("msg * done")
    TRUE
  }
)

# test by running these
123
456

# remove by finding callback name id
removeTaskCallback( which(getTaskCallbackNames() == "beep") )



# A more complicated example that looks at instances where paste() is used
.check_fn <- "paste"
.if_found <- expr(print("used!!!"))

.check_if_used <- function(expr, fun, do = NULL) {
  expr_names <-
    all.names(expr) |>
    gsub(pattern = "^[^:]+:", replacement = "")

  if (any(expr_names == fun)) {
    do
  }
}

# test
expression( paste(123) ) |> 
  .check_if_used(.check_fn, eval(.if_found))
# >  "used!!!"

# callback function
addTaskCallback(
   name = "detect-function",
   function(...) {
     expr <- as.expression(...)
     .check_if_used(
       expr = expr, 
       fun = .check_fn,
       do = eval(.if_found)
     )
     TRUE
   }
 )

# test
# not found
"paste"
#> "paste"

# is found
paste(123)
#> "123"
#> "Used !!!"


# remove with lapply if used multiple times
which(getTaskCallbackNames() == "detect-function") |> 
  lapply(\(x) removeTaskCallback(x)) |> 
  invisible()

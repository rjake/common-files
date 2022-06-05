# creating a reprex to troubleshoot devtools::check() or pkgdown

# simple ------------------------------------------------------------
# create tmp package
tmp <- tempdir()
pkg <- file.path(tmp, "test")
devtools::create(path = pkg)

# add R code
writeLines(
  con = file.path(pkg, "R/test.R"),
  text = 
    "#' does some stuff
    #' @inheritDotParams glue::glue_data
    #' @
    foo <- function(...) glue_data(...)"
)

# document is ok
devtools::document(pkg) 

# throws warning
devtools::check(pkg)
# warning: prepare_Rd: ./man/foo.Rd:20: unknown macro '\item'

# also throws warning
pkgdown::build_reference(pkg)
# In tools::parse_Rd(path, macros = macros, encoding = "UTF-8") :
#   C:/Users/jake/AppData/Local/Temp/RtmpYxG5C2/test/man/foo.Rd:20: unknown macro '\item'

# remove temp directory
unlink(tmp, recursive = TRUE)





# original ------------------------------------------------------

# create dummy package
tmp <- tempdir() 
setwd(tmp)

devtools::create(path = "test")
setwd("test")
usethis::use_mit_license()

# add R code
writeLines(
  con = "R/test.R",
  text = 
"#' does some stuff
#' @param list for list methods
#' @param x arg1
#' @param y arg2
#' @param ... some argument
#' @export
foo <- function(...) UseMethod('foo')

#' @export
#' @inheritParams foo
#' @method foo default
#' @S3method foo default 
foo.default <- function(x, y) paste(x, y)


#' does stuff for lists
#' @param list for list methods
#' @inheritParams foo
#' @export
#' @method foo list
foo.list <- function(list, x, y) foo(list[[x]], list[[y]])"
)

devtools::document()
devtools::install()
library(test)

devtools::load_all()

# examples
foo(1, 2)
list(a = 1, b = 2) |> foo("a", "b")

devtools::check(document = FALSE)

# W  checking S3 generic/method consistency (561ms)
# foo:
#   function(list, x, y, ...)
#     foo.default:
#   function(x, y, ...)
#     
#     See section 'Generic functions and methods' in the 'Writing R
#    Extensions' manual.


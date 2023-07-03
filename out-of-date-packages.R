library(tidyverse)

compare_version <- function(x, y) {
  # x <- "1.0.6"; y <- "1.0.7"
  x <- str_replace_all(x, "-", ".")
  y <- str_replace_all(y, "-", ".")
  if (str_count(x, "\\.") == 1) x <- paste0(x, ".0")
  if (str_count(x, "\\.") == 2) x <- paste0(x, ".0")
  if (str_count(y, "\\.") == 1) y <- paste0(y, ".0")
  if (str_count(y, "\\.") == 2) y <- paste0(y, ".0")
  
  
  x_parts <- str_split(x, "\\.")[[1]] |> as.integer()
  y_parts <- str_split(y, "\\.")[[1]] |> as.integer()
  
  major <- x_parts[1] - y_parts[1]
  minor <- ifelse(major > 0, 0, x_parts[2] - y_parts[2])
  revision <- ifelse(major + minor > 0, 0, x_parts[3] - y_parts[3])
  build <- ifelse(major + minor + revision > 0, 0, x_parts[4] - y_parts[4])
  
  paste(
    str_pad(major, 4, "left", "_"),
    str_pad(minor, 4, "left", "_"),
    str_pad(revision, 4, "left", "_"),
    str_pad(build, 4, "left", "_")
  ) |> 
    str_replace_all("_0\\b", "__") |> 
    str_replace_all("NA", "____")
}


out_of_date <- 
  old.packages() |> 
  as_tibble() |> 
  rename_all(tolower) |> 
  select(package, installed, avail = reposver) |> 
  print(n = Inf)


out_of_date |> 
  mutate(
    diff = map2_chr(avail, installed, compare_version)
  ) |>
  arrange(desc(diff), package) |> 
  # major minor revision build
  rename(`___M____m____r____b` = diff) |> 
  print(n = Inf) |> 
  suppressWarnings()


# Add version separator
version_separator <- function(x) {
  major = str_extract(x, "^\\d+") |> str_pad(2, "left", "0")

  if (is.na(major)) {
    return(NA)
  }

  minor = str_extract(x, "(?<=\\.)\\d+(?=\\.)") |> str_pad(2, "left", "0")
  fix = str_extract(x, "\\d+$") |> str_pad(2, "left", "0")
  paste(major, minor, fix)
}

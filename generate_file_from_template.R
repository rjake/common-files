generate_file_from_template <- function(name, type, pkg) {
  file <- ifelse(grepl("\\.[Rr]md$", name), name, paste0(name, ".Rmd"))

  try( # throws a weird error from slash in css in yaml
    rmarkdown::draft(file, template = type, package = pkg)
  )

  file.edit(file)
}

new_markdown <- function(name) {
  generate_file_from_template(name, "github_document", "rmarkdown")
}
 
# fails :(
new_dashboard <- function(name) {
  generate_file_from_template(name, "flex_dashboard", "flexdashboard")
}

new_markdown("test_md") 
new_dashboard("test_dashboard")

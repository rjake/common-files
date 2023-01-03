# user inputs -------------------------
pkg <- "glue"

exclude_symbols <- c(
  ".__global__", ".__NAMESPACE__.", ".__S3MethodsTable__.",
  ".data",  ".packageName", "%>%"
)

# code runs from here -----------------


# workspace -----
library(tidyverse)
library(glue)
library(networkD3)
library(shiny)
library(shinyWidgets)
library(htmlwidgets)
shiny::reactiveConsole(enabled = TRUE)


# extract code -----
# * fns ----
fns <- local({
  ns <-
    getNamespace(pkg) |>
    names() |>
    sort()

  exported <- getNamespaceExports(pkg) |> sort()
  internal <- setdiff(ns, exported)
  imported <- setdiff(exported, ns)

  list(
    exported = exported,
    imported = imported,
    internal = internal,
    all =
      c(exported, imported, internal) |>
      unique() |>
      sort()
  )
})

#' Find symbols
#' Searches for functions but may also find object names, argument names, etc
#' Regex is less robust than parsing but really anything can be a symbol
#' Doesn't catch places where fns are quoted, like rlang::call2("fn", a = 1)
#' @examples
#' find_symbols(fn = "runGadget", pkg = "shiny")                  # internal to shiny
#' find_symbols(fn = "runGadget", pkg = "shiny", fn_names = NULL) # all

find_symbols <- function(fn, pkg = pkg, fn_names = names(getNamespace(pkg))) {

  fn_call <- glue("{pkg}{sep}`{fn}`", sep = ifelse(fn %in% fns$exported, "::", ":::"))
  expr <- eval(parse(text = fn_call))

  if (fn %in% fns$imported) {
    return(
      paste("imported:", environment(expr) |> environmentName())
    )
  }

  args <- names(formals(expr))
  expr_body <- body(expr)[-1] |> as.character()

  objects_made <-
    expr_body |>
    str_extract_all("\\b[\\w\\.]+ *(?=<-)") |>
    flatten_chr() |>
    unique() |>
    trimws()

  # find all symbols
  unique_symbols <-
    expr_body |>
    str_replace_all(paste0(pkg, "::(:)?"), " ") |> # internal objects
    str_replace_all("\\b[\\w\\.]+ *<- *", " ") |> # inner assignments
    str_replace_all("\\\n", " ") |> # new lines
    str_replace_all("\\.{3}", " ") |> # ellipses
    str_remove_all('"[^"]+"') |> # anything between double quotes
    str_remove_all("'[^']+'") |> # anything between single quotes
    str_replace_all("\\breturn\\(\\S+\\)", " ") |>  # 'return(obj)'
    str_replace_all("\\breturn\\(", " ") |>  # remaining 'return('
    str_replace_all("[\\(\\),~\\!]", " ") |> # remaining (),~!
    str_replace_all("(?<=[a-zA-Z])\\[", " ") |> # remove [ from subset
    str_replace_all("\\S*\\$\\S+", " ") |> # data$column or fn()$attr
    str_replace_all("\\d+", " ") |> # digits
    str_split(" ") |> # separate into words
    flatten_chr() |>
    unique() |>
    sort() |>
    discard(~nchar(.x) == 0) |>  # drop words that have no characters
    discard(str_detect, "^(T(RUE)?|F(ALSE)?|NA|NULL|%in%)$") |> # drop T TRUE F FALSE NA NULL %in%
    discard(str_detect, "^([^[:alpha:]])$") # single character punctuation, [:[punct]:] doesn't work

  if (is.null(fn_names)) {
    unique_symbols |>
      discard(~(.x %in% c(args, objects_made)))

  } else {
    unique_symbols |>
      keep(~(.x %in% fn_names))
  }
}

# * fn_df ----
fn_df <-
  tibble(
    fn = fns$all,
    used = map(
      .x = fn,
      .f = possibly(
        ~find_symbols(fn = .x, pkg = pkg, fn_names = fns$all),
        NA
      )
    )
  ) |>
  unnest(used, keep_empty = TRUE) |>
  filter(
    !fn %in% exclude_symbols,
    !used %in% exclude_symbols
  ) |>
  mutate(
    n_used = map(fn, ~sum(.x %in% used)),
    used = ifelse(
      is.na(used) & n_used == 0,
      "(unknown)",
      used
    )
  )

# function to create tables for network
prep_for_network <- function(from, to, link_size = 3) {
  # from <- fn_df$used; to <- fn_df$fn; link_size = 3
  node_levels <-
    c(from, to) |>
    na.omit() |>
    unique() |>
    sort() |>
    factor()

  nodes <-
    tibble(
      name = node_levels,
      factor_id = as.integer(node_levels) - 1,
      group = node_levels %in% fns$exported
    ) |>
    arrange(name) |>
    as.data.frame()

  links <-
    tibble(
      source_name = from,
      target_name = to
    ) |>
    drop_na(source_name) |>
    mutate(
      source_name = factor(source_name, levels = levels(node_levels)),
      target_name = factor(target_name, levels = levels(node_levels)),
      source = as.integer(source_name) - 1,
      target = as.integer(target_name) - 1,
      n = link_size
    ) |>
    arrange(source_name) |>
    as.data.frame()

  list(
    nodes = nodes,
    links = links
  )
}

# see preview
if (interactive()) {
  forceNetwork(
    Links = prep_for_network(fn_df$used, fn_df$fn)$links,
    Nodes = prep_for_network(fn_df$used, fn_df$fn)$nodes,
    Source = "source",
    Target = "target",
    Value = "n",
    NodeID = "name",
    Group = "group",
    arrows = TRUE,
    fontSize = 14,
    colourScale = JS('d3.scaleOrdinal(["grey", "dodgerblue"])'),
    opacityNoHover = 1,
    linkDistance = 300,
    charge = -400,
    zoom = TRUE
  )
}


# shiny app -----

if (interactive()) { # allows you to debug
  input <- list(
    action = "Filter",
    build = TRUE,
    export = "Exported only",
    fn_names = tail(fns$exported, 1),
    node_distance = 3
  )
}

base_fns <- c(fn_df$fn, fn_df$used) |> unique() |> sort()

# _ui ----
ui <- {
  fluidPage(
    titlePanel(glue("Hello {pkg}!")),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        width = 3,
        selectInput( # $ fn_names ----
          inputId = "fn_names",
          label = "Focus on these functions",
          choices = base_fns,
          selected = tail(fns$exported, 3),
          multiple = TRUE
        ),
        radioGroupButtons( # $ action ----
          inputId = "action",
          label = NULL,
          choices = c("Filter", "Highlight")
        ),
        radioGroupButtons( # $ exported ----
          inputId = "exported",
          label = NULL,
          choices = c("Exported only", "All functions")
        ),
        actionBttn( # $ build ----
          inputId = "build",
          label = "build",
          icon = icon("project-diagram"),
          color = "primary",
          style = "simple"
        ),
        br(), br(), hr(), br(),
        sliderInput( # $ node_distance ----
          inputId = "node_distance",
          label = "Node Distance",
          min = 1, max = 6, value = 3,
          step = 1,
          ticks = FALSE
        ),
        br(),
        br(),
        br(),
        p(
          paste("package version", packageVersion(pkg))
        )
      ),
      mainPanel = mainPanel(
        forceNetworkOutput("network", width = "100%", height = "800px")
      )
    )
  )
}

# _server ----
server <- function(input, output, session) {
  # update inputs ----
  observeEvent(input$exported, {
    if (input$exported == "Exported only") {
      use_fns <- fns$exported
    } else {
      use_fns <- base_fns
    }

    updateSelectInput(
      inputId = "fn_names",
      choices = use_fns,
      selected = intersect(input$fn_names, use_fns)
    )
  })


  # create network chart
  network_df <- eventReactive(input$build, ignoreNULL = FALSE, {
    df <- fn_df

    if (!is.null(input$fn_names) && input$action == "Filter") {
      df <- filter(df, used %in% input$fn_names | fn %in% input$fn_names)
    }

    if (input$exported == "Exported only") {
      df <- filter(df, fn %in% fns$exported)
    }

    res <-
      prep_for_network(
        from = df$used,
        to = df$fn,
        link_size = 3
      )

    res$nodes <-
      res$nodes |>
      mutate(
        color = case_when(
          name %in% input$fn_names ~ "orangered",
          group == TRUE ~ "dodgerblue",
          TRUE ~ "grey"
        )
      )

    res
  })

  # [network] ----
  output$network <- renderForceNetwork({
    df <- network_df()
    color_palette <-
      glue(
        'd3.scaleOrdinal([{colors}])',
        colors =
          glue("'{unique(df$nodes$color)}'") |>
          glue_collapse(", ")
      )

    forceNetwork(
      Links = df$links,
      Nodes = df$nodes,
      Source = "source",
      Target = "target",
      Value = "n",
      NodeID = "name",
      Group = "color",
      arrows = TRUE,
      fontSize = 14,
      colourScale = JS(color_palette),
      opacityNoHover = 1,
      #linkDistance = JS("function(d){return d.value * 25}"),
      #linkDistance = 100,
      #bounded = TRUE,
      charge = -c(10, 50, 100, 200, 400, 500)[input$node_distance],
      zoom = TRUE
    ) |>
      htmlwidgets::onRender(
        "function(el,x) { d3.selectAll('.node').on('mouseleave', null); }"
      )
  })
}


if ("input" %in% ls()) rm(input)

shinyApp(
  ui = ui,
  server = server,
  options = list(launch.browser = TRUE)
)

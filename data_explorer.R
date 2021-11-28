#REFERENCE
#https://stackoverflow.com/questions/46463281/how-do-i-connect-fileinput-to-ggplot-in-shiny
#https://stackoverflow.com/questions/28176944/create-dynamic-ggvis-chart-from-uploaded-file-in-shiny
#https://rstudio.github.io/shinydashboard/structure.html#sidebar


#TO DO
#Change select items to selectize:https://shiny.rstudio.com/gallery/selectize-examples.html
#selectizeInput('e3', '3. Item creation', choices = state.name,options = list(create = TRUE)),
#Adjust layout, see this example
#https://shiny.rstudio.com/gallery/selectize-vs-select.html


library(tidyverse)
library(lubridate)
#library(odbc)
library(shiny)
library(shinydashboard)

date_guess <-
  function(x){
    parse_date_time(x,
                    orders = c("ymd_HMS", "mdy_HM","ymd", "mdy"))#,origin = "1970-01-01")
  }

ui <- {
  dashboardPage(
    dashboardHeader(title = "Viz Exlporer"),
    dashboardSidebar(width = "650px",
                     fluidRow(column(6, fileInput("file1", label = "Upload Data:", accept = c(".csv", ".sql"))),
                              column(6, selectInput('chart_type', 'Chart Type',
                                                    choices = c("geom_count", "geom_point", "geom_bar", "geom_line")))
                     ),
                     fluidRow(
                       column(6,
                              selectInput('x', 'X', 'x'),
                              selectInput('y', 'Y', 'y'),
                              selectInput('color', 'Color', 'color', selectize = T),
                              selectInput('fill', 'Fill', 'fill'),
                              selectInput('facet_row', 'Facet Row', 'facet_row'),
                              selectInput('facet_col', 'Facet Column', 'facet_row')
                       ),
                       column(6,
                              #textInput('text_df', 'Custom subset','coming soon...'),
                       textInput('text_x', 'Custom X'),
                       textInput('text_y', 'Custom Y'),
                       #textInput('to_do', 'Chart Type',  'coming soon...'),
                     textInput('text_c', 'Custom Color'),
                     textInput('text_f', 'Custom Fill'),
                     textInput('to_do', 'Custom Facet Row',
                               'coming soon...'),
                     textInput('to_do', 'Custom Facet
                               Column', 'coming soon...')
                     )
    ),
  fluidRow(
    column(6,
           checkboxInput('show_x_axis', 'Hide Labels - X'),
           checkboxInput('show_y_axis', 'Hide Labels - Y'),
           checkboxInput('coord', 'Coord Flip')
    ),
    column(6,
           checkboxInput('smooth', 'Smooth'),
           checkboxInput('jitter',
                         'Jitter')#,checkboxInput('to_do', 'Show Code (coming soon)')
    )
  )
  ),
dashboardBody(
  box(plotOutput("plot"), width = "100%", height = "100%",
      textOutput("text_path"),
      htmlOutput("gg_call")
  )
)
)
}

server <-
  function(input, output, session) {
    read_data <-
      reactive({
        infile <- input$file1
        if(is.null(infile)) {
          df <- mpg
        } else if(grepl(".csv", input$file1)){
          df <-
            read.csv(infile$datapath, stringsAsFactors = F, na.strings
                     = c('NULL', 'NA')) %>%
            mutate_at(vars(matches("DATE|DT|_ROOM")),
                      funs(date_guess))
        } else if(grepl(".sql", input$file1)){
          cdwprd <- dbConnect(drv = odbc(), dsn = "CDWPRD",
                              believeNRows = F,
                              bigint = "integer")
          df <- dbGetQuery(cdwprd, read_file(infile$datapath))
        }
        df
      })
    
    observe({
      data_selected <- read_data()
      
      updateSelectInput(session, 'x', choices = c(names(data_selected), Choose="."))
      updateSelectInput(session, 'y', choices = c(names(data_selected), Choose="."))
      updateSelectInput(session, 'color', choices = c(Choose=".", names(data_selected)))
      updateSelectInput(session, 'fill', choices = c(Choose=".", names(data_selected)))
      updateSelectInput(session, 'facet_row', choices = c(Choose=".", names(data_selected)))
      updateSelectInput(session, 'facet_col', choices = c(Choose=".", names(data_selected)))
      
      output$plot <-
        renderPlot({
          # if(nchar(input$text_df) > 0){
          #     get_df <- input$text_df, input$x)
          # } else {
          #   get_df <-
          # }
          get_x <- ifelse(nchar(input$text_x) > 0, input$text_x, input$x)
          get_y <- ifelse(nchar(input$text_y) > 0, input$text_y, input$y)
          get_c <- ifelse(nchar(input$text_c) > 0, input$text_c, input$color)
          get_f <- ifelse(nchar(input$text_f) > 0, input$text_f, input$fill)
          
          get_geom <- ifelse(get_y != '.' && input$chart_type ==
                               'geom_bar', 'geom_col', input$chart_type)
          #get_stat <- ifelse(get_y != '.' & input$chart_type %in% c('geom_bar', 'geom_line'), 'mapping = aes(), stat = "identity"', 'aes()')
      
      p <-
        ggplot(data_selected) +
        get(get_geom)()
      
      if (input$x != '.')         p <- p + aes_string(x = eval(get_x))
      
      if (input$y != '.')         p <- p + aes_string(y = eval(get_y))
      if (input$color != '.')     p <- p + aes_string(color = eval(get_c))
      if (input$fill != '.')  p <- p + aes_string(fill = eval(get_f))
      
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .')  p <- p + facet_grid(facets, scales = "free", space = "free")
      
      if (input$jitter)       p <- p + geom_jitter()
      if (input$smooth)       p <- p + geom_smooth()
      if (input$coord)        p <- p + coord_flip()
      
      if (get_geom == 'geom_col') p <- p + labs(y = paste0('sum of ', get_y))
      
      if(input$show_x_axis){  show_x_text <- element_blank()
      show_x_tick <- element_blank()
      } else {  show_x_text <- element_text()
      show_x_tick <- element_line()}
      
      if(input$show_y_axis){  show_y_text <- element_blank()
      show_y_tick <- element_blank()
      } else {  show_y_text <- element_text()
      show_y_tick <- element_line()}
      p <-
        p +
        theme(axis.ticks.x = show_x_tick,
              axis.ticks.y = show_y_tick,
              axis.text.x = show_x_text,
              axis.text.y = show_y_text)
      
      print(p)
        })
    output$gg_call <-
      renderText({ #isTRUE(all.equal(data_selected, mpg))})
        facets <- paste(input$facet_row, '~', input$facet_col)
        get_x <- ifelse(nchar(input$text_x) > 0, input$text_x, input$x)
        get_y <- ifelse(nchar(input$text_y) > 0, input$text_y, input$y)
        get_c <- ifelse(nchar(input$text_c) > 0, input$text_c, input$color)
        get_f <- ifelse(nchar(input$text_f) > 0, input$text_f, input$fill)
        get_geom <- ifelse(get_y != '.' && input$chart_type == 'geom_bar', 'geom_col', input$chart_type)
        
        HTML( gsub("(, ", "(",
                   paste0('ggplot(df',
                          #
                          ifelse(isTRUE(all.equal(data_selected, mpg)), 'mpg', 'df'),
                          ') +<br/>',
                          get_geom, '(',
                          'aes(',
                          paste0(ifelse(input$x != '.', paste0('x =', get_x), ''),
                                 ifelse(input$y != '.', paste0(', y = ', get_y), ''),
                                 ifelse(input$color != '.', paste0(', color = ', get_c), ''),
                                 ifelse(input$fill != '.', paste0(', fill = ', get_f), '')),
                          '))',
                          ifelse(input$jitter, '+\ngeom_jitter()', ''),
                          ifelse(facets != '. ~ .', paste0('+<br/>facet_grid(', facets, ', scales = "free", space =
                                        "free")'), ''),
                          ifelse(input$smooth, '+<br/>geom_smooth()', ''),
                          ifelse(input$coord, '+<br/>coord_flip()', ''),
                          ifelse(get_geom == 'geom_col',
                                 paste0('+<br/>labs(y = "sum of ', get_y, '")'), ''),
                          ifelse(input$show_x_axis | input$show_y_axis,
                                 paste0('+<br/>theme(',
                                        ifelse(input$show_x_axis, 'axis.ticks.x = element_blank(), axis.text.x = element_blank()', ''),
                                        ifelse(input$show_y_axis, ', axis.ticks.y = element_blank(), axis.text.y = element_blank()', ''),
                                        ')'), '')
                          )
                   , fixed = T)
                                 )
      })
    })
}

shinyApp(ui, server)
#runApp(list(ui = ui, server = server), launch.browser = F)

#####

#shinyApp(ui, server)
# a <- 'geom_point'
#
# b <- parse(text = "factor(Sepal.Length)")
#
# ggplot(iris, aes(eval(b), Sepal.Width)) +
#   get(a)() +
#   abs(x = b)

# conditionalPanel(
#       condition = "input.model == 'LOESS'",
#       sliderInput("span", label = "Span for LOESS", min = 0, max =1, value = .75)
# )
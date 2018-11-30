# https://rpubs.com/hadley/gh

library(gh) # devtools::install_github("gaborcsardi/gh")
library(purrr)
library(tidyverse)
library(lubridate)
library(highcharter)

# example repo
one_repo <-
    gh("/repos/rjake/CommonFiles/commits", 
       type = "public",
       .limit = Inf
    )[1]

one_file <-
    gh("/repos/rjake/CommonFiles/commits/64794e3d5b0f22f559cbec45327bf40d7d47d352", 
       type = "public",
       .limit = Inf
    )$files[[1]]$filename

# from hadley's example
all_repos <- 
    gh("/users/rjake/repos",
       type = "public",
       .limit = Inf)

my_repos <- # can find via [view source]: data-scope-id="156602851"
    #"156602851" #count_to_1_million
    map_chr(all_repos, "id")

full_name <-
    tibble(name = map_chr(all_repos, "full_name"),
           id = map_chr(all_repos, "id")) %>% 
    filter(id %in% my_repos)

get_commits <- function(full_name, since = "2016-01-01") {
    message("Requesting commits for ", full_name)
    #https://api.github.com/repos/rjake/Biking_at_Penn/commits
    
    commits <- 
        gh("GET /repos/:full_name/commits", 
           full_name = full_name, 
           since = since,
           .limit = Inf
        )
    
    if (length(commits) == 0) {
        return(NULL)
    }
    
    tibble(
        repo_name = full_name,
        url = map_chr(commits, "html_url", .null = NA_character_),
        #author_name = map_chr(commits, c("commit", "author", "name"), .null = NA_character_),
        author_login = 
            map_chr(commits, c("commit", "author", "email"), .null = NA_character_) %>% 
            str_replace("@.*", "") %>% 
            tolower(.),
        #commit_name = map_chr(commits, c("commit", "committer", "name"), .null = NA_character_),
        commit_time = 
            map_chr(commits, c("commit", "author", "date"), .null = NA) %>% 
            ymd_hms(),
        sha = map_chr(commits, "sha", .null = NA_character_),
        msg = map_chr(commits, c("commit", "message"), .null = NA_character_)
    )
}

get_files <- function(sha, since = "2016-01-01") {
    message("Requesting commits for ", sha)
    #https://github.research.chop.edu/api/v3/repos/CQI/ortho-radiology/commits
    tryCatch({
        file <- 
            gh(paste0("GET ", sha), 
               .api_url = "https://github.research.chop.edu/api/v3",
               type = "public",
               .limit = Inf
            )$files[[1]]$filename
        
        if (length(file) == 0) {
            return(NULL)
        }
        
        tibble(sha = str_extract(sha, "(?<=/)[^/]+$"),
               file = file)
    }, 
    error = function(e) data.frame()
    )
}

my_commits <- 
    map(full_name$name, get_commits) %>% 
    compact() %>% 
    bind_rows()

clean_commits <-
    my_commits %>% 
    #filter(author_login == "rjake") %>% 
    mutate(commit_time = with_tz(commit_time, tzone = "America/New_York"),
           hour = hour(commit_time),
           commit_date = as.Date(commit_time),
           repo_name = str_replace(repo_name, "rjake/", ""),
           year = year(commit_date),
           month = month(commit_date),
           day = day(commit_date))

clean_commits

all_sha <- str_replace(clean_commits$url, ".*\\.com(.*)", "/repos\\1")
    #paste0("/repos/rjake/", clean_commits$repo_name, "/commits/", clean_commits$sha)

my_files <-
    map(all_sha, get_files) %>% 
    compact() %>% 
    bind_rows()

final_commits <-
    clean_commits %>% 
    left_join(my_files) %>% 
    mutate(nice_file = str_replace(file, ".*/(.*)", "\\1"))


ggplot(final_commits) +
    geom_histogram(aes(commit_date))

heat_map <-
    final_commits %>% 
    filter(!str_detect(msg, "^Merge")) %>% 
    group_by(repo_name = tolower(repo_name), 
             commit_date = floor_date(commit_date) %>% as.Date(),
             year, month, day) %>% 
    summarise(n = n(),
              commits = paste(msg, collapse = '<br>')) %>%
    ungroup() %>% 
    mutate(repo_name = fct_reorder(repo_name, commit_date),
           color = as.numeric(repo_name) %% 4,
           high_level = as.numeric(repo_name) - 1)


ggplot(heat_map) +
    geom_line(aes(commit_date, repo_name, color = factor(color)), 
              size = 1.5, alpha = 0.4, show.legend = F) +
    geom_point(aes(commit_date, repo_name, size = n, color = factor(color)), 
               alpha = 0.7,
               show.legend = F) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_discrete(position = "right") +
    #scale_color_manual(values = c("red", "blue", "black")) +
    scale_size(range = c(2,10)) +
    labs(x = "Commit Date",
         y = "") +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "grey90")
    )

ggplot(heat_map) +
    geom_tile(aes(commit_date, repo_name, fill = n), alpha = 0.7) 

y_cat <- heat_map %>% pull(repo_name) %>% levels()

highchart() %>%
    hc_chart(zoomType = "xy") %>% 
    hc_add_series(heat_map, type = "line",
                  hcaes(x = commit_date, y = high_level,
                        group = repo_name, color = repo_name),
                  showInLegend = F, pointWidth = 0.2) %>% 
    hc_add_series(heat_map, "point", 
                  hcaes(x = commit_date, y = high_level,
                        group = repo_name, size = n, name = commits),
                  dataLabels = list(enabled = FALSE,
                                    y = -20,
                                    verticalAlign = "top",
                                    format = '<span style="font-weight:normal;">{point.commits}</span>'),
                  minSize = "1%", maxSize = "5%", showInLegend = F, 
                  #color = "darkblue", 
                  marker = list(fillOpacity = 0.8)) %>% 
    hc_yAxis(categories = y_cat) %>%
    hc_xAxis(type = "datetime",
             title = list(text = "Commit Date")) %>% 
    hc_chart(borderColor = "lightgrey",
             borderWidth = 1) %>% 
    hc_navigator(enabled = TRUE) %>% 
    hc_tooltip(headerFormat = "",
               pointFormat = 
                   '<span style="font-weight:bold;color:darkblue;">{point.repo_name} </span><br>
               {point.commit_date} | Commits: <span style="font-weight:bold;color:darkblue;">{point.n} </span><br>
               -------------------<br>
               {point.commits}') 

recent_commits <-
    final_commits %>% 
    filter(!str_detect(msg, "^Merge")) %>% 
    filter(commit_date > today()-21) %>% 
    select(-sha)

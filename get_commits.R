# https://rpubs.com/hadley/gh

library(gh) # devtools::install_github("gaborcsardi/gh")
library(purrr)
library(tidyverse)
library(lubridate)

# example repo
one_repo <-
    gh("/repos/rjake/CommonFiles/commits", 
       type = "public",
       .limit = Inf
    )[1]

# from hadley's example
all_repos <- 
    gh("/users/rjake/repos",
       type = "public",
       .limit = Inf)

my_repos <- # can find via [view source]: data-scope-id="156602851"
    c("156602851" #count_to_1_million
    )

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

map_chr(a, c("commit", "message"))[1]

my_commits <- 
    map(full_name$name, get_commits) %>% 
    compact() %>% 
    bind_rows()

final_commits <-
    my_commits %>% 
    #filter(author_login == "rjake") %>% 
    mutate(commit_date = as.Date(commit_time),
           repo_name = str_replace(repo_name, "CQI/", ""),
           year = year(commit_date),
           month = month(commit_date),
           day = day(commit_date))

final_commits

ggplot(final_commits) +
    geom_histogram(aes(commit_date))

heat_map <-
    final_commits %>% 
    group_by(repo_name = tolower(repo_name), 
             commit_date = floor_date(commit_date) %>% as.Date(),
             year, month, day) %>% 
    summarise(n = n()) %>%
    ungroup() %>% 
    mutate(repo_name = fct_reorder(repo_name, commit_date))
    

ggplot(heat_map) +
    geom_point(aes(commit_date, 
                   repo_name, 
                   size = n, 
                   color = repo_name), 
               alpha = 0.7,
               show.legend = F) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_size(range = c(2,10)) +
    labs(x = "Commit Date",
         y = "") +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "grey90"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "grey90")
          )


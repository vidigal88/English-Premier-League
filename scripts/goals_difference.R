
# References
# https://www.r-bloggers.com/2019/08/ggtext-for-images-as-x-axis-labels/
# https://stackoverflow.com/questions/67491715/adding-images-below-x-axis-labels-in-ggplot2
# https://wilkelab.org/ggtext/

# Load libraries ----

library(worldfootballR)
library(tidyverse)
library(cowplot)
library(ggtext)
library(magick)
library(ggtext)

# Pull English Premier League matches from 2009/10 to 2021/22 ----

premier_league <- get_match_results(country = "ENG", gender = "M", season_end_year = c(2010:2022), tier = "1st")
glimpse(premier_league)

# Create dir and save data ----
fs::dir_create("data/")
write_rds(premier_league, file = "data/premier_league.rds")

# Read in Premier League Data Matched From 2009/10 to 2021/22 ----

premier_league <- read_rds("data/premier_league.rds")
glimpse(premier_league)

# Number rows by season ----

premier_league %>% 
    group_by(Season_End_Year) %>% 
    summarise( n = n()) %>% 
    ungroup()

# Select vars ----

premier_league_tbl <- premier_league %>% 
    select(Season_End_Year, Home, Away, HomeGoals, AwayGoals) %>% 
    set_names(names(.) %>% str_to_lower())

# Total Number of Goals Scored and Conceded by Season and Club ----

# Goals home ----

premier_league_home_tbl <- premier_league_tbl %>% 
    group_by(season_end_year, home) %>% 
    summarise(goals_scored_home = sum(homegoals, na.rm = TRUE),
              goals_conceded_home = sum(awaygoals, na.rm = TRUE),
              .groups = "drop") 

# Goals away ----

premier_league_away_tbl <- premier_league_tbl %>% 
    group_by(season_end_year, away) %>% 
    summarise(goals_scored_away = sum(awaygoals, na.rm = TRUE),
              goals_conceded_away = sum(homegoals, na.rm = TRUE),
              .groups = "drop") 

# Join tables ----

premier_league_goals_diff_year <- premier_league_home_tbl %>% 
    left_join(premier_league_away_tbl, by = c("season_end_year", "home" = "away")) %>% 
    rename(`club` = home) %>% 
    mutate(goals_difference = (goals_scored_home + goals_scored_away) - 
               (goals_conceded_home + goals_conceded_away)) 

# Select only clubs in the latest season ----

premier_league_latest_tbl <- premier_league_goals_diff_year %>% 
    
    # Only clubs in the latest season
    mutate(flag_latest_season = case_when(
        season_end_year == max(season_end_year) ~ 1,
        TRUE ~ 0
    )) %>% 
    group_by(club) %>% 
    mutate(club_latest_season = max(flag_latest_season)) %>% 
    filter(club_latest_season == 1)  %>% 
    ungroup() %>% 
    
    # Arrange and reorder
    arrange(season_end_year, desc(goals_difference)) %>%
    mutate(
        club = club %>% fct_reorder2(season_end_year, desc(goals_difference)),
        season = str_glue("{season_end_year - 1}-{str_sub(season_end_year, 3, 4)}")
    ) 

# 1st plot

premier_league_latest_tbl %>%
    ggplot(aes(x=season, y = club, fill=goals_difference)) +
    geom_tile(height=.7, alpha=.9) +
    scale_x_discrete(breaks = c("2009-10", "2011-12", "2013-14", "2015-16", "2017-18", "2019-20", "2021-22"), position="top") +
    coord_cartesian(expand=F) 

## Adding club´s flags ----

flag_file <- dir("flags", full.names=TRUE)

flag_tbl <- tibble(club = sub(".png", "", sub("flags/","",flag_file)),
                   flag = flag_file)

# Join flags

premier_league_latest_flag_tbl <- premier_league_latest_tbl %>%
    left_join(flag_tbl) %>% 
    mutate(flag_club = str_glue("{club}  <img src='{flag}' width = '15' /><br>") ) %>%
    mutate(
        club = club %>% fct_reorder2(season_end_year, desc(goals_difference)),
        club_num = as.numeric(club)
    ) 

flag_club_ordered <- premier_league_latest_flag_tbl %>% 
    # pull(flag_club) %>% 
    distinct(club_num, flag_club) %>% 
    arrange(club_num)

plot_gd_logos <- premier_league_latest_flag_tbl %>%
    ggplot(aes(x=season, y = club, fill=goals_difference)) +
    geom_tile(height=.7, alpha=.9) +
    scale_x_discrete(breaks = c("2009-10", "2011-12", "2013-14", "2015-16", "2017-18", "2019-20", "2021-22"), position="top") +
    coord_cartesian(expand=F) +
    scale_fill_gradient(low = "#FFE135", high = "#3f004f", n.breaks = 6) +
    scale_y_discrete(name = NULL,
                     labels = flag_club_ordered$flag_club) +
    theme(axis.text.y = element_markdown()) +
    cowplot::theme_minimal_grid(13.5) +
    theme(
        legend.position = "top",
        axis.title=element_blank(),
        plot.margin=margin(.5,1,.5,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(face="bold"),
        panel.grid=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=10.5),
        legend.margin=margin(l=-70),
        plot.subtitle=element_text(size=10.5),
        plot.caption.position = "plot",
        plot.caption=element_text(size=9, color="grey30", hjust=0, lineheight = 1),
        axis.text.x.top = element_text(color="grey30", size=11, face = "bold.italic"),
        axis.text.y=element_markdown(size=11)
    ) +
    labs(
        title = "English Premier League - Goals Difference",
        subtitle = "Current 20 EPL Clubs Goals Difference Since 2009/10",
        x = "",
        y = "",
        caption = "Clubs ordered by the GD in season 2021/22\nSource: FBref.com\n@vidigal_br") +
    guides(fill = guide_colorbar(
        label.position = "top", title.hjust = .6, barwidth = unit(15, "lines"),
        barheight = unit(.9, "lines"))) +
    theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    )

plot_gd_logos

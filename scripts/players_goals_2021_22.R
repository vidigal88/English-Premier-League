# References ----
# https://twitter.com/ikashnitsky/status/1521960898440613889/photo/2

# Load libraries ----

# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
library(tidyverse)
library(tidyquant)
library(cowplot)
library(ggtext)
library(magick)
library(ggtext)

# Players´s url ----
top_5_players_url <- tibble(url = c("https://fbref.com/en/players/e342ad68/Mohamed-Salah", 
                                    "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo", 
                                    "https://fbref.com/en/players/92e7e919/Son-Heung-min", 
                                    "https://fbref.com/en/players/21a66f6a/Harry-Kane", 
                                    "https://fbref.com/en/players/c691bfe2/Sadio-Mane")) 

# Function to download players summary ----
download_players_summary <- function(url) {
    
    data <- url %>% 
        fb_player_match_logs(season_end_year = 2022, stat_type = 'summary')
    
    write_rds(data, file = str_glue("data/{tolower(substring(url, 39))}_summary.rds"))
}

# Download players´s summary data ----
top_5_players_url %>% 
    mutate(url %>% map(download_players_summary))

# Read in players´s summary data

fs::dir_info("data/players")

paths_chr <- fs::dir_info("data/players") %>% 
    pull(path)

players_summary <- paths_chr %>% 
    map(read_rds) %>% 
    set_names(paths_chr) %>% 
    bind_rows()

players_summary %>% distinct(Player)

players_goals_week_tbl <- players_summary %>% 
    filter(Gls >= 1 & Comp == "Premier League") %>% 
    select(Player, Season, Round, Venue, Gls) %>% 
    mutate(round_num = parse_number(Round)) %>% 
    group_by(Player) %>% 
    mutate(total_goals = sum(Gls)) %>% 
    ungroup() %>% 
    
    # Reorder
    mutate(player_chr = str_glue("{Player}: {total_goals} goals"),
           player_chr = player_chr %>% fct_reorder(desc(total_goals)),
           player_num = as.numeric(player_chr)) %>% 
    
    # Cum goals
    group_by(Player, Season, player_chr, player_num) %>% 
    mutate(goals_cum = cumsum(Gls)) %>% 
    ungroup()

# 1st plot

players_goals_week_tbl %>% 
    ggplot(aes(y = reorder(round_num, -round_num), x = Gls
               ,color = Venue
    )) +
    geom_point(size = 1.5) +
    facet_wrap(~ player_chr) +
    scale_y_discrete(breaks = seq(0, 38, 2)) +
    # scale_y_continuous(breaks = seq(0, 38, 2), limits = c(1, 38)) +

    scale_x_continuous(breaks = seq(1, 5, 1)) +
    labs( 
        title = "Top 5 EPL Player Stats: Goals",
        subtitle = "Season 2021-22",
        caption = "Source: FBref.com\n@vidigal_br") +
    ylab("Round") + xlab("Goals") +
    theme_tq() +
    scale_color_tq() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(face = "bold")) +
    theme(axis.text.y = element_text(face = "bold")) +
    theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    ) 


# From round 30 onwards, Son was decisive scoring 11 goals (including a hat-trick) while Salah netted only 3 times.


# Plot Total, Home and Away Goals

away_home_tbl <- players_goals_week_tbl %>% 
    select(Player, Venue, Gls) %>% 
    group_by(Player, Venue) %>% 
    summarise(goals = sum(Gls)) %>% 
    ungroup() %>% 
    spread(Venue, goals) %>% 
    mutate(total_goals = Away + Home) %>% 
    
    # Reorder
    mutate(Player = Player %>% fct_reorder(total_goals))

# 2nd plot

away_home_tbl %>% 
    ggplot(aes(y = Player))+
    geom_hline(
        aes(yintercept = Player),
        size = 5, color = "#eaeaea") +
    geom_point(
        aes(x = Away),
        color = "#024281",
        size = 3) +
    geom_point(
        aes(x = Home),
        color = "#BC6C82",
        size = 3,
        width = 0.4, height = 0.1) +
    geom_point(
        aes(x = total_goals),
        color = "#edc177",
        size = 3) +
    geom_text(
        data = tibble(
            label = c(
                "Away",
                "Home",
                "Total Goals"
            )
        ),
        x = c(4, 12, 22), y = 5.3,
        aes(label = label),
        vjust = 0, size = 5,
        color = c("#024281", "#BC6C82", "#edc177"),
        lineheight = .8) +
    coord_cartesian(ylim = c(1, 5), xlim = c(0, 25)) +
    theme_tq() +
    scale_color_tq() +
    theme(
        axis.text.y = element_text(face = 2, size = 10)
    ) +
    labs(
        x = "Goals",
        y = NULL
    ) +
    labs( 
        title = "Top 5 EPL Player Stats: Home, Away and Total Goals",
        subtitle = "Season 2021-22",
        caption = "Source: FBref.com\n@vidigal_br") +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(face = "bold")) +
    theme(axis.text.y = element_text(face = "bold")) +
    theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    ) 



# References ----
# https://www.r-bloggers.com/2019/08/ggtext-for-images-as-x-axis-labels/
# https://stackoverflow.com/questions/67491715/adding-images-below-x-axis-labels-in-ggplot2
# https://wilkelab.org/ggtext/

# Load library ----
library(rvest)

# URL ----
url <- "https://fbref.com/en/comps/9/Premier-League-Stats"
html <- read_html(url)

flags_img  <- html_nodes(html_nodes(html, "table")[1][[1]], "img")[1:20]

flags_url <- paste0("http://d2p3bygnnzw9w3.cloudfront.net/req/202204185/tlogo/fb/mini.", substr(sub(".*mini.", "", flags_img), 1, 8), ".png")

# Club´s names
club_name <- url %>%
    read_html() %>% 
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table() %>% 
    select(Squad)

club_flags_names_tbl <- bind_cols(club_name, flags_url) %>% 
    set_names(c("club", "logo_url"))


# Download flags ----
for(i in seq_along(club_flags_names_tbl$club)) {
    
    download.file(club_flags_names_tbl$logo_url[i], paste0("flags/", club_flags_names_tbl$club[i], ".png"))
}



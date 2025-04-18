library(rvest)
library(stringr)

url <- "https://www.sputnik8.com/ru/moscow"
page <- read_html(url)

tours <- page %>% html_nodes(".activity-card_new_xjRx")

titles <- tours %>% html_node(".title_xIA3") %>% html_text(trim = TRUE)

descriptions <- tours %>% html_node(".description_phiK") %>% 
  html_text(trim = TRUE)

prices <- tours %>% html_node(".gtm-activity-card-price") %>%
  html_text(trim = TRUE) %>% str_replace_all("[^0-9]", "") %>%
  as.numeric()

durations <- sapply(tours, function(tour) {
  duration <- tour %>%html_nodes("div.detail_QQhb") %>%html_text(trim = TRUE)
  tail(duration, 1)
})


links <- tours %>%html_node("a") %>%html_attr("href") %>%
  paste0("https://www.sputnik8.com", .)

df <- data.frame(
  Название = titles,
  Описание = descriptions,
  Длительность = durations,
  Цена = prices,
  Ссылка = links,
  stringsAsFactors = FALSE
)
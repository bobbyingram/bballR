scrape_players <- function(initial){
  stopifnot(stringr::str_length(initial) == 1)
  stopifnot(stringr::str_detect(initial, "[:alpha:]"))

  initial <- stringr::str_to_lower(initial)

  url <- glue::glue("http://www.basketball-reference.com/players/{initial}/")

  html <- xml2::read_html(url)

  suffix <- rvest::html_nodes(html, css = "th a") %>%
    rvest::html_attr(name = "href")

  player_id <- suffix %>%
    stringr::str_replace(pattern = "/players/[:alpha:]/", "") %>%
    stringr::str_replace(pattern = ".html", "")

  table <- rvest::html_node(html, css = "table") %>%
    rvest::html_table(header = TRUE)

  players <- table %>%
    dplyr::rename(Position = .data$Pos,
                  Height = .data$Ht,
                  Weight = .data$Wt,
                  DOB = .data$`Birth Date`) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Player = stringr::str_replace_all(.data$Player, "\\*", ""),
                  DOB = lubridate::mdy(.data$DOB, tz = NULL),
                  PlayerId = player_id) %>%
    dplyr::mutate_if(is.character, empty_as_na) %>%
    dplyr::select(.data$Player, .data$PlayerId, dplyr::everything())

  players
}

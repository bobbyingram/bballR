empty_as_na <- function(x){
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  }
  ifelse(as.character(x) != "", x, NA)
}

year_to_season <- function(year){
  glue::glue("{year-1}-{stringr::str_sub(year, 3, 4)}")
}

parse_player_ids <- function(node){
  format <- "/players/[:alpha:]"
  links <- rvest::html_nodes(node, css = "a")
  i <- stringr::str_detect(links, format)
  links <- links[i]
  ids <- rvest::html_attr(links, name = "href") %>%
    stringr::str_replace(format, "") %>%
    stringr::str_replace(".html", "") %>%
    tibble::tibble(PlayerId = .,
                   Player = rvest::html_text(links)) %>%
    dplyr::group_by(.data$Player) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}

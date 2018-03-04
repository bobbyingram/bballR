#' Scrape all NBA players
#'
#' This function scrapes the entire NBA & ABA player directory.
#'
#' @return A tibble with a row for each player.
#'
#' The fields are:
#'
#' \describe{
#' \item{Player}{Player name}
#' \item{PlayerId}{https://www.basketball-reference.com/ player page identifier}
#' \item{From}{First year in league}
#' \item{To}{Last year in league}
#' \item{Pos}{Position(s) played}
#' \item{Ht}{Height in 'feet-inches' format}
#' \item{Wt}{Weight in pounds}
#' \item{Birth Date}{Date of birth}
#' \item{College}{What college did the player attend?}
#' \item{HoF}{Is this player in the Hall of Fame?}
#' }
#'
#' @export
#' @md
scrape_all_players <- function(){
  # As of 2017 there are no players with initial X so ignore it.
  purrr::map_df(letters[-24], scrape_players_by_initial)
}

scrape_players_by_initial <- function(initial){
  # Check
  stopifnot(stringr::str_length(initial) == 1)
  stopifnot(stringr::str_detect(initial, "[:alpha:]"))

  # Scrape
  letter <- stringr::str_to_lower(initial)
  url <- glue::glue("http://www.basketball-reference.com/players/{letter}/")
  try(html <- xml2::read_html(url), silent = TRUE)

  node <- rvest::html_node(html, xpath ='//*[@id="players"]')

  # Parse
  parse_players(node)
}

parse_players <- function(node){
  ids <- parse_player_ids(node)

  # Scrape the stats and join the ids on player.
  table <- rvest::html_table(node, header = T)
  table <- table[, which(colnames(table) != "")] #Remove blank columns.

  dt <- table %>%
    dplyr::mutate(HoF = stringr::str_detect(.data$Player, "\\*"),
                  Player = stringr::str_replace_all(.data$Player, "\\*", ""),
                  `Birth Date` = lubridate::mdy(.data$`Birth Date`, tz = NULL)) %>%
    dplyr::mutate_if(is.character, empty_as_na) %>%
    dplyr::left_join(ids, by = "Player") %>%
    dplyr::select(.data$Player, .data$PlayerId, dplyr::everything())

  tibble::as_tibble(dt)
}

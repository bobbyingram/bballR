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
#' \item{PlayerId}{Player page Id}
#' \item{From}{First year in league}
#' \item{To}{Last year in league}
#' \item{Position}{Position(s) played}
#' \item{Height}{Height in 'feet-inches' format}
#' \item{Weight}{Weight in pounds}
#' \item{DOB}{Date of birth}
#' \item{College}{What college did the player attend?}
#' \item{HOF}{Is this player in the Hall of Fame?}
#' }
#' @export
#' @md
scrape_all_players <- function(){
  # As of 2017 there are no players with initial X so ignore it.
  purrr::map_df(letters[-24], scrape_players_by_initial)
}

scrape_players_by_initial <- function(initial){
  stopifnot(stringr::str_length(initial) == 1)
  stopifnot(stringr::str_detect(initial, "[:alpha:]"))

  letter <- stringr::str_to_lower(initial)
  url <- glue::glue("http://www.basketball-reference.com/players/{initial}/")

  try(html <- xml2::read_html(url), silent = TRUE)

  if (!is.list(html)) {
    return (tibble::tibble())
  }

  links <- rvest::html_nodes(html, css = "th a") %>%
    rvest::html_attr(name = "href")

  player_id <- links %>%
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
    dplyr::mutate(HOF = stringr::str_detect(.data$Player, "\\*"),
                  Player = stringr::str_replace_all(.data$Player, "\\*", ""),
                  DOB = lubridate::mdy(.data$DOB, tz = NULL),
                  PlayerId = player_id) %>%
    dplyr::mutate_if(is.character, empty_as_na) %>%
    dplyr::select(.data$Player, .data$PlayerId, dplyr::everything())

  players
}

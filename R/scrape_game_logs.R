#' Scrape player per game statistics for a given season.
#'
#' This function scrapes the per game player statistics for a single season.
#'
#' @param player_id The basketball-reference player page identifier code.
#' @param year The year that the season in question finished. i.e. 2015 would
#' return the statistics for the season 2014-15.
#' @return A tibble with a row for each player.
#'
#' The fields are:
#' \describe{
#' \item{Season}{The regular season the game took place in}
#' \item{Player}{Player name}
#' \item{G}{Season game}
#' \item{Date}{The date the game took place on}
#' \item{Age}{The age of the player on the day of the game}
#' \item{Tm}{Team}
#' \item{Opp}{Opposition}
#' \item{GS}{Games started}
#' \item{MP}{Minutes played}
#' \item{FG}{Field goals}
#' \item{FGA}{Field goal attempts}
#' \item{FG\%}{Field goal percentage}
#' \item{3P}{3-point field goals}
#' \item{3PA}{3-point field goal attempts}
#' \item{3P\%}{Field goal percentage on 3-point field goal attempts}
#' \item{FT}{Free throws}
#' \item{FTA}{Free throw attempts}
#' \item{FT\%}{Free throw percentage}
#' \item{ORB}{Offensive rebounds}
#' \item{DRB}{Defensive rebounds}
#' \item{TRB}{Total rebounds}
#' \item{AST}{Assists}
#' \item{STL}{Steals}
#' \item{BLK}{Blocks}
#' \item{TOV}{Turnovers}
#' \item{PF}{Personal fouls}
#' \item{PTS}{Points}
#' \item{GmSc}{Game score}
#' \item{+/-}{Plus minus (possibly)}
#' \item{PlayerId}{https://www.basketball-reference.com/ player page identifier}
#' }
#' @export
#' @md
scrape_game_logs <- function(player_id, year){
  # Check
  stopifnot(is.numeric(year) & is.character(player_id))
  stopifnot(stringr::str_detect(player_id, "^([a-zA-Z]+)(\\d\\d)"))

  min_year <- 1947
  max_year <- lubridate::year(lubridate::today()) + 1
  if (!(dplyr::between(year, min_year, max_year))) {
    stop(glue::glue("year must be between {min_year} and {max_year}."))
  }

  # Scrape
  initial <- stringr::str_sub(player_id, 1, 1)
  url <- glue::glue("https://www.basketball-reference.com/players/",
                    "{initial}/{player_id}/gamelog/{year}")
  try(html <- xml2::read_html(url), silent = TRUE)

  if (!exists("html")){
    stop(glue::glue("bad request url: {url}"))
  }

  # Parse
  season <- year_to_season(year)
  title <- rvest::html_node(html, "title") %>%
    rvest::html_text()
  i <- stringr::str_locate(title, "[:number:]")[1,1]
  player <- stringr::str_sub(title, 1, i - 2)

  # Regular season.
  node <- rvest::html_node(html, xpath = '//*[@id="pgl_basic"]')
  table <- rvest::html_table(node, header = T, fill = T) %>%
    remove_blank_cols()

  dt <- table %>%
    dplyr::filter(.data$Rk != "Rk") %>%
    dplyr::mutate_if(is.character, empty_as_na) %>%
    dplyr::mutate_at(c(2, 7, 9, 10, 12, 13, 15, 16, 18:26),
                     as_suppress, as.integer) %>%
    dplyr::mutate_at(c(11, 14, 17, 27),
                     as_suppress, as.numeric) %>%
    dplyr::mutate(Date = lubridate::as_date(.data$Date),
                  MP = ms_to_minutes(.data$MP),
                  Age = yd_to_years(.data$Age, .data$Date),
                  Season = season,
                  PlayerId = player_id,
                  Player = player) %>%
    dplyr::select(-.data$Rk) %>%
    dplyr::select(.data$Season, .data$Player,
                  dplyr::everything())

  tibble::as_tibble(dt)
}

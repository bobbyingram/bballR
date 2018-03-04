#' Scrape player per game statistics for a given season.
#'
#' This function scrapes the per game player statistics for a single season.
#'
#' @param year The year that the season in question finished. i.e. 2015 would
#' return the statistics for the season 2014-15.
#' @return A tibble with a row for each player.
#'
#' The fields are:
#' \describe{
#' \item{Season}{The season these statistics are for}
#' \item{Player}{Player name}
#' \item{PlayerId}{https://www.basketball-reference.com/ player page identifier}
#' \item{Pos}{Position}
#' \item{Age}{Age of player at the start of February 1st of that season}
#' \item{Tm}{Team}
#' \item{G}{Games}
#' \item{GS}{Games started}
#' \item{MP}{Minutes played}
#' \item{FG}{Field goals per game}
#' \item{FGA}{Field goal attempts per game}
#' \item{FG\%}{Field goal percentage}
#' \item{3P}{3-point field goals per game}
#' \item{3PA}{3-point field goal attempts per game}
#' \item{3P\%}{Field goal percentage on 3-point field goal attempts}
#' \item{2P}{2-point field goals per game}
#' \item{2PA}{2-point field goal attempts per game}
#' \item{2P\%}{Field goal percentage on 2-point field goal attempts}
#' \item{eFG\%}{Effective field goal percentage}
#' \item{FT}{Free throws per game}
#' \item{FTA}{Free throw attempts per game}
#' \item{FT\%}{Free throw percentage}
#' \item{ORB}{Offensive rebounds per game}
#' \item{DRB}{Defensive rebounds per game}
#' \item{TRB}{Total rebounds per game}
#' \item{AST}{Assists per game}
#' \item{STL}{Steals per game}
#' \item{BLK}{Blocks per game}
#' \item{TOV}{Turnovers per game}
#' \item{PF}{Personal fouls per game}
#' \item{PS/G}{Points per game}
#' }
#'
#' @export
#' @md
scrape_season_per_game <- function(year){
  scrape_season(year, stat_type = "per game")
}

#' Scrape player advanced statistics for a given season.
#'
#' This function scrapes the advanced player statistics for a single season.
#'
#' @inheritParams scrape_season_per_game
#' @return A tibble with a row for each player.
#'
#' The fields are:
#' \describe{
#' \item{Season}{The season these statistics are for}
#' \item{Player}{Player name}
#' \item{PlayerId}{https://www.basketball-reference.com/ player page identifier}
#' \item{Pos}{Position}
#' \item{Age}{Age of player at the start of February 1st of that season}
#' \item{Tm}{Team}
#' \item{G}{Games}
#' \item{PER}{Player efficiency rating: A measure of per-minute production
#' standardized such that the league average is 15.}
#' \item{TS\%}{True shooting percentage: A measure of shooting efficiency that
#' takes into account 2-point field goals, 3-point field goals, and free throws.}
#' \item{3PAr}{3-point attempt rate: The percentage of field goal attempts
#' taken from 3-point range}
#' \item{FTr}{Free throw attempt rate: The number of free throw attempts per field
#' goal attempt}
#' \item{ORB\%}{Offensive rebound percentage: An estimate of the percentage
#' of available offensive rebounds a player grabbed while he was on the floor}
#' \item{DRB\%}{Defensive rebound percentage: An estimate of the percentage
#' of available defensive rebounds a player grabbed while he was on the floor}
#' \item{TRB\%}{Total rebound percentage: An estimate of the percentage
#' of available rebounds a player grabbed while he was on the floor}
#' \item{AST\%}{Assist percentage: An estimate of the percentage of team-mate
#' field goals a player assisted while he was on the floor}
#' \item{STL\%}{Steal percentage: An estimate of the percentage of opponent
#' possessions that end with a steal by the player while he was on the floor}
#' \item{BLK\%}{Block percentage: An estimate of opponent two-point field goal
#' attempts blocked by the player while he was on the floor}
#' \item{TOV\%}{Turnover percentage: An estimate of turnovers committed per 100
#' plays}
#' \item{USG\%}{Usage percentage: An estimate of the percentage of team plays
#' used by a player while he was on the floor}
#' \item{OWS}{Offensive win shares: An estimate of the number of wins
#' contributed by a player due to his offense}
#' \item{DWS}{Defensive win shares: An estimate of the number of wins
#' contributed by a player due to his defense}
#' \item{WS}{Win shares: An estimate of the number of wins contributed by a
#' player}
#' \item{WS/48}{Win shares per 48 minutes: An estimate of the number of wins
#' contributed by a player per 48 minutes (the league average is .100)}
#' \item{OBPM}{Offensive box plus/minus: A box score estimate of the offensive
#' points per 100 possessions a player contributed above a league average
#' player, translated to an average team}
#' \item{DBPM}{Defensive box plus/minus: A box score estimate of the defensive
#' points per 100 possessions a player contributed above a league average
#' player, translated to an average team}
#' \item{BPM}{Box plus/minus: A box score estimate of the points per 100
#' possessions a player contributed above a league average player, translated to
#' an average team}
#' \item{VORP}{Value over replacement player: A box score estimate of the
#' points per 100 team possessions that a player contributed above a
#' replacement level (-2.0) player, translated to an average team and
#' prorated to an 82-game season. Multiply by 2.70 to convert to wins over
#' replacement.}
#' }
#' @export
#' @md
scrape_season_advanced <- function(year){
  scrape_season(year, stat_type = "advanced")
}

#' Scrape player per 36 minute statistics for a given season.
#'
#' This function scrapes the per 36 minute player statistics for a single season.
#'
#' @param year The year that the season in question finished. i.e. 2015 would
#' return the statistics for the season 2014-15.
#' @return A tibble with a row for each player.
#'
#' The fields are:
#' \describe{
#' \item{Season}{The season these statistics are for}
#' \item{Player}{Player name}
#' \item{PlayerId}{https://www.basketball-reference.com/ player page identifier}
#' \item{Pos}{Position}
#' \item{Age}{Age of player at the start of February 1st of that season}
#' \item{Tm}{Team}
#' \item{G}{Games}
#' \item{GS}{Games started}
#' \item{MP}{Minutes played}
#' \item{FG}{Field goals per 36 minutes}
#' \item{FGA}{Field goal attempts per 36 minutes}
#' \item{FG\%}{Field goal percentage}
#' \item{3P}{3-point field goals per 36 minutes}
#' \item{3PA}{3-point field goal attempts per 36 minutes}
#' \item{3P\%}{Field goal percentage on 3-point field goal attempts}
#' \item{2P}{2-point field goals per 36 minutes}
#' \item{2PA}{2-point field goal attempts per 36 minutes}
#' \item{2P\%}{Field goal percentage on 2-point field goal attempts}
#' \item{eFG\%}{Effective field goal percentage}
#' \item{FT}{Free throws per 36 minutes}
#' \item{FTA}{Free throw attempts per 36 minutes}
#' \item{FT\%}{Free throw percentage}
#' \item{ORB}{Offensive rebounds per 36 minutes}
#' \item{DRB}{Defensive rebounds per 36 minutes}
#' \item{TRB}{Total rebounds per 36 minutes}
#' \item{AST}{Assists per 36 minutes}
#' \item{STL}{Steals per 36 minutes}
#' \item{BLK}{Blocks per 36 minutes}
#' \item{TOV}{Turnovers per 36 minutes}
#' \item{PF}{Personal fouls per 36 minutes}
#' \item{PTS}{Points per 36 minutes}
#' }
#' @export
#' @md
scrape_season_per_36_minute <- function(year){
  scrape_season(year, stat_type = "per minute")
}

#' Scrape player per 100 possession statistics for a given season.
#'
#' This function scrapes the per 100 possessions player statistics for a single
#' season.
#'
#' @param year The year that the season in question finished. i.e. 2015 would
#' return the statistics for the season 2014-15.
#' @return A tibble with a row for each player.
#'
#' The fields are:
#' \describe{
#' \item{Season}{The season these statistics are for}
#' \item{Player}{Player name}
#' \item{PlayerId}{https://www.basketball-reference.com/ player page identifier}
#' \item{Pos}{Position}
#' \item{Age}{Age of player at the start of February 1st of that season}
#' \item{Tm}{Team}
#' \item{G}{Games}
#' \item{GS}{Games started}
#' \item{MP}{Minutes played}
#' \item{FG}{Field goals per 100 possessions}
#' \item{FGA}{Field goal attempts per 100 possessions}
#' \item{FG\%}{Field goal percentage}
#' \item{3P}{3-point field goals per 100 possessions}
#' \item{3PA}{3-point field goal attempts per 100 possessions}
#' \item{3P\%}{Field goal percentage on 3-point field goal attempts}
#' \item{2P}{2-point field goals per 100 possessions}
#' \item{2PA}{2-point field goal attempts per 100 possessions}
#' \item{2P\%}{Field goal percentage on 2-point field goal attempts}
#' \item{eFG\%}{Effective field goal percentage}
#' \item{FT}{Free throws per 100 possessions}
#' \item{FTA}{Free throw attempts per 100 possessions}
#' \item{FT\%}{Free throw percentage}
#' \item{ORB}{Offensive rebounds per 100 possessions}
#' \item{DRB}{Defensive rebounds per 100 possessions}
#' \item{TRB}{Total rebounds per 100 possessions}
#' \item{AST}{Assists per 100 possessions}
#' \item{STL}{Steals per 100 possessions}
#' \item{BLK}{Blocks per 100 possessions}
#' \item{TOV}{Turnovers per 100 possessions}
#' \item{PF}{Personal fouls per 100 possessions}
#' \item{PTS}{Points per 100 possessions}
#' \item{ORtg}{Offensive rating: An estimate of points produced per 100
#' possessions}
#' \item{DRtg}{Defensive rating: An estimate of points allowed per 100
#' possessions}
#' }
#' @export
#' @md
scrape_season_per_100_poss <- function(year){
  scrape_season(year, stat_type = "per poss")
}

#' Scrape player total statistics for a given season.
#'
#' This function scrapes the player statistics totals for a single season.
#'
#' @param year The year that the season in question finished. i.e. 2015 would
#' return the statistics for the season 2014-15.
#' @return A tibble with a row for each player.
#'
#' The fields are:
#' \describe{
#' \item{Season}{The season these statistics are for}
#' \item{Player}{Player name}
#' \item{PlayerId}{https://www.basketball-reference.com/ player page identifier}
#' \item{Pos}{Position}
#' \item{Age}{Age of player at the start of February 1st of that season}
#' \item{Tm}{Team}
#' \item{G}{Games}
#' \item{GS}{Games started}
#' \item{MP}{Minutes played}
#' \item{FG}{Field goals}
#' \item{FGA}{Field goal attempts}
#' \item{FG\%}{Field goal percentage}
#' \item{3P}{3-point field goals}
#' \item{3PA}{3-point field goal attempts}
#' \item{3P\%}{Field goal percentage on 3-point field goal attempts}
#' \item{2P}{2-point field goals}
#' \item{2PA}{2-point field goal attempts}
#' \item{2P\%}{Field goal percentage on 2-point field goal attempts}
#' \item{eFG\%}{Effective field goal percentage}
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
#' }
#' @export
#' @md
scrape_season_totals <- function(year){
  scrape_season(year, stat_type = "totals")
}

scrape_season <- function(year, stat_type){
  # Checks
  stopifnot(year %in% seq(1947, lubridate::year(lubridate::today()) + 1))
  options <- c("per game", "totals", "per minute", "per poss", "advanced")
  stats <- stringr::str_to_lower(stat_type)
  stopifnot(stats %in% options)

  # Scrape
  stats <- stringr::str_replace(stats, " ", "_")
  url <- glue::glue("https://www.basketball-reference.com/leagues/NBA_{year}_{stats}.html")
  try(html <- xml2::read_html(url), silent = TRUE)
  season <- year_to_season(year)
  path <- glue::glue( '//*[@id="{stats}_stats"]')
  node <- rvest::html_node(html, xpath = path)

  # Parse
  parse_stats(node) %>%
    dplyr::mutate(Season = season) %>%
    dplyr::select(.data$Season, dplyr::everything())
}

parse_stats <- function(node){
  ids <- parse_player_ids(node)

  # Scrape the stats and join the ids on player.
  table <- rvest::html_table(node, header = T)
  table <- table[, which(colnames(table) != "")] # Remove blank columns.

  dt <- table %>%
    dplyr::filter(.data$Rk != "Rk") %>%
    dplyr::mutate(Player = stringr::str_replace_all(.data$Player, "\\*", "")) %>%
    dplyr::mutate_at(c(4, 6, 7), as.integer) %>%
    dplyr::mutate_at(8:ncol(table), as.numeric) %>%
    dplyr::select(-.data$Rk) %>%
    dplyr::left_join(ids, by = "Player") %>%
    dplyr::select(.data$Player,
                  .data$PlayerId,
                  dplyr::everything())

  tibble::as_tibble(dt)
}

#' Scrape franchise records.
#'
#' This function scrapes the NBA & ABA Team Index for overall records by
#' franchise.
#'
#' @param status 'active', 'defunct' or 'both'. 'active' returns only
#' those franchises that are still participating in the NBA. 'defunct' returns
#' only those franchises that no longer participate in the NBA. 'both' returns
#' all franchises.
#' @return A tibble with a row for each team.
#'
#' The fields are:
#'
#' \describe{
#' \item{Franchise}{Franchise name}
#' \item{FranchiseId}{Three letter identifier}
#' \item{Lg}{The leagues the franchise participated in}
#' \item{From}{First year in league}
#' \item{To}{Last year in league}
#' \item{Yrs}{Years active}
#' \item{G}{Total games played}
#' \item{W}{Total wins}
#' \item{L}{Total losses}
#' \item{W\/L\%}{Wins / Losses}
#' \item{Plyfs}{Total appearances in playoffs}
#' \item{Div}{Total divisional title wins (including ties)}
#' \item{Conf}{Total conference title wins}
#' \item{Champ}{Total championship title wins}
#' }
#' @export
#' @md
scrape_teams <- function(status = "active"){
  # Checks
  options <- c("active", "defunct", "both")
  status <- stringr::str_to_lower(status)
  stopifnot(status %in% options)

  # Scrape
  if (status == "both") {
    return(purrr::map_df(options[1:2], scrape_teams))
  }
  url <- "https://www.basketball-reference.com/teams/"
  try(html <- xml2::read_html(url), silent = TRUE)
  path <- glue::glue('//*[@id="teams_{status}"]')
  node <- rvest::html_node(html, xpath = path)

  # Parse
  parse_teams(node)
}

parse_teams <- function(node){
  ids <- parse_team_ids(node)

  dt <- rvest::html_table(node, header = TRUE) %>%
    dplyr::inner_join(ids, by = "Franchise") %>%
    dplyr::group_by(.data$Franchise) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$Franchise, .data$FranchiseId, dplyr::everything())

  tibble::as_tibble(dt)
}

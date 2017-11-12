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
#' \item{League}{The leagues the franchise participated in}
#' \item{From}{First year in league}
#' \item{To}{Last year in league}
#' \item{Years}{Years active}
#' \item{Games}{Total games played}
#' \item{Won}{Total wins}
#' \item{Lost}{Total losses}
#' \item{WinLossRatio}{Wins / Losses}
#' \item{PlayoffApps}{Total appearances in playoffs}
#' \item{DivisionWins}{Total divisional title wins}
#' \item{ConferenceWins}{Total conference title wins}
#' \item{ChampionshipWins}{Total championship title wins}
#' \item{Active}{True if franchise is still active in NBA.}
#' }
#' @export
#' @md
scrape_teams <- function(status = "active"){
  options <- c("active", "defunct", "both")
  status <- stringr::str_to_lower(status)
  stopifnot(status %in% options)

  if (status == "both") {
    return(purrr::map_df(options[1:2], scrape_teams))
  }

  url <- "https://www.basketball-reference.com/teams/"

  try(html <- xml2::read_html(url), silent = TRUE)

  # Which table are we looking at? Defunct or active?
  path <- glue::glue('//*[@id="teams_{status}"]')

  # First we want the Franchise Ids. We can get these from those items in the
  # table with links.
  node <- rvest::html_node(html, xpath = path)

  names <- rvest::html_nodes(node, css = "th a") %>%
    rvest::html_text()

  ids <- rvest::html_nodes(node, css = "th a") %>%
    rvest::html_attr(name = "href") %>%
    stringr::str_replace(pattern = "/teams/", replacement = "") %>%
    stringr::str_replace(pattern = "/", replacement = "") %>%
    tibble::as_tibble() %>%
    dplyr::rename(FranchiseId = .data$value) %>%
    dplyr::mutate(Franchise = names,
                  Active = status == "Active")

  # Now we can get the details.
  teams <- rvest::html_table(node, header = TRUE) %>%
    dplyr::inner_join(ids, by = "Franchise") %>%
    dplyr::group_by(.data$Franchise) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(League = .data$Lg,
           Years = .data$Yrs,
           Games = .data$G,
           Won = .data$W,
           Lost = .data$L,
           WinLossRatio = .data$`W/L%`,
           PlayoffApps = .data$Plyfs,
           DivisionWins = .data$Div,
           ConferenceWins = .data$Conf,
           ChampionshipWins = .data$Champ) %>%
    dplyr::select(.data$Franchise, .data$FranchiseId, dplyr::everything())

  teams
}

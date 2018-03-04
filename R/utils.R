empty_as_na <- function(x, na = NA){
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  }
  ifelse(as.character(x) != "", x, na)
}

as_suppress <- function(x, f){
  suppressWarnings(as.integer(x))
}

year_to_season <- function(year){
  glue::glue("{year-1}-{stringr::str_sub(year, 3, 4)}")
}

remove_blank_cols <- function(dt){
  dt[, which(colnames(dt) != "")]
}

ms_to_minutes <- function(ms){
  ms[!stringr::str_detect(ms, ":")] <- "00:00"
  lubridate::period_to_seconds(lubridate::hms(paste0("00:", ms))) / 60
}

yd_to_years <- function(yd, date){
  s <- stringr::str_split(yd, "-", simplify = T)
  days <- dplyr::if_else(lubridate::leap_year(date), 366, 365)
  as.numeric(s[,1]) + as.numeric(s[,2])/days
}

parse_player_ids <- function(node){
  format <- "/players/[:alpha:]/"
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

parse_team_ids <- function(node){
  format <- "/teams/*/"
  links <- rvest::html_nodes(node, css = "th a")
  i <- stringr::str_detect(links, format)
  links <- links[i]
  ids <- rvest::html_attr(links, name = "href") %>%
    stringr::str_replace(format, "") %>%
    stringr::str_replace("/", "") %>%
    tibble::tibble(FranchiseId = .,
                   Franchise = rvest::html_text(links)) %>%
    dplyr::group_by(.data$Franchise) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}

as.num = function(x, na.strings = "NA") {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

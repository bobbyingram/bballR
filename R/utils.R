empty_as_na <- function(x){
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  }
  ifelse(as.character(x) != "", x, NA)
}

year_to_season <- function(year){
  glue::glue("{year-1}-{stringr::str_sub(year, 3, 4)}")
}

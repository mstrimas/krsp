is_integer <- function(x) {
  all(x == as.integer(x))
}

current_year <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

valid_loc <- function(x, alpha = TRUE) {
  if (is.numeric(x)) {
    # if a numeric vector convert to character to check
    x <- as.character(x)
  } else if (!is.character(x)) {
    # if neither numeric nor character, return FALSE
    return(rep(FALSE, length(x)))
  }
  # perform check
  if (alpha) {
    return(grepl("^([A-Z]|(-?[0-9]{1,2}))([.][0-9])?$", x, ignore.case = TRUE))
  } else {
    return(grepl("^-?[0-9]{1,2}([.][0-9])?$", x, ignore.case = TRUE))
  }
}

valid_year <- function(year, single = FALSE) {
  check <- all(is_integer(year) & (year >= 1984) & (year <= current_year()))
  if (single) {
    check <- check && (length(year) == 1)
  }
  return(check)
}

valid_grid <- function(grid, single = FALSE) {
  valid_grids <- c("AG", "BT", "CH", "EN", "FL", "JO", "KL",
                   "LL", "LR", "RR", "SU", "SX", "UL", "UR")
  check <- all(grid %in% valid_grids)
  if (single) {
    check <- check && (length(grid) == 1)
  }
  return(check)
}

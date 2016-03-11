is_integer <- function(x) {
  all(x == as.integer(x))
}

current_year <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

valid_grids <- function() {
  c("AG", "BT", "CH", "EN", "FL", "JO", "KL",
    "LL", "LR", "RR", "SU", "SX", "UL", "UR")
}

check_loc <- function(x) {
  grepl("^([A-Z]|(-?[0-9]{1,2}))([.][0-9])?$", x, ignore.case = TRUE)
}

#' @export
loc_to_numeric <- function(x) {
  assertthat::assert_that(is.character(x))
  x <- toupper(gsub('[[:space:]]', '', x))
  x <- ifelse(check_loc(x), x, NA)
  for (i in 1:26) {
    x <- sub(LETTERS[i], i, x, fixed = TRUE)
  }
  round(as.numeric(x), 1)
}

is_integer <- function(x) {
  all(x == as.integer(x))
}

current_year <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

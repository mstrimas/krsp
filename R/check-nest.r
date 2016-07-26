#' @export
#' @rdname check_trapping
check_trapping_locs <- function(con, grid, year, observer, reflo = TRUE) {
  UseMethod("check_trapping_locs")
}

#' @export
check_trapping_locs.krsp <- function(con, grid, year, observer, reflo = TRUE) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year),
              missing(observer) || is.character(observer),
              missing(observer) || all(nchar(observer) <= 3),
              assertthat::is.flag(reflo))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year) && missing(observer)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ !is.na(squirrel_id)) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # find bad locs
  results <- collect(trapping) %>%
    filter_(~ !valid_loc(locx, reflo = reflo),
            ~ !valid_loc(locy, alpha = FALSE, reflo = reflo)) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_locs"
  if (nrow(results) == 0) {
    message("check_trapping_locs: no errors found.")
  }
  return(results)
}

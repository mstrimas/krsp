#' Identify squirrels without breeding status
#'
#' Identify female squirrels who are missing a breeding status (\code{br} is the
#' \code{litter} table) in the given year. Note that all squirrels should have
#' appear in the litter table and have a breeding status even if they did not
#' breed in a given year.
#'
#' @param con Connection to KRSP database
#' @param year integer; vector of years to search within. Defaults to current year.
#'
#' @return A tbl of squirrels, including their IDs and identifying
#'    information. Can be converted to a data.frame.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_needs_br(con, 2015)
krsp_needs_br <- function(con, year) {
  UseMethod("krsp_needs_br")
}

#' @export
krsp_needs_br.krsp <- function(con, year = current_year()) {
  # assertions
  assertthat::assert_that(is_integer(year),
                         all(year >= 1984),
                         all(year <= current_year()))

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      filter(is.null(br), yr == year) %>%
      select(squirrel_id, br, yr)
    squirrel <- tbl(con, "squirrel")
  })
  inner_join(litter, squirrel, by = c("squirrel_id" = "id")) %>%
    arrange(gr, trap_date) %>%
    select(gr,
           id,
           colorlft, colorrt,
           taglft, tagrt,
           locx, locy,
           trap_date)
}

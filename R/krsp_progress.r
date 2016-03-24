#' Display seasonal data collection progress
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; defaults to current year
#'
#' @return Displays and returns a \code{ggvis} plot of rattle locations.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_progress(con, "AG", 2015)
krsp_progress <- function(con, grid, year) {
  UseMethod("krsp_progress")
}

#' @export
krsp_progress.krsp <- function(con, grid, year = current_year()) {
  # assertions
  assertthat::assert_that(assertthat::is.count(year),
                          all(year >= 1984),
                          all(year <= current_year()),
                          length(grid) == 1,
                          grid %in% valid_grids())

  year <- as.integer(year)
  grid_choice <- grid

  # query for most recent trapping record
  female_query <- sprintf(
    "SELECT
      t.id, t.squirrel_id, t.date,
      s.taglft, s.tagrt,
      s.colorlft, s.colorrt,
      t.ft, t.rep_con, t.nipple
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
    WHERE
      s.sex = 'F'
      AND s.gr = '%s'
      AND (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE YEAR(date) = %i
        GROUP BY squirrel_id
      );", grid_choice, year)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    females <- DBI::dbGetQuery(con$con, female_query)
    litter <- tbl(con, "litter") %>%
      filter(yr == year) %>%
      select(squirrel_id, ln, fieldBDate, date1, tagDt) %>%
      collect
  })

  # remove multiple trapping records from same date
  females <- females %>%
    # remove dead squirrels
    filter(ft %in% 1:3) %>%
    group_by(squirrel_id) %>%
    arrange(desc(id)) %>%
    filter(row_number() == 1) %>%
    ungroup

  # bring in litter data
  females <- left_join(females, litter, by = "squirrel_id") %>%
    arrange(squirrel_id, ln) %>%
    mutate(nest_status =
             ifelse(!is.na(tagDt), "N2",
                    ifelse(!is.na(date1), "N1",
                           ifelse(!is.na(fieldBDate), "Parturition", NA))),
           trap_status = ifelse(nipple == 5, "LL",
                                ifelse(rep_con == 1, "P0/P1",
                                       ifelse(rep_con == 4, "P2/P3", NA)))
    )
  females
}

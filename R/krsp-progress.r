#' Display seasonal data collection progress
#'
#' Display an interactive visualization of data collection progress for the
#' season. All adult females caught in the given year are inlcuded provided their
#' most recent trapping record has fate 1-3.
#'
#' The status of a female is based on the most recent trapping record and the
#' litter table. If the most recent trapping record is dated prior to the date
#' the litter table was updated it is ignored, and only the litter table is
#' used. If nipple condition is 5, the status is LL (Lost Litter). Otherwise,
#' status is Parturition, N1, or Completed if fieldBDate, date1, or tagDt fields
#' in litter table are filled in, respectively. Finally, if litter table dates
#' are empty, rep_con field in trapping record is used and status is P0, P1, P2,
#' or P3 is assigned for rep_con = 1, 2, 3, 4, respectively.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; defaults to current year
#' @param data logical; if TRUE return data frame instead of plotting
#'
#' @return Displays and returns a \code{ggvis} plot of seasonal workflow
#'   progress for all females, unless \code{data} is TRUE, in which case a data
#'   frame is returned and nothing is plotted.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_progress(con, "JO", 2015, data = TRUE) %>%
#'   head()
#' krsp_progress(con, "KL", 2011)
krsp_progress <- function(con, grid, year, data) {
  UseMethod("krsp_progress")
}

#' @export
krsp_progress.krsp <- function(con, grid, year = current_year(), data = FALSE) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE),
              valid_grid(grid, single = TRUE))

  year <- as.integer(year)
  grid_choice <- grid

  # query for most recent trapping record
  female_query <- sprintf(
    "SELECT
      t.id, t.squirrel_id, t.date,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      t.locx, t.locy,
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
        GROUP BY squirrel_id)
      AND t.squirrel_id NOT IN (
        SELECT j.squirrel_id
        FROM JUVENILE     j
        LEFT JOIN LITTER  l
          ON j.litter_id = l.id
        WHERE
          YEAR(COALESCE(fieldBDate, date1, tagDt)) = %i
          AND GRID = '%s'
        );", grid_choice, year, year, grid_choice)
  mass_query <- sprintf(
    "SELECT st.squirrel_id, GROUP_CONCAT(st.wgt SEPARATOR ',') AS mass
    FROM (
      SELECT
        t.squirrel_id,
        t.date,
        t.wgt
      FROM
        squirrel                s
        INNER JOIN trapping     t
          ON s.id = t.squirrel_id
      WHERE
        s.gr = '%s'
        AND s.sex = 'F'
        AND YEAR(t.date) = %i
        AND t.wgt > 100
      ORDER BY
        t.squirrel_id,
        t.date) st
    GROUP BY st.squirrel_id;",
    grid_choice, year)
  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    females <- krsp_sql(con, female_query)
    masses <- krsp_sql(con, mass_query)
    litter <- tbl(con, "litter") %>%
      filter_(~ yr == year) %>%
      select_("squirrel_id", "ln", "fieldBDate", "date1", "tagDt") %>%
      collect()
  })

  # remove multiple trapping records from same date
  females <- females %>%
    # remove dead squirrels
    filter_(~ ft %in% 1:3) %>%
    group_by_("squirrel_id") %>%
    arrange_(~ desc(id)) %>%
    filter_(~ row_number() == 1) %>%
    ungroup()

  # bring in litter data
  rep_con_map <- c("P0", "P1", "P2", "P3")
  females <- left_join(females, litter, by = "squirrel_id") %>%
    arrange_("squirrel_id", "ln") %>%
    # sort out the various permutations of data - messy!
    mutate_(
      date = ~ suppressWarnings(as.Date(lubridate::ymd(date))),
      ln = ~ ifelse(is.na(ln), 0, ln),
      nest_date = ~ pmax(fieldBDate, date1, tagDt, na.rm = TRUE),
      nest_date = ~ suppressWarnings(as.Date(lubridate::ymd(nest_date))),
      nest_status = ~ ifelse(!is.na(tagDt), "N2",
                           ifelse(!is.na(date1), "N1",
                                  ifelse(!is.na(fieldBDate), "Parturition", NA))),
      trap_status = ~ ifelse(!is.na(nipple) & nipple == 5, "LL",
                           ifelse(is.na(rep_con) | !rep_con %in% 1:4, "Unknown",
                                  rep_con_map[rep_con])),
      # nest record more recent than trap record
      status = ~ ifelse(!is.na(nest_date) & nest_date > date, nest_status,
                      # LL takes precedence
                      ifelse(trap_status == "LL", "LL",
                             ifelse(is.na(nest_status), trap_status, nest_status))),
      status = ~ ifelse(status == "N2", "Completed", status),
      status = ~ factor(status,
                      levels = c("", "Unknown", rep_con_map, "LL",
                                 "Parturition", "N1", "Completed")),
      completion = ~ ifelse(status == "LL", "Lost Litter",
                          ifelse(status == "Completed", "Completed",
                                 "In Progress")),
      completion = ~ factor(completion,
                          levels = c("In Progress", "Lost Litter",
                                     "Completed"))) %>%
    # prepare tags, colours, and locs
    mutate_(
      color_left = ~ ifelse(is.na(color_left) | color_left == "",
                            "-", color_left),
      color_right = ~ ifelse(is.na(color_right) | color_right == "",
                             "-", color_right),
      taglft = ~ ifelse(is.na(taglft) | taglft == "", "-", taglft),
      tagrt = ~ ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
      locx = ~ ifelse(is.na(locx) | locx == "", "-", locx),
      locy = ~ ifelse(is.na(locy) | locy == "", "-", locy),
      colours = ~ paste(color_left, color_right, sep = "/"),
      tags = ~ paste(taglft, tagrt, sep = "/"),
      loc = ~ paste(locx, locy, sep = "/"))
  # target trap date
  days_forward <- c(3, 3,
                    18, 14, 7, 3, 3,
                    10, 10, 10)
  names(days_forward) <-c("", "Unknown",
                          "P0", "P1", "P2", "P3", "LL",
                          "Parturition", "N1", "Completed")
  females <- females %>%
    mutate_(target_trap_date = ~ next_trap(as.character(status), date))
  # bring in masses
  females <- left_join(females, masses, by = "squirrel_id")
  # sensible ordering
  females <- females %>%
    group_by_("squirrel_id") %>%
    summarize_(arr_comp = ~ min(as.integer(completion), na.rm = TRUE),
              arr_status = ~ min(as.integer(status), na.rm = TRUE)) %>%
    inner_join(females, by = "squirrel_id") %>%
    arrange_("arr_comp", "arr_status", "squirrel_id", "ln") %>%
    select_("squirrel_id", "tags", "colours", "loc", litter_number = "ln",
           "status", "trap_status", "nest_date", "nest_status",
           "mass", last_trapped = "date", "target_trap_date")

  # return raw data frame or DataTable
  if (data) {
    return(females)
  } else {
    progress_datatable(females)
  }
}

progress_datatable <- function(df) {
  df <- df %>% select_(~ -trap_status)
  # create DataTable
  # mass sparkline javascript
  col_defs <- list(list(targets = 8,
                        render = DT::JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
  line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
  cb_line = DT::JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ",
                          line_string, ", chartRangeMin: ", 100, ", chartRangeMax: ", 300, " }); }"),
                   collapse = "")
  col_names <- c("ID", "Tags", "Colours", "Loc", "Litter",
                 "Status",  "Nest Date", "Nest Status",
                 "Mass", "Last Trapped", "Trap By")
  dt <- DT::datatable(df,
                      rownames = FALSE,
                      colnames = col_names,
                      class = "nowrap stripe compact",
                      fillContainer = TRUE,
                      options = list(
                        paging = FALSE,
                        searching = FALSE,
                        info = FALSE,
                        scrollX = TRUE,
                        scrollY = TRUE,
                        columnDefs = col_defs,
                        fnDrawCallback = cb_line))
  dt$dependencies <- append(dt$dependencies,
                            htmlwidgets:::getDependency("sparkline"))
  # highlight based on status
  clr <- c("#e41a1c", "#e41a1c",
           "#377eb8", "#377eb8", "#377eb8", "#377eb8",
           "#377eb8", "#377eb8", "#4daf4a")
  clr_lvl <- c("Unknown", "LL",
               "P0", "P1", "P2", "P3",
               "Parturition", "N1", "Completed")
  dt <- dt %>%
    DT::formatStyle("status",
                    color = "white",
                    textAlign = "center",
                    fontWeight = "bold",
                    backgroundColor = DT::styleEqual(clr_lvl, clr))
  return(dt)
}

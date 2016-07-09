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
#'   head
#' krsp_progress(con, "KL", 2011)
krsp_progress <- function(con, grid, year, data) {
  UseMethod("krsp_progress")
}

#' @export
krsp_progress.krsp <- function(con, grid, year = current_year(), data = FALSE) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              assertthat::is.count(year),
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
  rep_con_map <- c("P0", "P1", "P2", "P3")
  females <- left_join(females, litter, by = "squirrel_id") %>%
    arrange(squirrel_id, ln) %>%
    # sort out the various permutations of data - messy!
    mutate(
      date = suppressWarnings(as.Date(lubridate::ymd(date))),
      ln = ifelse(is.na(ln), 0, ln),
      nest_date = pmax(fieldBDate, date1, tagDt, na.rm = TRUE),
      nest_date = suppressWarnings(as.Date(lubridate::ymd(nest_date))),
      nest_status = ifelse(!is.na(tagDt), "N2",
                           ifelse(!is.na(date1), "N1",
                                  ifelse(!is.na(fieldBDate), "Parturition",
                                         "None"))),
      trap_status = ifelse(!is.na(nipple) & nipple == 5, "LL",
                           ifelse(is.na(rep_con) | !rep_con %in% 1:4, "Unknown",
                                  rep_con_map[rep_con])),
      # nest record more recent than trap record
      status = ifelse(!is.na(nest_date) & nest_date > date, nest_status,
                      # LL takes precedence
                      ifelse(trap_status == "LL", "LL",
                             ifelse(nest_status == "None", trap_status, nest_status))),
      status_label = ifelse(status %in% c("N2", "LL"), "Completed",
                            ifelse(status == "None", "Unknown", status)),
      status_label = factor(status_label,
                            levels = c("", "Unknown", rep_con_map,
                                       "Parturition", "N1", "Completed")),
      completion = ifelse(status == "LL", "Lost Litter",
                          ifelse(status == "N2", "Completed Successfully",
                                 "In Progress")),
      completion = factor(completion,
                          levels = c("In Progress", "Completed Successfully",
                                     "Lost Litter"))) %>%
    # prepare tags and colours
    mutate(
      id = row_number(),
      colorlft = ifelse(is.na(colorlft) | colorlft == "", "-", colorlft),
      colorrt = ifelse(is.na(colorrt) | colorrt == "", "-", colorrt),
      taglft = ifelse(is.na(taglft) | taglft == "", "-", taglft),
      tagrt = ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
      colours = paste(colorlft, colorrt, sep = "/"),
      tags = paste(taglft, tagrt, sep = "/"),
      label = sprintf("%s %s %s %s", squirrel_id, tags, colours, ln),
      label = factor(label))
  # sensible ordering
  females <- group_by(females, squirrel_id) %>%
    summarize(max_status = min(as.integer(status_label), na.rm = TRUE)) %>%
    inner_join(females, by = "squirrel_id") %>%
    arrange(max_status, tags, ln) %>%
    mutate(id = row_number(),
           label = reorder(label, id, max)) %>%
    select(id, squirrel_id, tags, colours, litter_number = ln,
           locx, locy,
           trap_date = date, trap_status,
           nest_date, nest_status,
           status = status_label, completion, label) %>%
    arrange(squirrel_id, litter_number) %>%
    mutate(litter_number = factor(litter_number, levels = 0:2,
                                  labels = c("None", "1", "2")),
           id = row_number())

  # skip plotting and return data frame instead
  if (data) {
    return(females)
  }

  # produce interactive bar chart
  # create interactive plot
  popup <- function(x) {
    row <- females[females$id == x$id, ]
    paste(
      sprintf("<strong>Squirrel ID:</strong> %s", row$squirrel_id),
      sprintf("<strong>Tags:</strong> %s", row$tags),
      sprintf("<strong>Colours:</strong> %s", row$colours),
      sprintf("<strong>LocX:</strong> %s; <strong>LocX:</strong> %s",
              row$locx, row$locy),
      sprintf("<strong>Last Trapped:</strong> %s",
              format(row$trap_date, "%Y-%m-%d")),
      sprintf("<strong>Trapping Status:</strong> %s", row$trap_status),
      sprintf("<strong>Nest:</strong> %s", row$nest_status),
      sprintf("<strong>Litter Number:</strong> %s", row$litter_number),
      sprintf("<strong>Nest Date:</strong> %s",
              ifelse(is.na(row$nest_date), "",
                     format(row$nest_date, "%Y-%m-%d"))),
      sep = "<br />")
  }

  fnt <- c("Helvetica Neue", "sans-serif")
  g <- ggvis::ggvis(females) %>%
    ggvis::layer_rects(x = "", x2 = ~status, y = ~label,
                       height = ggvis::band(), fill = ~completion,
                       key := ~id) %>%
    ggvis::scale_nominal("fill", range = c("#377EB8", "#4DAF4A", "#E41A1C"),
                         label = NA) %>%
    # axes
    ggvis::add_axis("y", title = "",
                    properties = ggvis::axis_props(
                      labels = list(fontSize = 10, font = fnt)
                    )) %>%
    ggvis::add_axis("x", title = "Status", orient = "bottom",
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    ggvis::add_axis("x", title = "", orient = "top",
                    properties = ggvis::axis_props(
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    # popup tooltips with additional information
    ggvis::add_tooltip(popup) %>%
    ggvis::set_options(height = 13 * nrow(females) + 100, width = 900)
  return(g)
}

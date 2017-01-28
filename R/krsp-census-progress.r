#' Display a table of progress towards census
#'
#' This function is to meant to be used to monitor progress towards completion
#' of the census. It lists all squirrels caught since the last census and notes
#' whether then have been entered into the current census.
#'
#' The user must identify which census they are completing. If it is an August
#' census, then this function returns all squirrels caught between May 15 and
#' August 15. If it's a May census that is being completed, then this function
#' returns all squirrels seen up to May 15 of the given year.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; year of census you're working on. Must be greater than
#'   2012 when the new census table was implemented.
#' @param census character; are you completing the may or august census?
#'
#' @return A data frame of squirrel seen since the last census.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_census_progress(con, "JO", 2014, "may") %>%
#'   head()
krsp_census_progress <- function(con, grid, year, census) {
  UseMethod("krsp_census_progress")
}

#' @export
krsp_census_progress.krsp <- function(con, grid, year,
                                      census = c("august", "may")) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE), year > 2012,
              valid_grid(grid, single = TRUE))
  census <- match.arg(census)

  year <- as.integer(year)
  grid_choice <- grid
  reverse_grid <- (grid_choice == "AG")

  # search period
  if (census == "may") {
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-05-15")
  } else if (census == "august") {
    start_date <- paste0(year, "-05-16")
    end_date <- paste0(year, "-08-15")
  } else {
    stop("Invalid census, must be may or august")
  }

  # query for most recent trapping record
  recent_query <- sprintf(
    "SELECT
      t.id, t.squirrel_id, t.date AS trap_date,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      t.locx, t.locy,
      s.sex, t.gr AS grid
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
    WHERE
      s.gr = '%s' AND
      (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE date BETWEEN '%s' AND '%s'
        GROUP BY squirrel_id);",
    grid_choice, start_date, end_date)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # get necessary tables from database
    census <- tbl(con, "census") %>%
      filter_(~ gr == grid_choice,
              ~ census_date == end_date) %>%
      select_("squirrel_id",
              census_reflo = "reflo",
              census_fate = "sq_fate") %>%
      collect()
    recent <- krsp_sql(con, recent_query)
  })
  # remove possible duplicates in trapping
  recent <- recent %>%
    group_by_("squirrel_id") %>%
    filter_(~ id == max(id)) %>%
    ungroup()
  results <- left_join(recent, census, by = "squirrel_id")

  # clean up
  results <- results %>%
    mutate_(squirrel_id = ~ as.integer(squirrel_id),
            locx = ~ loc_to_numeric(locx),
            locy = ~ suppressWarnings(round(as.numeric(locy), 1)),
            trap_date = ~ suppressWarnings(as.Date(lubridate::ymd(trap_date))),
            color_left = ~ ifelse(is.na(color_left) | color_left == "",
                                  "-", color_left),
            color_right = ~ ifelse(is.na(color_right) | color_right == "",
                                   "-", color_right),
            taglft = ~ ifelse(is.na(taglft) | taglft == "", "-", taglft),
            tagrt = ~ ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
            colours = ~ paste(color_left, color_right, sep = "/"),
            tags = ~ paste(taglft, tagrt, sep = "/"),
            sex = ~ factor(coalesce(sex, "?"),
                           levels = c("F", "M", "?")),
            in_census = ~ !(is.na(census_fate) & is.na(census_reflo))) %>%
    select_("squirrel_id", "colours", "tags",
            "sex", "grid", "trap_date",
            "locx", "locy", "in_census", "census_reflo", "census_fate") %>%
    arrange_("in_census", "locx", "locy")
  return(results)
}

plot_census <- function(census, reverse_grid = FALSE) {
  # no results
  if (nrow(rattles) == 0) {
    return("No rattles found.")
  }
  # create squirrel_id factor variable for colouring
  rattles <- rattles %>%
    mutate_(sid = ~ factor(squirrel_id)) %>%
    mutate_(id = ~ row_number()) %>%
  # middens present?
  all_data <- rattles
  middens <- rattles %>%
    filter_(~ source == "census")
  rattles <- rattles %>%
    filter_(~ source != "census")
  # create interactive plot
  popup <- function(x) {
    row <- rattles[all_data$id == x$id, ]
    paste(
      sprintf("<strong>Date:</strong> %s", row$date),
      sprintf("<strong>Colours:</strong> %s", row$colours),
      sprintf("<strong>Tags:</strong> %s", row$tags),
      sprintf("<strong>Last Trapped:</strong> %s", row$trap_date),
      sep = "<br />")
  }
  fnt <- c("Helvetica Neue", "sans-serif")
  x_ticks <- floor(min(all_data$x)):ceiling(max(all_data$x))
  y_ticks <- floor(min(all_data$y)):ceiling(max(all_data$y))
  # letter labels for x-axis
  x_labels <- data_frame(x = x_ticks + ifelse(reverse_grid, 0.2, -0.2),
                         y = ceiling(max(all_data$y)),
                         label = sapply(x_ticks, function(i) {
                           ifelse(i > 0 & i <= 26, LETTERS[i], i)
                         })
  )
  g <- ggvis::ggvis(rattles, ~x, ~y) %>%
    ggvis::layer_points(fill = ~sid, shape = ~sex,
                        key := ~id, opacity := 0.7) %>%
    # assign shapes to sexes
    ggvis::scale_nominal("shape", range = c("circle", "square", "diamond")) %>%
    # labels for x loc letters
    ggvis::layer_text(~x, ~y, text := ~label,
                      fontSize := 14, fontWeight := "bold",
                      data = x_labels) %>%
    # main x-axis
    ggvis::add_axis("x", title = "LocX", values = x_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    # dummy x-axis for title
    ggvis::add_axis("x", orient = "top", ticks = 0,
                    title = "", #sprintf("Rattles on %s in %i", grid, year),
                    properties = ggvis::axis_props(
                      labels = list(fontSize = 0),
                      title = list(fontSize = 24, font = fnt))) %>%
    # reverse x direction for agnes
    ggvis::scale_numeric("x", reverse = reverse_grid) %>%
    # left y-axis
    ggvis::add_axis("y", title = "LocY", values = y_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    # right y-axis
    ggvis::add_axis("y", orient = "right", title = "LocY", values = y_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    ggvis::hide_legend("fill") %>%
    # visualize sex with different symbols
    ggvis::add_legend("shape", title = "Sex",
                      properties = ggvis::legend_props(
                        title = list(fontSize = 20, font = fnt),
                        labels = list(fontSize = 16, font = fnt),
                        symbols = list(fill = "black", stroke = "black", size = 100)
                      )) %>%
    # popup tooltips with additional information
    ggvis::add_tooltip(popup) %>%
    ggvis::set_options(height = 650, width = 900)

  # add middens locations if requested
  g <- ggvis::layer_points(vis = g, data = middens, stroke = ~sid,
                           fill := NA, shape = ~sex) %>%
    ggvis::hide_legend("stroke")
  return(g)
}

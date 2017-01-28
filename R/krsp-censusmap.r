#' Display a map of progress towards census
#'
#' This function is to meant to be used to monitor progress towards completion
#' of the census. It shows all middens from the previous census and whether or
#' not they have been entered in this census.
#'
#' The user must identify which census they are completing. An August census
#' will be compared to the May census from that year, while a May census will be
#' compared to the previous year's August census.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; year of census you're working on. Must be greater than
#'   2012 when the new census table was implemented.
#' @param census character; are you completing the may or august census?
#' @param data logical; whether to just return the data instead of a plot.
#'
#' @return An interactive plot comparing censuses.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_censusmap(con, "JO", 2014, "may", data = TRUE) %>%
#'   head()
#' krsp_censusmap(con, "KL", 2015, "august")
krsp_censusmap <- function(con, grid, year, census, data) {
  UseMethod("krsp_censusmap")
}

#' @export
krsp_censusmap.krsp <- function(con, grid, year, census = c("august", "may"),
                                data = FALSE) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE), year > 2012,
              valid_grid(grid, single = TRUE),
              assertthat::is.flag(data))
  census <- match.arg(census)

  year <- as.integer(year)
  grid_choice <- grid
  reverse_grid <- (grid_choice == "AG")

  # census dates
  if (census == "may") {
    this_census <- paste0(year, "-05-15")
    last_census <- paste0(year - 1, "-08-15")
  } else if (census == "august") {
    this_census <- paste0(year, "-08-15")
    last_census <- paste0(year, "-05-15")
  } else {
    stop("Invalid census, must be may or august")
  }

  # get trapping record closest to census to give colours and tags
  recent_query <- sprintf(
    "SELECT
      t.id, t.squirrel_id,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      s.sex
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
      WHERE
        s.gr = '%s' AND
        (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE date <= '%s'
        GROUP BY squirrel_id);",
    grid_choice, this_census)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # get necessary tables from database
    census <- tbl(con, "census") %>%
      filter_(~ gr == grid_choice,
              ~ !is.na(reflo),
              ~ census_date == this_census | census_date == last_census) %>%
      select_("reflo", "gr", "census_date", "locx", "locy",
              "squirrel_id", fate = "sq_fate") %>%
      collect()
    recent <- krsp_sql(con, recent_query)
  })
  # remove possible duplicates in trapping
  recent <- recent %>%
    group_by_("squirrel_id") %>%
    filter_(~ id == max(id)) %>%
    ungroup()
  # process trapping
  recent <- recent %>%
    mutate_(squirrel_id = ~ as.integer(squirrel_id),
            color_left = ~ ifelse(is.na(color_left) | color_left == "",
                                  "-", color_left),
            color_right = ~ ifelse(is.na(color_right) | color_right == "",
                                   "-", color_right),
            taglft = ~ ifelse(is.na(taglft) | taglft == "", "-", taglft),
            tagrt = ~ ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
            colours = ~ paste(color_left, color_right, sep = "/"),
            tags = ~ paste(taglft, tagrt, sep = "/"),
            sex = ~ factor(coalesce(sex, "?"), levels = c("F", "M", "?"))) %>%
    select_("squirrel_id", "colours", "tags", "sex")

  # process census
  census <- census %>%
    mutate_(squirrel_id = ~ as.integer(squirrel_id),
            locx = ~ loc_to_numeric(locx),
            locy = ~ suppressWarnings(round(as.numeric(locy), 1)),
            in_census = TRUE)
  # split into censuses
  results <- full_join(
    filter_(census, ~ census_date == this_census),
    filter_(census, ~ census_date == last_census),
    by = "reflo", suffix = c("_this", "_last")) %>%
    mutate_(grid = ~ coalesce(gr_this, gr_last),
            locx = ~ if_else(in_census_this, locx_this, locx_last),
            locy = ~ if_else(in_census_this, locy_this, locy_last)) %>%
    filter_(~ !is.na(locx), ~ !is.na(locy))
  names(results) <- stringr::str_replace(names(results), "_this$", "")
  results$status <- case_when(
    is.na(results$in_census) & results$in_census_last ~ "not censused",
    results$in_census & is.na(results$in_census_last) ~ "new midden",
    results$squirrel_id == results$squirrel_id_last ~ "unchanged",
    results$squirrel_id != results$squirrel_id_last ~ "switch",
    TRUE ~ "other"
  )

  results <- results %>%
    select_("reflo", "grid", "locx", "locy",
            "squirrel_id", "squirrel_id_last", "fate", "fate_last",
            "in_census", "in_census_last")

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

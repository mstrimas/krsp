#' Map behaviour records on grid
#'
#' Create an interative plot of rattle or feeding behaviour on given grid and in
#' given year. In the plot, squirrels are colour coded, sex is distinguished by
#' different symbols, and further information is available in a tool tip on
#' mouseover. Plotting is done using \code{ggvis}, which creates an HTML object
#' that pops up in the Viewer pane in RStudio.
#'
#' Records can be further filtered to a date range using \code{from_date} and
#' \code{to_date} arguments. If both dates are provided, they must be in the
#' same year, and \code{year} is ignored. If one of the two is provided it must
#' be in the same year as the \code{year} argument.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer
#' @param from_date Date object or character representation of date in YMD format
#'   (e.g. "2016-05-25"); only show records that are on or after this date
#' @param to_date Date object or character representation of date in YMD format
#'   (e.g. "2016-05-25"); only show records that are on or before this date
#' @param data logical; if TRUE return data frame instead of plotting
#'
#' @return Displays and returns a \code{ggvis} plot of rattle locations, unless
#'   \code{data} is TRUE, in which case a data frame is returned and nothing is
#'   plotted.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_locmap(con, "JO", 2014, data = TRUE) %>%
#'   head
#' krsp_locmap(con, "KL", 2015)
#' krsp_locmap(con, "SU", 2014, from_date = "2014-04-01")
#' krsp_locmap(con, "AG", from_date = "2014-04-01", to_date = "2014-04-30")
krsp_locmap <- function(con, grid, year, from_date, to_date, data) {
  UseMethod("krsp_locmap")
}

#' @export
krsp_locmap.krsp <- function(con, grid, year, from_date, to_date,
                             data = FALSE) {
  assertthat::assert_that(missing(year) || assertthat::is.count(year),
                          missing(from_date) || length(from_date) == 1,
                          missing(to_date) || length(to_date) == 1,
                          length(grid) == 1,
                          grid %in% valid_grids())
  # convert dates
  if (!missing(from_date)) {
    from_date <- suppressWarnings(as.Date(lubridate::ymd(from_date)))
    assertthat::assert_that(!is.na(from_date))
  }
  if (!missing(to_date)) {
    to_date <- suppressWarnings(as.Date(lubridate::ymd(to_date)))
    assertthat::assert_that(!is.na(to_date))
  }

  # handle various permutations of provided dates and year
  if (missing(from_date) && missing(to_date)) {
    assertthat::assert_that(assertthat::is.count(year))
  } else if (!missing(from_date) && !missing(to_date)) {
    year <- lubridate::year(from_date)
    assertthat::assert_that(lubridate::year(to_date) == year)
  } else if (!missing(from_date) && missing(to_date)) {
    assertthat::assert_that(lubridate::year(from_date) == year)
  } else if (missing(from_date) && !missing(to_date)) {
    assertthat::assert_that(lubridate::year(to_date) == year)
  }
  assertthat::assert_that(assertthat::is.count(year),
                          all(year >= 1984),
                          all(year <= current_year()))

  year <- as.integer(year)
  grid_choice <- grid
  reverse_grid <- (grid_choice == "AG")

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # get necessary tables from database
    behaviour <- tbl(con, "behaviour") %>%
      filter(grid == grid_choice,
             year(date) == year,
             mode %in% c(1L, 4L),
             behaviour == 2L,
             detail == 1L) %>%
      select(squirrel_id, grid, locx, locy, date)
    squirrel <- tbl(con, "squirrel") %>%
      select(id, sex, colorlft, colorrt, taglft, tagrt, trap_date)
  })
  results <- inner_join(behaviour, squirrel, by = c("squirrel_id" = "id")) %>%
    collect
  # full list of rattles and corresponding squirrel info
  results <- mutate(results,
                    squirrel_id = as.integer(squirrel_id),
                    x = loc_to_numeric(locx),
                    y = suppressWarnings(round(as.numeric(locy), 1)),
                    date = suppressWarnings(as.Date(lubridate::ymd(date))),
                    colorlft = ifelse(is.na(colorlft), "-", colorlft),
                    colorrt = ifelse(is.na(colorrt), "-", colorrt),
                    taglft = ifelse(is.na(taglft), "-", taglft),
                    colorrt = ifelse(is.na(colorrt), "-", colorrt),
                    colours = paste(colorlft, colorrt, sep = "/"),
                    tags = paste(taglft, tagrt, sep = "/")) %>%
    filter(!is.na(x), !is.na(y)) %>%
    mutate(id = row_number()) %>%
    select(id, squirrel_id, x, y, grid, sex, colours, tags, date, trap_date)

  # date filtering
  if (!missing(from_date)) {
    results <- filter(results, date >= from_date)
  }
  if (!missing(to_date)) {
    results <- filter(results, date <= to_date)
  }

  # skip plotting and return data frame instead
  if (data) {
    return(results)
  }

  # create interactive plot
  popup <- function(x) {
    row <- results[x$id, ]
    paste(
      sprintf("<strong>Date:</strong> %s", row$date),
      sprintf("<strong>Colours:</strong> %s", row$colours),
      sprintf("<strong>Tags:</strong> %s", row$tags),
      sprintf("<strong>Last Trapped:</strong> %s", row$trap_date),
      sep = "<br />")
  }
  fnt <- c("Helvetica Neue", "sans-serif")
  x_ticks <- floor(min(results$x)):ceiling(max(results$x))
  y_ticks <- floor(min(results$y)):ceiling(max(results$y))
  # letter labels for x-axis
  x_labels <- data_frame(x = x_ticks + ifelse(reverse_grid, 0.2, -0.2),
                         y = ceiling(max(results$y)),
                         label = sapply(x_ticks, function(i) {
                           ifelse(i > 0 & i <= 26, LETTERS[i], i)
                         })
                         )
  g <- ggvis::ggvis(results, ~x, ~y) %>%
    ggvis::layer_points(fill = ~factor(squirrel_id), shape = ~sex,
                        key := ~id, opacity := 0.7) %>%
    # assign shapes to sexes
    ggvis::scale_nominal("shape", range = c("circle", "square")) %>%
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
                    title = sprintf("Rattles on %s in %i", grid, year),
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
  return(g)
}

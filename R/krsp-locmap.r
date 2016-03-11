#' Map behaviour records on grid
#'
#' Create an interative plot of rattle or feeding behaviour on given grid and in
#' given year. In the plot, squirrels are colour coded, sex is distinguished by
#' different symbols, and further information is available in a tool tip on
#' mouseover. Plotting is done using \code{ggvis}, which creates an HTML object
#' that pops up in the Viewer pane in RStudio.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; defaults to current year
#'
#' @return Displays and returns a \code{ggvis} plot of rattle locations.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_locmap(con, "AG", 2015)
krsp_locmap <- function(con, grid, year) {
  UseMethod("krsp_locmap")
}

#' @export
krsp_locmap.krsp <- function(con, grid, year = current_year()) {
  # assertions
  assertthat::assert_that(assertthat::is.count(year),
                          all(year >= 1984),
                          all(year <= current_year()),
                          length(grid) == 1,
                          grid %in% valid_grids())

  year <- as.integer(year)
  grid_choice <- grid

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
                    colours = paste(colorlft, colorrt, sep = "/"),
                    tags = paste(taglft, tagrt, sep = "/")) %>%
    filter(!is.na(x), !is.na(y)) %>%
    mutate(id = row_number()) %>%
    select(id, squirrel_id, x, y, grid, sex, colours, tags, date, trap_date)
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
  y_ticks <- seq(floor(min(results$y)), ceiling(max(results$y)), by = 2)
  g <- ggvis::ggvis(results, ~x, ~y, key := ~id) %>%
    ggvis::layer_points(fill = ~factor(squirrel_id), shape = ~sex) %>%
    ggvis::add_axis("x", title = "LocX", values = x_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    ggvis::scale_numeric("x", reverse = (grid == "AG")) %>%
    ggvis::add_axis("y", title = "LocY", values = y_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    ggvis::hide_legend("fill") %>%
    ggvis::add_legend("shape", title = "Sex",
               properties = ggvis::legend_props(
                 title = list(fontSize = 20, font = fnt),
                 labels = list(fontSize = 16, font = fnt),
                 symbols = list(fill = "black", stroke = "black", size = 100)
               )) %>%
    ggvis::add_tooltip(popup) %>%
    ggvis::set_options(height = 600, width = 1000) %>%
    ggvis::add_axis("x", orient = "top", ticks = 0,
             title = sprintf("Rattles on %s in %i", grid, year),
             properties = ggvis::axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0),
               title = list(fontSize = 24, font = fnt)))
  return(g)
}

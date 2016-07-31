is_integer <- function(x) {
  all(x == as.integer(x))
}

current_year <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

# functions for listing levels of factors in database
year_list <- function(con) {
  years <- NULL
  if (!missing(con)) {
    sql <- "
    SELECT DISTINCT YEAR(date) AS year
    FROM trapping
    WHERE date IS NOT NULL;
    "
    years <- tryCatch(krsp_sql(con, sql), error = function(e) NULL)
    years <- as.integer(years$year)
  }
  if (is.null(years)) {
    years <- 1984:current_year()
  }
  sort(years)
}

grid_list <- function(con) {
  grids <- NULL
  if (!missing(con)) {
    sql <- "
    SELECT gr, COUNT(*) AS n
    FROM trapping
    WHERE gr IS NOT NULL
    GROUP BY gr
    HAVING n > 5;
    "
    grids <- tryCatch(krsp_sql(con, sql), error = function(e) NULL)
    grids <- grids$gr
  }
  if (is.null(grids)) {
    grids <- c("AG", "BT", "CH", "EN", "FL", "JO", "KL",
               "LL", "LR", "RR", "SU", "SX", "UL", "UR")
  }
  sort(grids)
}

# functions for validation

valid_year <- function(year, single = FALSE) {
  check <- all(is_integer(year) & (year >= 1984) & (year <= current_year()))
  if (single) {
    check <- check && (length(year) == 1)
  }
  return(check)
}

valid_grid <- function(grid, single = FALSE) {
  check <- all(grid %in% grid_list())
  if (single) {
    check <- check && (length(grid) == 1)
  }
  return(check)
}

valid_loc <- function(x, alpha = TRUE, reflo = FALSE) {
  if (is.numeric(x)) {
    # if a numeric vector convert to character to check
    x <- as.character(x)
  } else if (!is.character(x)) {
    # if neither numeric nor character, return FALSE
    return(rep(FALSE, length(x)))
  }
  # define regular expression
  if (alpha) {
    if (reflo) {
      regex <- "^([A-Z]|(-?[0-9]{1,2}))([.][05])?$"
    } else {
      regex <- "^([A-Z]|(-?[0-9]{1,2}))([.][0-9])?$"
    }
  } else {
    if (reflo) {
      regex <- "^-?[0-9]{1,2}([.][05])?$"
    } else {
      regex <- "^-?[0-9]{1,2}([.][0-9])?$"
    }
  }
  # perform check
  grepl(regex, x, ignore.case = TRUE)
}

valid_color <- function(x) {
  grepl("^(-|([!*BGOPRWY]|B(k|K)){1,4})$", x)
}

valid_tag <- function(x) {
  grepl("^(-|[DFHJM0-9]{1}[0-9]{4})$", x)
}

valid_dna <- function(x, grid, year) {
  year <- substr(as.character(year), 3, 5)
  # 2 letters followed by 6 numbers
  val <- grepl("^[A-Z]{2}[0-9]{6}$", x)
  # first 2 letters are grid
  val <- val & substr(x, 1, 2) == grid
  # next two are year
  val <- val & substr(x, 3, 4) == year
  # can also be NA
  val <- val | is.na(x)
  val
}

krsp_filter <- function(con) {
  UseMethod("krsp_tables")
}

#' @export
krsp_filter.krsp <- function(con, year, grid, ex_unatural, ex_no_br) {
  src_tbls(con)
}

# Andrews comments
# Year range
# Grids to be included (some of the older data includes grids that we don’t typically analyze)
# Experimental animals: litter table food = 1, 4, 5, 7, 9, 10, or 13
# Un-natural deaths: FLastall f2 = 12 or 4 or 5
# Female missed breeding info: litter br = 3 or 5
# specify only first litters or all litters: litter ln = 1
# litter size code: exclude if litter lsc = 1

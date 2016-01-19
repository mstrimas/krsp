#' krsp: Working with the Kluane Red Squirrel Project Database
#'
#' This package is designed to help user bring data from the krsp database into
#' R. In addition, common database queries are standardized as R functions to
#' ensure consistency. The vignette demonstrates the use of this package:
#' \code{browseVignettes(package = "krsp")}
#'
#' @name krsp
#' @docType package
#' @importFrom dplyr %>%
NULL

# R CMD check doesn't like %>%, this fix deals with that
globalVariables(".")

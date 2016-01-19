#' Connect to the krsp database
#'
#' \code{krsp_connect} is a wrapper for \code{\link[dplyr]{src_mysql}} that
#' connects to the krsp database. By default, this function will connect to a
#' local copy of the database. The host, username, and password must be supplied
#' if a remote database is to be used.
#'
#' @param dbname database name; defaults to \code{krsp}
#' @param host,port Host name and port of krsp database. Defaults to a local
#'    instance of the database.
#' @param user,password Username and password. For a local instance of the
#'    database, these can typically be left as; however, to connect to a remote
#'    instance of the krsp database these will be user specific and must be
#'    supplied.
#' @param ... Additional arguments passed on to \code{\link[dplyr]{src_mysql}}.
#'
#' @return
#' @export
#'
#' @examples
#' db <- krsp_connect()
krsp_connect <- function(dbname = "krsp", host = "localhost", port = 0L,
                         user = "root", password = "", ...) {
  dplyr::src_mysql(dbname = dbname, host = host, port = port, user = user,
                   password = password, ...)
}

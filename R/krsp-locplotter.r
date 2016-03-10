##########
# KRSP locplotter.R
#
# This program can connect to an KRSP access database (on Windows only), query the behavior table and plot locations
# The query is based on an existing query in the db to limit to location-significant behavior.
#
#
#  On Windows, install R and the RODBC library
#  source this file
# to see a quick plot of an entire grid
#  >locplot()   - you'll be asked to select an MDB file
# to manually connect (do the steps in locplot) and use special parameters

#  >loc.df = queryLocationData(mdbconnect())
# > plotlocations(loc.df, xlim=c(5,10), ylim=c(5,10))

# then to use R's features to click on a point and see the Animal Colour use
# >identify(loc.df$xvalues, loc.df$yvalues, labels=loc.df$Colour)
# or
# > plotlocations(loc.df, xlim=c(5,10), ylim=c(5,10), idpoints=TRUE)
#
# for the Agnes grid, use the parameter reverseX to show the x-axis in reverse
# > plotlocations(loc.df, xlim=c(5,10), ylim=c(5,10), reverseX=TRUE)

#####
# developement notes
# version 1 Feb 2007 Pat Bills @ MSU
#  todo: needs more error checking!  It's possible to open a non-KRSP database
#  if the database doesn't exist no informative message is given
#  etc
#  todo: date is not part of the query but should be
#  todo: add date-ranges to the plot or query parameters

#########

mdbconnect <- function(mdbfile="") {
  # generic Access connection function
  # given a filename (or not), returns a read-only connection to mdb
  # if you don't give a file name, it will ask for one
  # requires Windows and the RODBC library
  if(!require(RODBC)) {
    warning("This works on windows only and you must install the RODBC library")
    return (FALSE)
  }

  if (mdbfile=="")  {
    if (interactive()) {
      mdbextensionfilter = matrix(c("Microsoft Access Database", "*.mdb"),1,2)
      mdbfile = choose.files(filters = mdbextensionfilter)      }

    if (length(mdbfile) == 0) {
      # raise no file given
      return(FALSE) }
  }

  if (!file.exists(mdbfile)) {
    # raise File not found
    return(FALSE)
  }

  conn = odbcConnectAccess(mdbfile)
  # error check : is  connection for real? to see if it'sok basic db test here

  # not sure if the following will work but it's an attempt to auto-close this connection when the program exits.
  # on.exit(odbcClose(conn))

  return (conn)

}

#global variable to lookup xloc code
locx.codes <-c('-10'=-10,
               '-9'= -9,
               '-8'= -8,
               '-7'= -7,
               '-6'= -6,
               '-5'= -5,
               '-4'= -4,
               '-3'= -3,
               '-2'= -2,
               '-1'= -1,
               '0'=  0,
               'A'=  1,
               'B'=  2,
               'C'=  3,
               'D'=  4,
               'E'=  5,
               'F'=  6,
               'G'=  7,
               'H'=  8,
               'I'=  9,
               'J'=  10,
               'K'=  11,
               'L'=  12,
               'M'=  13,
               'N'=  14,
               'O'=  15,
               'P'=  16,
               'Q'=  17,
               'R'=  18,
               'S'=  19,
               'T'=  20,
               'U'=  21,
               'V'=  22,
               'W'=  23,
               'X'=  24,
               'Y'=  25,
               'Z'=  26)


# function to convert xloc codes into a decimal value.  upper or lowecase

locx2value<- function(locxes) {

  # inner function used to fix up non-standard letter data
  strstandardize<-function(str) {
    toupper(sub('[[:space:]]*','',str))
  }

  # should do some errorchecking - must match regex [\-]?[0-10,A-Z]\.[0-9]
  # relies on global variable above and splits on


  locx.parts = strsplit(as.character(locxes),".", fixed=TRUE)

  # this returns a very annoying list of 2-element vectors.  [[1]] [1] A [2] 3

  # use sapply to delist into to vectors, and change while we are at it

  locx.letterpart = sapply(locx.parts, function(x) strstandardize(x[1]))
  locx.decimalpart = sapply(locx.parts,function(x) as.numeric(x[2]))

  return (as.numeric(locx.codes[locx.letterpart])  + (as.numeric(locx.decimalpart) *.1))
}



queryLocationData<- function(mdbconnection)  {

  # select statement - be sure to adjust the as.is vector below if you change the coloumns
  # this came from access, but with alternate spacing for readibility
  sql = "SELECT BehaviourData.Grid, BehaviourData.Colour, BehaviourData.LocX, BehaviourData.LocY FROM BehaviourData
  WHERE ((BehaviourData.Mode='map')
  OR (
  (BehaviourData.Mode='cas obs' Or BehaviourData.Mode='telem, nest loc')
  AND (BehaviourData.BehavNum=1)
  )
  OR (
  (BehaviourData.Mode)='cas obs' Or (BehaviourData.Mode)='telem, nest loc')
  AND (BehaviourData.BehavNum=2) AND (BehaviourData.DetailNum=1)
  );"


  ## sql columns above are Grid, colour, locx,locy
  ######## parameters to force non factor!
  # force the location columns to use character and not factor:
  asisvector = c(FALSE,FALSE,TRUE,TRUE)  # adjust this when the sql code above is changed
  loc.df = sqlQuery(mdbconnection,sql, as.is=asisvector)
  loc.df = data.frame(loc.df,"xvalues"=locx2value(loc.df$LocX),"yvalues"=as.numeric(loc.df$LocY))
  close(mdbconnection)
  return(loc.df)
}


plotlocations <- function(loc.df,xlim=c(NA,NA),ylim=c(NA,NA),reverseX=FALSE,idpoints=FALSE) {
  # produces a scatter plot of locations using a dataframe based on the KRSP database
  # colors and symbols differ for the $Colour column factor of the df
  # plot ranges can be sent as vectors xlim = c(5,6).  If NA is used, xlim=(15,NA) then NA is replaced with maximal value
  # reverseX will reverse the x-axis order

  # calc the ranges of values.  Have to use this syntax b/c when the data has NAs max(x) = NA and min(x) = NA
  minx=min(as.numeric(loc.df$xvalues[!is.na(loc.df$xvalues)]))
  maxx=max(as.numeric(loc.df$xvalues[!is.na(loc.df$xvalues)]))
  miny=min(as.numeric(loc.df$yvalues[!is.na(loc.df$yvalues)]))
  maxy=max(as.numeric(loc.df$yvalues[!is.na(loc.df$yvalues)]))

  # if no parameter sent then use the full range. (used 0 vector for defaults )

  if (is.na(xlim[1])) { xlim[1] = minx }
  if (is.na(xlim[2])) { xlim[2] = maxx }
  if (is.na(ylim[1])) { ylim[1] = miny }
  if (is.na(ylim[2])) { ylim[2] = maxy }
  # there is probably a slick way to do this with vectors xlim[is.na(xlim)] = xrange[is.na(xlim)] or something

  # reverseX used for those Grids with values going East to West instead.
  # turns out that if you reverse the xlim, the axis is reversed.  Handy
  if (reverseX) { xlim = c(xlim[2],xlim[1]) }


  # set the point color and symbol based on the loc.df$Colour factor

  # use all possible colors from the rainbow palette
  plotcolors = rainbow(nlevels(loc.df$Colour))
  # use the first 24 symbols. (pch>32 uses keyboard characters)  since n > 24, use modulo to stick within 24
  plotsymbols = c(as.numeric(loc.df$Colour)%%25)


  plot(yvalues~xvalues, data=loc.df, xlim = xlim, ylim = ylim, col=plotcolors[loc.df$Colour],pch=plotsymbols,main="Animal Observation Locations", xlab="LocX", ylab="LocY")
  # legend would go here...

  # draw grid: # light lines every onem # heavy lines every 5
  abline(h = seq(miny,maxy,by=1), v = seq(minx,maxx,by=1), col = "lightgray", lty=1, lwd=0.5)
  abline(h = seq(miny,maxy,by=5), v = seq(minx,maxx,by=5), col = "gray", lty=1)

  # see help(identify). it only works inside this
  if (idpoints) {
    identify(loc.df$xvalues, loc.df$yvalues, labels=loc.df$Colour)
  }

  return()

}


# main program
locplot<- function() {
  mdbconnection = mdbconnect() # optionally put the  mdb file name here
  loc.df = queryLocationData(mdbconnection)
  # For Mac - export the data to csv and use something like loc.df = opencsv(asisvector = c(FALSE,FALSE,TRUE,TRUE))
  plotlocations(loc.df)
  # identify(loc.df$xvalues, loc.df$yvalues, labels=loc.df$Colour)
}

# uncomment to use the identify feature to click to ID points
# mdbconnection = mdbconnect()
# loc.df = queryLocationData(mdbconnection)
# plotlocations(loc.df)
# identify(loc.df$xvalues, loc.df$yvalues, labels=loc.df$Colour)

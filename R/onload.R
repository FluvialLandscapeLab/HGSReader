.onLoad <- function(libname, pkgname) {

  op <- options()
  op.HGSReader <- list(
    HGSLineTags = list(
      # note: if a line in the input file matches more than one tag, the first
      # match take precedence!
      pm = c(titles = "TITLE", vars = "VARIABLES", zones = "ZONE", nodes = "# element", data = "#")
    ),
    # the following attributes will be added to and HGS.data.frame object.  The
    # names of each element are the attribute names.  The values will be parsed
    # and evaluated in the enviroment of the HGSData function.
    HGSDataAttributes = c(varLoc = "varLocs", modelDim = "HGSFile$modelDim"), #, vars = "names(x)"),
    HGSVariableLocations = c(nodeCentered = "NODECENTERED", cellCentered = "CELLCENTERED"),
    HGSCubeDimLabels = c(x = "X", y = "Y", z = "Z", time = "Time", var = "Var")
  )
  options(op.HGSReader)
  invisible()
}


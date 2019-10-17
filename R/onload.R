.onLoad <- function(libname, pkgname) {

  op <- options()
  op.HGSReader <- list(
    HGSLineTags = list(
      # note: if a line in the input file matches more than one tag, the first
      # match take precedence!
      pm = c(titles = "TITLE", vars = "VARIABLES", zones = "ZONE", nodes = "# element", data = "#")
    )
  )
  options(op.HGSReader)
  invisible()
}


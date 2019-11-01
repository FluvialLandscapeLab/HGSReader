#' Get data values from HGS files
#'
#' Get and manipulate output values associated with nodes or cells in an HGS
#' model run.
#'
#' @param HGSFile An S3 object of class "HGSFile" created by calling
#'   \code{\link{HGSFile}}.
#'
#' @param variables A character vector containing valid variable names. Valid
#'   variable names are stored in the HGSFile object and can be displayed with
#'   <HGSFile>$variables, where <HGSFile> is the name of the HGSFile object.
#'
#' @param blockNumber,solutionTime Specify one, but only one, of these two
#'   variables to choose the time step from which data should be retrieved. Use
#'   blockNumber to specify an index of the times steps that were output (e.g.,
#'   to get data from the first time step that was output, specify '1', for the
#'   second '2', etc.). Use solutionTime to specify the solution time from which
#'   to retrieve data. (Note: Available solutionTimes are stored in the HGSFile
#'   object and can be accessed using the \code{\link{HGSQueryBlocks}} function
#'   with "SOLUTIONTIME" as a descriptor).
#'
#' @param includeCoords Set to TRUE to include the X, Y, Z coordinates associated
#'   with the selected variable.  As an alternative, X, Y, and/or Z can be
#'   included the \code{variables} vector with other NODE centered variable
#'   names; when \code{variables} containes names of CELLCENTERED variables, set
#'   \code{includeCoords = T} to return the coordinates of cell centroids.
#'
#' @param HGSdf S3 object of class "HGS.data.frame" created by calling
#'   \code{\link{HGSGetData}}.
#'
#' @return \code{HGSGetData} returns an HGS.data.frame object of values
#'   associated with nodes (for NODECENTERED variables) or cells (for
#'   CELLCENTERED variables). There is one column for each variable requested,
#'   along with columns for the X, Y, and Z coordinates (in the spatial units
#'   determined when setting up the HGS model run) if includeCoords is TRUE.
#'
#'   Use \code{attr(<HGS.data.frame_Object>, "varLoc")} to determine if
#'   variables in the HGS.data.frame are NODECENTERED and which are
#'   CELLCENTERED.
#'
#'   \code{HGSCube} returns an "HGSCube" S3 object, which is 4-dimensional
#'   \code{array} where the first three dimensions are the node indices in the
#'   X, Y, and Z dimensions and the 4th dimension is the variable name. "NodeID"
#'   or "CellID" is a variable name added to the array automatically.  As with
#'   any \code{array} in R, the HGSCube can be sectioned using square brackets
#'   "[]". Note that "drop = F" can be a useful parameter after the dimensions
#'   specified within the square brackets to prevent collapsing with a single
#'   value of one dimension is specified.  See the help for square bracket by
#'   clicking this link: \code{\link{[}}
#'
#'   The HGSCube object represents all output for a single model timestep.
#'   HGSCube inherits from HGSArray.  When HGSCube is sectioned with square
#'   brackets, it becomes simply an HGSArray object.
#'
#' @export
HGSGetData = function(HGSFile, variables, blockNumber = NULL, solutionTime = NULL, includeCoords = F) {
  # few checks of parameters
  if(is.null(solutionTime) && is.null(blockNumber)) stop("You must specify either the solution time or the block number associated with the data retrieval.")
  if(is.null(blockNumber)) {
    blockNumber = which(sapply(HGSFile$blocks, function(x) x$SOLUTIONTIME == solutionTime))
    if(length(blockNumber) > 1) stop("Can't determine blockNumber.  SOLUTIONTIME = ", solutionTime, " for more than one block in the specified HGSFile.")
    if(length(blockNumber) == 0) stop("Can't determine blockNumber.  SOLUTIONTIME = ", solutionTime, " is not associated with any block in the specified HGSFile.")
  }
  if(length(blockNumber)!=1) stop("Only one blockNumber can be specified.")
  if(blockNumber > length(HGSFile$blocks)) stop("You requested blockNumber ", blockNumber, ", but there are only ", length(HGSFile$blocks), "in the specified HGSFile.")
  dm = HGSFile$blocks[[blockNumber]]$DATAMAP
  badVars = !(variables %in% row.names(dm))
  if(any(badVars)) stop("The following variables were requested by not found in the specified block: '", paste0(variables[badVars], collapse = "', '"), "'")

  varDm = dm[variables,]
  varLocs = unique(varDm$varLocation)
  if(length(varLocs) > 1) {
    stop("Requested variables are not at the same model location.\n", paste0("    '", rownames(varDm), "' is ", varDm$varLocation, "\n"))
  }

  if(includeCoords) variables = variables[!(variables %in% c("X", "Y", "Z"))]

  requestedData =
    as.data.frame(
      structure(
        lapply(
          variables,
          function(v) {
            skip = dm[v, "skip"]
            nlines = dm[v, "nlines"]
            scan(file = HGSFile$fileInfo$path, skip = skip, nlines = nlines, quiet = T)
          }
        ),
        names = variables
      )
    )

  if(includeCoords) {
    # get the XYZ location of each node
    XYZs = HGSGetData(HGSFile, c("X", "Y", "Z"), blockNumber = blockNumber)
    # if they are cell centered, get the average XYZ for each cell
    if (varLocs == "CELLCENTERED") {
      XYZs =
        apply(
          HGSF$elementNodes,
          1,
          function(ids) {
            apply(XYZs[ids,], 2, mean)
          }
        )
      XYZs = structure(data.frame(t(XYZs)), names = c("X", "Y", "Z"))
      # NODECENTERED and CELLCENTERED are the only expected locations; throw and
      # error if another location is detected.
    } else if(varLocs != "NODECENTERED") {
      stop("Unexpected variable location: ", VarLocs, ".  If this error is repeatable, contact the package developer.")
    }
    # Bind XYZ data to requested data
    requestedData = cbind(XYZs, requestedData)
  }

  x = structure(requestedData, class = c("HGS.data.frame", class(requestedData)))

  moreAttrs = getOption("HGSDataAttributes")
  for (i in 1:length(moreAttrs)) {
    attr(x, names(moreAttrs)[i]) = eval(parse(text = moreAttrs[i]))
  }
  x
}

#' Convert HGSArray to a data.frame
#'
#' Create a data.frame containing values from an HGSArray.
#'
#' @param x An HGSArray S3 object.
#'
#' @return A data.frame where columns represent variables from the HGSArray.
#'   Column names are variable names.  A "NodeID" or "CellID" variable in the
#'   HGSArray, if present, is not included in the data.frame, but values are
#'   used as row names the resulting data.frame.
#'
#' @export
as.data.frame.HGSArray = function(x) {
  validDimNames = names(dimnames(x)) %in% c("X", "Y", "Z", "var")
  if(!all(validDimNames)) stop("Valid dimension labels are 'X', 'Y', 'Z', and 'var'.  The following labels are unexpected: '", paste0(names(dimnames(x))[!validDimNames], collapse = "', '"), "'.")

  # determine the variables in the HGSArray from the "var" dimension, if present, or the "var" attribute
  varDim = which(names(dimnames(x)) == "var")
  if(length(varDim) == 0) {
    vars = attr(x, "var")
  } else {
    vars = dimnames(x)[["var"]]
  }

  # make a data frame from the array with a column for each var.
  result =
    structure(
      as.data.frame(
        array(
          x,
          dim = c(length(x)/length(vars), length(vars))
        )
      ),
      names = vars
    )

  # copy the HGSDataAttributes from the input HGSArray.
  # attributes(result) = c(attributes(result), attributes(x)[names(getOption("HGSDataAttributes"))])

  # determine if one of the variables in the HGSArray is a NodeID or CellID, and
  # if so, move the IDs to the rownames of the dataframe.
  IDVar = which(vars %in% c("NodeID", "CellID"))
  if(length(IDVar) > 0 ) {
    rownames(result) = as.integer(result[[IDVar]])
    result[IDVar] = NULL
  }

  # class(result) = c("HGS.data.frame", class(result))
  result
}

#' @rdname HGSGetData
#' @export
HGSCube = function(HGSdf) {
  cellCentered = (attr(HGSdf, "varLoc") == "CELLCENTERED")
  # subtract 1 from all model dimensions if CELLCENTERED data
  dims = attr(HGSdf, "dims") - ifelse(cellCentered, 1, 0)
  # add dimensions for variable names
  dims = c(dims, var = ncol(HGSdf)+1)
  # add names for the variables, including "NodeID" or "CellID"
  dimNames =
    list(
      X = 1:dims["X"],
      Y = 1:dims["Y"],
      Z = 1:dims["Z"],
      var = c(ifelse(cellCentered, "CellID", "NodeID"), names(HGSdf))
    )
  # convert data.frame to array with dimensions X, Y, Z, vars.
  x = structure(
    array(c(attr(HGSdf, "row.names"), unlist(HGSdf, use.names = F)), dim = dims, dimnames = dimNames),
    class = c("HGSCube", "HGSArray")
  )
  attributes(x) = c(attributes(x), attributes(HGSdf)[names(getOption("HGSDataAttributes"))])
#  attr(x, "vars") = dimnames(x)[["var"]]
  x
}

#' @export
`[.HGSCube` = function(x, ...) {
  class(x) = class(x)[class(x) != "HGSCube"]
  NextMethod()
}

#' @export
`[.HGSArray` = function(x, ...) {
  # perform the `[` fuction.
  result = NextMethod()
  if(is.null(dim(result))) return(result)


  # # if there is no dimension labeled "var"...
  # if(length(varDim)==0) {
  #   # ... there can't be a parameter that represents the variables to keep in
  #   # the `[` function.
  #   varSpec = NULL
  # # otherwise...
  # } else {
  #   # get the specification (which might be an empty symbol, so leave it is a list of length == 1)
  #   varSpec = as.list(match.call())[[varDim + 2]]
  #   # check for an empty symbol...
  #   if(!missing(varSpec)) {
  #     # otherwise, evaluate the varspec so that we have a vector of some sort to
  #     # work with.  This converts, e.g., variable names that might be passed to
  #     # their values.
  #     varSpec = eval(as.list(match.call())[[varDim + 2]])
  #     # if we have a numeric varSpec, convert it to the corresponding names from
  #     # dimnames.
  #     if(mode(varSpec) == "numeric") varSpec = dimnames(x)[["var"]][varSpec]
  #   }
  # }

  class(result) = class(x)
  attributes(result) = c(attributes(result), attributes(x)[names(getOption("HGSDataAttributes"))])

  # if the array passed in has a "var" dimension...
  if(!("var" %in% names(dimnames(result)))) {
    # Check to see if "var" dimension has been lost in result.  If so, there
    # must have been a single variable specified by the user in the call to `[`.
    # To keep track of that variable, we want to store that variable as a "var" attribute.
    if("var" %in% names(dimnames(x))) {
      # So determine which dimension if the "var" dimension of the HGSArray.
      varDim = which(names(dimnames(x)) == "var")
      # get the parameter that specified the variable to keep...
      varSpec = eval(as.list(match.call())[[varDim + 2]])
      # if that specification is numeric, convert it to a variable name...
      if(mode(varSpec) == "numeric") varSpec = dimnames(x)[["var"]][varSpec]
      # and store the variable name in the attributes.
      attr(result, "var") = varSpec
    } else {
      # if there was no "var" dimension in the original array, there must be a
      # var attribute, so copy it.
      attr(result, "var") = attr(x, "var")
    }
  }
  # # Now we need to update the "vars" attribute. If result still has a "var"
  # # dimension, the "vars" attribute is just the names of the "var" dimension.
  # if("var" %in% names(dimnames(result))) {
  #   attr(result, "vars") = dimnames(result)[["var"]]
  # } else {
  #   # if the varSpec is missing, the vars attributes just stays the way it was.
  #   # otherwise, if a varSpec was specified, set the "vars" attribute to the varSpec.
  #   if(!missing(varSpec)) attr(result, "vars") = varSpec
  # }
  result
}

#' @export
print.HGSArray = function(x, ...) {
  #X,Y,Z means Z,X,Y with Z reversed
  #X,Y means Y,X with Y reversed
  #X,Z means Z,X with Z reversed
  #Y,Z means Z,Y with Z reversed
  dN = dimnames(x)
  hasVar = (names(dN)[length(dN)] == "var")
  if(hasVar && (length(dN) == 2)) {
    # get rid of attributes and class
    x = array(x, dim = dim(x), dimnames = dimnames(x))
    if(names(dimnames(x))[1] == "Z") x = x[nrow(x):1,,drop = F]
  } else if((length(dN) - hasVar) == 2) {
    if(hasVar) {
      x = aperm(x, c(2,1,3))
      x = x[nrow(x):1,,,drop = F]
    } else {
      x = aperm(x, c(2,1))
      x = x[nrow(x):1,,drop = F]
    }
  } else if ((length(dN) - hasVar) == 3) {
    if(hasVar) {
      x = aperm(x, c(3,1,2,4))
      x = x[nrow(x):1,,,,drop = F]
    } else {
      x = aperm(x, c(2,1,3))
      x = x[nrow(x):1,,, drop = F]
    }
  } else {
    stop("Unexpected number of dimensions in HGSArray object")
  }
  print(x)
}

#' @export
HGSData = function(HGSFile, variables, blockNumber = NULL, solutionTime = NULL, includeCoords = F) {
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

#' @export
HGSCube = function(source, variables, blockNumber = NULL, solutionTime = NULL, includeCoords = F) {
  UseMethod("HGSCube")
}

#' @export
as.data.frame.HGSArray = function(x) {
  validDimNames = names(dimnames(x)) %in% c("X", "Y", "Z", "var")
  if(!all(validDimNames)) stop("Valid dimension labels are 'X', 'Y', 'Z', and 'var'.  The following labels are unexpected: '", paste0(names(dimnames(x))[!validDimNames], collapse = "', '"), "'.")

  vars = attr(x, "vars")
  varDim = which(names(dimnames(x)) == "var")
  IDVar = which(vars %in% c("NodeID", "CellID"))

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

  attributes(result) = c(attributes(result), attributes(x)[names(getOption("HGSDataAttributes"))])
  if(length(IDVar) > 0 ) {
    attr(result, "vars") = attr(result, "vars")[-IDVar]
    rownames(result) = as.integer(result[[IDVar]])
    result[IDVar] = NULL
  }
  class(result) = c("HGS.data.frame", class(result))
  result
}

#' @export
HGSCube.HGSFile = function(source, variables, blockNumber = NULL, solutionTime = NULL, includeCoords = F) {
  requestedData = HGSData(source, variables, blockNumber = blockNumber, solutionTime = solutionTime, includeCoords = includeCoords)
  HGSCube(requestedData)
}

#' @export
HGSCube.HGS.data.frame = function(source) {
  cellCentered = (attr(source, "varLoc") == "CELLCENTERED")
  # subtract 1 from all dimensions if CELLCENTERED data
  dims = attr(source, "dims") - ifelse(cellCentered, 1, 0)
  dimNames = list(X = 1:dims["X"], Y = 1:dims["Y"], Z = 1:dims["Z"])
  # add dimensions for variable names
  dims = c(dims, var = ncol(source)+1)
  # add names for the variables, including "NodeID" or "CellID"
  dimNames = c(dimNames, list(var = c(ifelse(cellCentered, "CellID", "NodeID"), names(source))))
  # convert data.frame to array with dimensions X, Y, Z, vars.
  x = structure(
    array(c(attr(source, "row.names"), unlist(source, use.names = F)), dim = dims, dimnames = dimNames),
    class = c("HGSCube", "HGSArray")
  )
  attributes(x) = c(attributes(x), attributes(source)[names(getOption("HGSDataAttributes"))])
  attr(x, "vars") = dimnames(x)[["var"]]
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

  # get the additional attributes of the HGSArray object
  inputAttrs = attributes(x)[names(getOption("HGSDataAttributes"))]
  # store the class of the HGSArray object
  inputClass = class(x)
  # determine which dimension if the "var" dimension of the HGSArray.
  varDim = which(names(dimnames(x)) == "var")

  # if there is no dimension labeled "var"...
  if(length(varDim)==0) {
    # ... there can't be a parameter that represesnt the variables to keep in
    # the `[` function.
    varSpec = NULL
  # otherwise...
  } else {
    # get the specification (which might be an empty symbol, so leave it is a list of length == 1)
    varSpec = as.list(match.call())[[varDim + 2]]
    # check for an empty symbol...
    if(!missing(varSpec)) {
      # otherwise, evaluate the varspec so that we have a vector of some sort to
      # work with.  This converts, e.g., variable names that might be passed to
      # their values.
      varSpec = eval(varSpec[[1]])
      # if we have a numeric varSpec, convert it to the corresponding names from
      # dimnames.
      if(mode(varSpec) == "numeric") varSpec = dimnames(x)[["var"]][varSpec]
    }
  }

  class(result) = inputClass
  attributes(result) = c(attributes(result), inputAttrs)
  # Now we need to update the "vars" attribute. If result still has a "var"
  # dimension, the "vars" attribute is just the names of the "var" dimension.
  if("var" %in% names(dimnames(result))) {
    attr(result, "vars") = dimnames(result)[["var"]]
  } else {
    # if the varSpec is missing, the vars attributes just stays the way it was.
    # otherwise, if a varSpec was specified, set the "vars" attribute to the varSpec.
    if(!missing(varSpec)) attr(result, "vars") = varSpec
  }
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
    x = unclass(x)
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

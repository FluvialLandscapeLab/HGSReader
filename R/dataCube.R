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
#' @param blockNumbers,solutionTimes Specify one, but only one, of these two
#'   variables to choose the time step from which data should be retrieved. Use
#'   blockNumber to specify an index of the times steps that were output (e.g.,
#'   to get data from the first time step that was output, specify '1', for the
#'   second '2', etc.). Use solutionTimes to specify the solution time from
#'   which to retrieve data. (Note: Available solutionTimess are stored in the
#'   HGSFile object and can be accessed using the \code{\link{HGSQueryBlocks}}
#'   function with "solutionTimes" as a descriptor).
#'
#' @param includeCoords Set to TRUE to return X, Y, Z coordinates (in HGS model
#'   units) in addition to specified variables.
#'
#' @param asArray When TRUE, returns an "HGSArray" object.  When FALSE, returns
#'   an "HGS.data.frame" object.  See "Value," below.
#'
#' @return \code{HGSGetData} When asArray is TRUE, returns an "HGSArray" S3
#'   object, which is 5-dimensional \code{array} where the first three
#'   dimensions are the node indices in the X, Y, and Z dimensions, the 4th
#'   dimension is simulation time, and the 5th dimension consists of the
#'   requested variables. "NodeID" or "CellID" is a variable name added to the
#'   array automatically.  As with any \code{array} in R, the HGSArray can be
#'   sectioned using square brackets "[]".
#'
#'   Note that "drop = F" can be a useful parameter within the square brackets
#'   to prevent loss of an array dimension when a single value of one dimension
#'   is specified.  See the help for square bracket by clicking this link:
#'   \code{\link{[}}
#'
#'   An HGSCube object always includes all 5 dimensions.  HGSCube inherits from
#'   HGSArray.  As soon as an HGSCube is sectioned with square brackets
#'   (therefore, potentially losing at least one dimension), it becomes simply
#'   an HGSArray object.  When sectioning with square brackets results in
#'   a 1-D vector, the HGSArray class designation is lost.
#'
#'   When asArray is FALSE, returns an HGS.data.frame object of values
#'   associated with nodes (for NODECENTERED variables) or cells (for
#'   CELLCENTERED variables). There is one column for solution time,
#'   CellID/NodeID, and each variable requested, along with columns for the X,
#'   Y, and Z coordinates (in the spatial units determined when setting up the
#'   HGS model run) if includeCoords is TRUE.
#'
#'   Use \code{attr(<HGS.data.frame_Object>, "varLoc")} to determine if
#'   variables in the HGS.data.frame are NODECENTERED and which are
#'   CELLCENTERED.
#'
#' @export
HGSGetData = function(HGSFile, variables, blockNumbers = NULL, solutionTimes = NULL, includeCoords = F, asArray = T) {

  # a few checks of parameters
  if(anyDuplicated(names(HGSFile$blocks))) stop("HGSFile object contains the same solution time for more than one block.")

  if(is.null(solutionTimes) && is.null(blockNumbers)) solutionTimes = names(HGSFile$blocks)
  if (!is.null(blockNumbers)) {
    blockNumbers = as.numeric(blockNumbers)
    badBlocks = !(blockNumbers %in% 1:length(HGSFile$blocks))
    if(any(badBlocks)) stop("There are ", length(HGSFile$blocks), " blocks in the HGSFile.  The following requested blocks are not valid: ", paste0(blockNumbers[badBlocks], collapse = ", "))
    solutionTimes = names(HGSFile$blocks[blockNumbers])
  } else if (!is.null(solutionTimes)) {
    solutionTimes = as.character(solutionTimes)
    badTimes = !(solutionTimes %in% names(HGSFile$blocks))
    if(any(badTimes)) stop("The following solution times were requested but do not exist in the HGSFile: ", paste0(solutionTimes[badTimes], collapse = ", "))
  } else {
    stop("Please specify either blockNumbers or solutionTimes, not both.")
  }

  dmList = lapply(HGSFile$blocks[solutionTimes], function(x) x$DATAMAP)

  # make a list of T/F vectors saying whether a variable is in each block
  badVars = sapply(variables, function(x) sapply(dmList, function(d) !(x %in% row.names(d))), simplify = F)
  # if there are any F's in badVars
  if(any(unlist(badVars))) {
    # get the variable names that have F's
    badVarNames = variables[sapply(badVars, any)]
    # make a list of indices for each variable, representing the block from which they are missing.
    badBlocks = lapply(badVars, which)
    badBlocks = badBlocks[sapply(badBlocks, length)>0]
    badSolutionTimes = lapply(badBlocks, function(bb) names(HGSFile$blocks)[bb])
    stop("The following variables were requested but not found at the following solution time: \n",
         paste0("    '", badVarNames, "' is missing at solution time(s): ", sapply(badSolutionTimes, paste0, collapse = ', ')))
  }
  varDm = dmList[[1]][variables,]
  varLocs = unique(varDm$varLocation)
  if(length(varLocs) > 1) {
    stop("Requested variables are not at the same model location.\n", paste0("    '", rownames(varDm), "' is ", varDm$varLocation, "\n"))
  }

  if (!(varLocs %in% getOption("HGSVariableLocations"))) stop("Unexpected variable location: ", VarLocs, ".  If this error is repeatable, contact the package developer.")


  if(includeCoords) variables = variables[!(variables %in% c("X", "Y", "Z"))]
  variables = unique(variables)

  cellCentered = (varLocs == getOption("HGSVariableLocations")["cellCentered"])

  requestedData =
    lapply(
      dmList,
      function(dm) {
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
      }
    )

  if(includeCoords) {
    # get the XYZ location of each node

        # note: [,c("X", "Y", "Z")] at end of next statement is needed because
        # "NodeID" and "Time" come back as columns in the data.frame
    XYZs = HGSGetData(HGSFile, c("X", "Y", "Z"), blockNumbers = 1, asArray = F)[,c("X", "Y", "Z")]

    # if they are cell centered, get the average XYZ for each cell
    if (cellCentered) {
      XYZs =
        lapply(
          XYZs,
          function(vals) {
            # tElementNodes = t(HGSF$elementNodes)
            # means =
            #   mapply(
            #     function(b,e) mean(vals[tElementNodes[b:e]]),
            #     b = seq(1, length(tElementNodes), 8),
            #     e = seq(8, length(tElementNodes), 8)
            #   )
            means = apply(HGSF$elementNodes, 1, function(ids) mean(vals[ids]))
            #structure(data.frame(t(means)), names = c("X", "Y", "Z"))
          }
        )
    }
    # Bind XYZ data to requested data
    requestedData = lapply(requestedData, function(x) cbind(as.data.frame(XYZs), x))
    variables = c(c("X", "Y", "Z"), variables)
  }

  if (!asArray){
      if(cellCentered) {
        requestedData = Map(function(df, time) data.frame(Time = as.numeric(time), CellID = 1:prod(HGSFile$modelDim-1), df), requestedData, solutionTimes)
      } else {
        requestedData = Map(function(df, time) data.frame(Time = as.numeric(time), NodeID = 1:prod(HGSFile$modelDim), df), requestedData, solutionTimes)
      }
    #}
    requestedData = do.call(rbind, c(requestedData, list(make.row.names = F)))
    class(requestedData) = c("HGS.data.frame", "data.frame")
  } else {
    requestedData = do.call(rbind, c(requestedData, list(make.row.names = F)))

    # subtract 1 from all model dimensions if CELLCENTERED data
    modelDim = HGSFile$modelDim - ifelse(cellCentered, 1, 0)
    # add dimensions for variable names and time
    modelDim = c(modelDim, length(solutionTimes), length(variables)+1)
    names(modelDim) = getOption("HGSCubeDimLabels")[c("x", "y", "z", "time", "var")]
    # add names for the variables, including "NodeID" or "CellID"
    dimNames =
      list(
        1:modelDim[getOption("HGSCubeDimLabels")["x"]],
        1:modelDim[getOption("HGSCubeDimLabels")["y"]],
        1:modelDim[getOption("HGSCubeDimLabels")["z"]],
        solutionTimes,
        c(unname(ifelse(cellCentered, "CellID", "NodeID")), variables)
      )
    names(dimNames) = names(modelDim)
    # convert data.frame to array with dimensions X, Y, Z, vars.
    requestedData =
      array(
        c(
          #the "rep" makes a repeating list of CellIDs or NodeIDs.  Each X, Y,
          #and Z has a unique ID; the list is repped for each variable and time
          #so the array is filled properly.
          rep(1:prod(modelDim[1:3]), modelDim[4]),
          unlist(requestedData, use.names = F)
        ),
        dim = modelDim,
        dimnames = dimNames
      )
    class(requestedData) = c("HGSCube", "HGSArray")

  }

  moreAttrs = getOption("HGSDataAttributes")
  for (i in 1:length(moreAttrs)) {
    attr(requestedData, names(moreAttrs)[i]) = eval(parse(text = moreAttrs[i]))
  }
  requestedData
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

  xAttr = attributes(x)

  validDimNames = names(dimnames(x)) %in% getOption("HGSCubeDimLabels")
  if(!all(validDimNames)) stop("Valid dimension labels are '", paste0(getOption("HGSCubeDimLabels"), collapse = "', '"), "'.  The following labels are unexpected: '", paste0(names(dimnames(x))[!validDimNames], collapse = "', '"), "'.")

  # determine the variables in the HGSArray from the "Var" dimension, if present, or the "Var" attribute
  for (idx in c("var", "time")) {
    dimVarName = paste0(idx, "Dim")
    dimValName = paste0(idx, "s")
    assign(dimVarName, which(names(dimnames(x)) == getOption("HGSCubeDimLabels")[idx]))
    if(length(get(dimVarName)) == 0) {
      assign(dimValName, attr(x, getOption("HGSCubeDimLabels")[idx]))
    } else {
      assign(dimValName, dimnames(x)[[getOption("HGSCubeDimLabels")[idx]]])
    }
  }
  # make a data frame from the array with a column for each var.
  x =
    structure(
      as.data.frame(
        array(
          c(rep(as.integer(times), each = length(x)/length(vars)/length(times)), x),
          dim = c(length(x)/length(vars), length(vars) + 1)
        )
      ),
      names = c(unname(getOption("HGSCubeDimLabels")["time"]), vars)
    )

  # if NodeID or CellID column exists, convert it to integer.
  idx = which(names(x) %in% c("NodeID", "CellID"))
  if(length(idx) == 1) x[[idx]] = as.integer(x[[idx]])

  # copy the HGSDataAttributes from the input HGSArray.
  attributes(x) = c(attributes(x), xAttr[names(getOption("HGSDataAttributes"))])

  # determine if one of the variables in the HGSArray is a NodeID or CellID, and
  # if so, move the IDs to the rownames of the dataframe.
  # IDVar = which(vars %in% c("NodeID", "CellID"))
  # if(length(IDVar) > 0 ) {
  #   rownames(x) = as.integer(x[[IDVar]])
  #   result[IDVar] = NULL
  # }

  class(x) = c("HGS.data.frame", class(x))
  x
}


#' @export
`[.HGSCube` = function(x, ..., drop = T) {
  class(x) = class(x)[class(x) != "HGSCube"]
  NextMethod()
}

#' @export

`[.HGSArray` = function(x, ..., drop = T) {

  if(!drop) {
    x = NextMethod()
  } else {
    # store the attributes of x
    xAttr = attributes(x)

    # Find demensions that will be dropped, which are those where the dimension length is 1 when drop = F.
#    newAttr = attributes(unclass(x)[..., drop = F])
    newAttr = attributes(NextMethod(drop = F))
    lostDimNames = newAttr$dimnames[newAttr$dim == 1]
#    lostDimNames = sapply(lostDimNames, function(x) ifelse(is.na(as.numeric(x)), x, as.numeric(x)), simplify = F)

    # perform the `[` fuction.
#    x = x[..., drop = T]
    x = NextMethod()

    if(!is.null(dim(x))){
      attributes(x) = c(attributes(x), xAttr[!(names(xAttr) %in% names(attributes(x)))], lostDimNames)
    }
  }
  x
}

#' @export
print.HGSArray = function(x, ...) {

  dN = dimnames(x)
  if(!all(names(dN) %in% getOption("HGSCubeDimLabels"))) {
    warning("Dimension labels of the HGSArray have been changed.  HGS print formating is disabled and HGSReader functions may fail.  Recommend recreating the array.")
    NextMethod(x, ...)
  }
  nDim = length(dim(x))
  spatialDims = which(names(dN) %in% getOption("HGSCubeDimLabels")[c("x", "y", "z")])

  # create a list of all dimension indices for each dimension and a default
  # permutation for reordering the array with aperm().  These will be used
  # later.
  indices = structure(lapply(dim(x), function(m) 1:m), names = names(dN))
  if(getOption("HGSCubeDimLabels")["z"] %in% names(dN)) indices[[getOption("HGSCubeDimLabels")["z"]]] = rev(indices[[getOption("HGSCubeDimLabels")["z"]]])
  perm = 1:nDim

  # if the dims are X,Y, X,Z, or Y,Z, we want to have the second dimension print
  # in rows, so swap those dimensions...
  if(length(spatialDims) == 2) {
      perm[1:2] = c(2,1)
  # if the dims are X,Y,Z, we want Z in rows, X in columns, and Z has pages.
  } else if (length(spatialDims) == 3) {
      perm[1:3] = c(3,1,2)
  }
  # apply the perms to reorder the dimensions for printing
  x = aperm(x, perm)
  # apply the indices to reverse the Z dimension of the array.
  print(do.call(`[`, c(list(x), indices[perm], list(drop = F))))
}

#' Get data values from HGS files
#'
#' Returns the data values associated with each node in an HGS model.
#'
#' @param HGSFile An S3 object of class "HGSFile" created by calling
#'   \code{\link{HGSFile}}.
#'
#' @param variables A character vector containing valid variable names.  The
#'   valid variable names are stored in the HGSFile object and cans be displayed
#'   with <HGSFile>$variables, where <HGSFile> is the name of the HGSFile
#'   object.
#'
#' @param blockNumber,solutionTime Specify one of these two variables to return
#'   data from a particular model time step that was output from HydroGeoSphere.
#'   Use blockNumber to specify a serial number of the output block (e.g., 1, 2,
#'   3, 4, etc.).  Use solutionTime to specify the specific solution time.
#'   (Note: Available solutionTimes are stored in HGSFile objects and can be
#'   accessed using the \code{\link{HGSQueryBlocks}} function with
#'   "SOLUTIONTIME" as a descriptor).
#'
#' @param X,Y,Z The index (i.e., the node number, not the spatial coordinates)
#'   of nodes to be returned in each dimension.  Setting one or two of these
#'   parameters involkes "slicing", returning either a vector or a matrix
#'   representing an axis or a slice through the model.  See "Value" section,
#'   below, for behavior depending on how many parameters are set.
#'
#' @param includeCoords Set to T to include the X, Y, Z coordinates associated
#'   with the selected variable.  Presently, this feature only works for
#'   node-centered variables such as head and Depth2GWT.  A future release may
#'   work with
#'
#' @return Either a data.frame, a vector, or a matrix, depending on the values
#'   for X, Y, and Z.
#'
#'   HGSGetData will return a data.frame when no values are passed for X, Y, and
#'   Z (or X, Y, and Z are all equal to integer(0)).  The
#'   data.frame columns will contain the values for the variables requested,
#'   along with the x, y, and z coordinates (in spatial units used to run HGS)
#'   if includeCoords is TRUE.
#'
#' @export
HGSGetData = function(HGSFile, variables, blockNumber = NULL, solutionTime = NULL, X = integer(0), Y = integer(0), Z = integer(0), includeCoords = F) {
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
  if(any(badVars)) stop("The following variables were requested by not found: '", paste0(variables[badVars], collapse = "', '"), "'")
  if(includeCoords) variables = unique(c(c("X", "Y", "Z"), variables))

  varDm = dm[variables,]
  varLocs = unique(varDm$varLocation)
  if(length(varLocs) > 1) stop("Requested variables are not at the same model location.\n", paste0("    '", rownames(varDm), "' is ", varDm$varLocation, "\n"))

  requestedData =
    as.data.frame(
      structure(
        lapply(
          variables,
          function(v) {
            skip = dm[v, "skip"]
            nlines = dm[v, "nlines"]
            scan(file = HGSFile$fileInfo$path, skip = skip, nlines = nlines)
          }
        ),
        names = variables
      )
    )
  if(any(sapply(c(X,Y,Z), length) > 0)){
    if(length(variables) > 1) stop("Values of X, Y, and Z can't be specified if more than one variable is requested or if 'includeCoords' is TRUE.")
    elements = varLocs == "CELLCENTERED"
    nodeIDs = HGSSlice(HGSFile, X, Y, Z, elements)
    nodeIDs[TRUE] = requestedData[nodeIDs,]
    requestedData = nodeIDs
  }

  return(requestedData)
}

HGSSlice = function(HGSFile, X = integer(0), Y = integer(0), Z = integer(0), elements = F) {

  # subroutine that calculates the nodes along an axis given values of (X,Y),
  # (X,Z), or (Y,Z).  This is an internal function that can only be called from
  # within HGSSlice() because it references values from the HGSSlice
  # environment.
  axisNodes = function(dimValues) {
    if(!("Z" %in% names(dimValues))) {
      #for any axis in the Z dimension, the first node ID is (Y-1)*MaxX + X.
      #IDs then skip across layers and therefore increment by the number of
      #nodes per layer.
      nodes = seq((dimValues["Y"]-1)*HGSFile$dims["X"] + dimValues["X"], maxNode, nodesPerLayer)
    } else if(!("Y" %in% names(dimValues))) {
      #for any axis in the Y dimension, the first node ID is Z*nodesperlayer +
      #X.  IDs then skip across rows, so increment by MaxX.
      nodes = seq(nodesPerLayer*(dimValues["Z"]-1) + dimValues["X"], nodesPerLayer*dimValues["Z"], HGSFile$dims["X"])
    } else if(!("X" %in% names(dimValues))) {
      #for any axis in the X dimension, the first node ID is Z*nodesperlayer +
      #(Y-1)*X + 1.  Nodes IDs then increment by 1 for another MaxX nodes.
      nodes = nodesPerLayer*(dimValues["Z"]-1) + (dimValues["Y"]-1)*HGSFile$dims["X"] + 1:HGSFile$dims["X"]
    } else {
      stop("Crazy unexpected error.  Apparently 'X', 'Y', and 'Z' dims are specified despite error check that should catch this.  If this behavior is repeatable, please notify the developer.")
    }
    return(nodes)
  }

  # to map elements (e.g. the "cubes" in between the nodes), the algorithm is
  # identical to mapping nodes, but the X, Y, and Z dimensions of the model are
  # decremented by 1.
  if(elements) {
    HGSFile$dims = HGSFile$dims - 1
    if(any(HGSFile$dims <= 0)) stop("Model is 1D or 2D.  Therefore, 3D elements can't be mapped.")
  }
  # double check to be sure the names of the dims element are c("X", "Y", "Z")
  if(any(names(HGSFile$dims)!= c("X", "Y", "Z"))) stop("HGSAxis() can't work with this HGSFile Object because the names of the 'dims' element is not c('X', 'Y', 'Z').")
  # make a list of X, Y, and Z
  dimValues = list(X, Y, Z)
  # make sure they are numeric
  if(any(sapply(dimValues, mode) != "numeric")) stop("X, Y, and Z must be numeric.")
  # make sure they are all vectors of length 1
  paramLengths = sapply(dimValues, length)
  if(any(paramLengths>1)) stop("Values of X, Y, and Z must be vectors of length = 1")
  # make sure at least 1 and no more than 2 of X, Y, Z are specified
  specified = which(paramLengths==1)
  if(!(length(specified) %in% c(1,2))) stop("Specify X, Y, or Z for a plane arrayed in the other two dimensions.  Specify X and Y for a vertical axis, X and Z for a horizontal axis in the Y direction, or Y and Z for a horizonal axis in the X direction.")

  # make a named vector of the values specified
  dimValues = structure(unlist(dimValues[specified]), names = names(HGSFile$dims[specified]))
  # get the dimensions of the model in the specified dimensions
  maxDimValues = HGSFile$dims[specified]
  # check to be sure the requested plane or axis is not outside the model
  if(any(dimValues>maxDimValues)) stop("Requested dimenstions are outside the model.  ", paste0("'", names(dimValues), "' must be <= ", maxDimValues, collapse = "; "))
  if(any(dimValues<1)) stop("Values for X, Y, and Z can't be less than 1.")
  # calculate the total number of nodes in the model
  maxNode = prod(HGSFile$dims)
  # calcualte the total number of nodes in each layer of the model
  nodesPerLayer = prod(HGSFile$dims[c("X", "Y")])
  # if two dimensions are specified as constant, return simply a 1D vector of
  # node ids along the remaining node.
  if(length(specified) == 2) {
    nodes = axisNodes(dimValues)
  # Otherwise, one dimension is specified as constant, so create a 2D array of
  # node ids representing a plane along the unspecified dimensions.
  } else {
    # The plane is created by using sapply to call the axisNodes fuction (i.e.
    # multiple calls to the function that creates a 1D vector of nodes). Next we
    # determine the dimension that is built by multiple calls (vs. the dimension
    # along which "axisNode" returns node IDs)
    if(names(dimValues) == "X") {
      sapplyDim = "Y"
    } else {
      sapplyDim = "X"
    }
    # For each node along the sapplyDim dimension, call axisNodes.  sapply will
    # return a matrix assuming the number of nodes in the axisNodes dimension is
    # >1.
    nodes =
      lapply(
        1:HGSFile$dims[sapplyDim],
        function(v) {
          # axisNodes is a function defined within HGSSlice, above.
          axisNodes(c(dimValues, structure(v, names = sapplyDim)))
        }
      )
    # convert list of node vectors to a matrix
    nodes = do.call(cbind, nodes)
    # determine which dimensions are represented in the matrix
    varyingDims = rev(names(HGSFile$dims)[-which(names(HGSFile$dims) == names(dimValues))])
    # assign some useful lables to the matrix
    dimnames(nodes) = structure(lapply(HGSFile$dims[varyingDims], function(x) seq(1,x)), names = varyingDims)
    # invert the rows (to orient Z or Y dimension)
    nodes = nodes[nrow(nodes):1,]
  }

  return(nodes)
}


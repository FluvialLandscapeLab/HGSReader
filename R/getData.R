# Get data values from HGS files
#
# Returns the data values associated with a particular output time for node
# nodes or cells in an HGS model.
#
# @param HGSFile An S3 object of class "HGSFile" created by calling
#   \code{\link{HGSFile}}.
#
# @param variables A character vector containing valid variable names. Valid
#   variable names are stored in the HGSFile object and can be displayed with
#   <HGSFile>$variables, where <HGSFile> is the name of the HGSFile object.
#
# @param blockNumber,solutionTime Specify one, but only one, of these two
#   variables to choose the time step from which data should be retrieved. Use
#   blockNumber to specify an index of the times steps that were output (e.g.,
#   to get data from the first time step that was output, specify '1', for the
#   second '2', etc.). Use solutionTime to specify the solution time from which
#   to retrieve data. (Note: Available solutionTimes are stored in the HGSFile
#   object and can be accessed using the \code{\link{HGSQueryBlocks}} function
#   with "SOLUTIONTIME" as a descriptor).
#
# @param X,Y,Z The index (i.e., '1' for first node or cell, '2' for second,
#   etc., not the spatial coordinates) of the node or cell to be returned in
#   each dimension. Setting one or two of these parameters invokes "slicing" of
#   model output, returning either a vector representing an axis or a matrix
#   representing a slice through the model. See "Value" section, below, for
#   behavior depending on how many parameters are set.
#
# @param includeCoords Set to T to include the X, Y, Z coordinates associated
#   with the selected variable.
#
# @return Either a data.frame, a vector, or a matrix, depending on the values
#   for X, Y, and Z.  Importantly, the values of X, Y, and Z are index values
#   for nodes.  For instance, Z=1 would specify the bottom-most layer of nodes
#   (for node-centered data) or cells (for cell-centered data) in the model,
#   regardless of the actual spatial coordinates of the nodes/cells.
#
#   if X, Y, and Z are all omitted (or specified as integer(0)), the return
#   value is a data.frame of all values associated with nodes (for node-centered
#   data) or cells (for cell centered data).  Setting "includeCoords" will
#   include in the data.frame the spatial coordinates of each value.
#
#   If only one value for X, Y, or Z is specified, a matrix is returned, which
#   represents a 2D slice (plane) through the model at the specified node or
#   cell index.  If two of the three values X, Y, and Z are specified, the
#   result is a vector representing a 1D axis through the model at the specified
#   node (or cell) indicies.  Only one model variable can be requested in a
#   slice or axis.  If more than one variable is requested, or if includeCoords
#   is TRUE when X, Y, or Z is specified, an error results.
#
#   HGSGetData will return a data.frame when no values are passed for X, Y, and
#   Z (or X, Y, and Z are all equal to integer(0)).  The data.frame columns will
#   contain the values for the variables requested, along with the x, y, and z
#   coordinates (in spatial units used to run HGS) if includeCoords is TRUE.
#
# @export
#
#
# ARGH! Axis with multiple variables crashes!!!
#
HGSGetDataOld = function(HGSFile, variables, blockNumber = NULL, solutionTime = NULL, X = integer(0), Y = integer(0), Z = integer(0), includeCoords = F) {
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
      stop("Unexpected variable location: ", VarLocs, ".  Contact the package developer.")
    }
    # Bind XYZ data to requested data
    requestedData = cbind(XYZs, requestedData)
  }

  # If slicing is requested...
  if(any(sapply(list(X,Y,Z), length) > 0)){
    # get the node IDs of the slice
    nodeIDs = HGSSlice(HGSFile, X, Y, Z, cells = (varLocs == "CELLCENTERED") )
    # if there is only one column requested, the dimensions of nodeIDs will be
    # the dimensions of the return values
    if(ncol(requestedData) == 1) {
      dims = dim(nodeIDs)
      dimNames = dimnames(nodeIDs)
    # otherwise, we have to add nVariables as an additional dimension of the
    # return values
    } else {
      # get the dimensions of nodeIDs
      dims = dim(nodeIDs)
      dimNames = dimnames(nodeIDs)
      # if nodeIDs is 1D, then the length of nodeIDs is the first dimension
      if(is.null(dim(nodeIDs))) {
        dims = length(nodeIDs)
        dimNames =
          structure(
            list(as.character(1:length(nodeIDs))),
            names = c("X", "Y", "Z")[sapply(list(X,Y,Z), length) == 0]
          )
        # if we are dealing with the Z dimension, reverse it.
#        if(names(dimNames) == "Z") {
#          nodeIDs = rev(nodeIDs)
#          dimNames[[1]] = rev(dimNames[[1]])
#        }
      }
      # add the extra dimension to account for more than one variable
      dims = c(dims, ncol(requestedData))
      dimNames = c(dimNames, list(var = names(requestedData)))
    }
    requestedData =
      structure(
#        sapply(requestedData, function(x) x[nodeIDs]),
        unlist(requestedData[nodeIDs,], use.names = F),
        dim = dims,
        dimnames = dimNames
      )
#    nodeIDs[TRUE] = requestedData[nodeIDs,]
#    requestedData = nodeIDs
  }

  return(requestedData)
}

# @export
HGSSlice = function(HGSFile, X = integer(0), Y = integer(0), Z = integer(0), cells = F) {

  # axisNodes is a subroutine that calculates the nodes along an axis given
  # values of (X,Y), (X,Z), or (Y,Z).  This is an internal function that can
  # only be called from within HGSSlice() because it references values from the
  # HGSSlice environment.
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

  # to map cells (e.g. the "cubes" in between the nodes), the algorithm is
  # identical to mapping nodes, but the X, Y, and Z dimensions of the model are
  # decremented by 1.
  if(cells) {
    HGSFile$dims = HGSFile$dims - 1
    if(any(HGSFile$dims <= 0)) stop("Model is 1D or 2D.  Therefore, 3D cells can't be mapped.")
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
 #   nodes = nodes[nrow(nodes):1,]
  }

  return(nodes)
}

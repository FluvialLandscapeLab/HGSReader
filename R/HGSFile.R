#' Read HGS output file and create HSGFile object
#'
#' Returns information about an file created by HGS in the form of an HGSFile
#' object.  The HGSFile object can then be passed to other functions to return
#' HGS output in R friendly formats.
#'
#' @param x A file name to read.
#'
#' @return Returns an S3 object with a class of "HGSFile".  The object will also
#'   have a subclass, which is the text "HGSFile_" contatinated with the type of
#'   HGS output file.  So, for instance, the S3 class resulting from reading an
#'   HGS "pm" file will be c("HGSFile_pm", "HGSFile").  (To see the currently
#'   supported types, use \code{\link{HGSSupported}}
#'
#'   An HGSFile object is a list which has, at minimum, three elements:
#'
#'   \itemize{
#'
#'   \item{ \code{fileInfo} will contain the results from
#'   \code{\link{file.info}} for the HGS file that was read, at the time the
#'   HGSFile object was created.}
#'
#'   \item{\code{description} will contain the model run description provided by
#'   the HGS user when the model was run.}
#'
#'   \item{\code{taggedLines} is a data.frame with one record for each "tagged"
#'   line of the HGS file.  Tagged lines demarcate the beginning of an item of
#'   interest in the HGS data file.  For instance, "pm" files contain tags for
#'   titles, variable names, zones, data, and node IDs. To see the tags for each
#'   supported for each file type, use \code{\link{HGSTags}}.}
#'
#'   }
#'
#'   An HGSFile object may have additional elements, determined by the subclass.
#'
#'   \code{HGSFile_pm} objects will contain the following additional elements:
#'
#'   \itemize{
#'
#'   \item{\code{variables} is a vector of variable names included in output.}
#'
#'   \item{\code{elementNodeLookup} A named numeric vector telling the number of
#'   lines to skip and the number of lines to read in the original data file to
#'   determine the node IDs associated with each element of the model.  Useful
#'   with \code{\link{scan}}.  (HGS elements are, e.g., the cubes demarcated by
#'   eight nodes in the simulation space.)}
#'
#'   \item{\code{dims} A named numeric vector telling the number of nodes in the
#'   simulation space in X, Y, and Z dimensions.}
#'
#'   \item{\code{blocks} A list containing sublists describing information about
#'   each "block" in the original data file. The information available for each
#'   \code{block} can be investigated with \code{\link{HGSQueryBlocks}}.  The
#'   information stored in a \code{block}'s sublist is variable, depending on
#'   the contents of the HGS data file.  For instance, in a 'HGSFile_pm' object,
#'   there is a block for each simulation time at which the user requested
#'   output from HGS. Therefore, in a 'HGSFile_pm' object, each block has a
#'   "SOLUTIONTIME" element.  Critically, each block has a \code{DATAMAP}
#'   element, containing a data.frame with one row for each variable that can be
#'   extracted from the HGS data file.  Model variable names are the row names
#'   of the \code{DATAMAP} data.frame. The columns tell how many lines to skip
#'   and how many lines to read in the datafile in order to retrieve the data
#'   for each variable from the \code{block}.  That said, rather than try to
#'   read the \code{DATAMAP}s directly, use the \code{\link{HGSGetData}}
#'   function, which uses the \code{DATAMAP}s correctly to retrieve data for
#'   specific variables and simulation times from an HGS output file.}
#'
#'   }
#'
#' @examples
#'   # requires name of a legetimate pm file
#'   HGSF <- HGSFile("a_File_Name.pm.dat")
#'
#' @seealso \code{\link{HGSQueryBlocks}}
#'
#' @export
HGSFile = function(x = file.choose()) {
  #get the file types defined in .onload().
  fileTypes = names(getOption("HGSLineTags"))
  # regular expressions to determine the end of the file name for all of the file types.
  fileTypesRE = paste0(".", fileTypes, ".dat$")
  # make sure the file name suggests it is a supported HGS file
  fileType = which(mapply(grepl, pattern = fileTypesRE, x = x))
  if(length(fileType) == 0) stop("The file name", x, " isn't a recognized type.  Recognized types are: '", paste0(gsub("\\$", "", fileTypesRE), collapse = "', '"), "'")

  #HGSReadFile(x) is a generic method that dispatches on the file type.  So,
  #assign the class of the file name as a file type.
  x = structure(x, class = fileTypes[fileType])
  taggedLines = HGSReadFile(x)

  #return the file description, with always includes fileInfo, description, and
  #taggedLines, as well as the result of HGSFileBody, which is also a generic
  #method that dispatches on classes associated with file type names.
  structure(
    c(
      list(
        fileInfo = c(list(path = normalizePath(x)), as.list(file.info(x))),
        description = paste(trimws(taggedLines$text[taggedLines$tag == "titles"]), collapse = " "),
        taggedLines = taggedLines
      ),
      HGSFileBody(x, taggedLines)
    ),
    class = c(paste0("HGSFile_", fileTypes[fileType]), "HGSFile")
  )
}

# generic methods

### READ TAGGED LINES
HGSReadFile = function(x) {
  UseMethod("HGSReadFile")
}

HGSReadFile.pm = function(x) {
  getTaggedLines(x, getTags(class(x)))
}

### PROCESS TAGGED LINES
HGSFileBody = function(x, taggedLines) {
  UseMethod("HGSFileBody")
}

HGSFileBody.pm = function(x, taggedLines) {
  #names of columns that should be included in the DATAMAP; note that the first
  #to values must be for "skip" and "nline" in "scan()" function.
  mapColumns = c(skip = "line", nlines = "nlines", text = "text")
  #line number of End Of File being read
  fileEnd = attr(taggedLines, "lineCount") + 1
  #tags used to generate taggedLines
  tags = names(attr(taggedLines, "tags"))
  #create a named list of rows in taggedLines data.frame for each type of tag
  subset = structure(lapply(tags, function(x) which(taggedLines$tag == x)), names = tags)
  #create the variable names from the tagged line that lists variable names
  vars = c(strsplit(taggedLines$text[subset$vars], ",")[[1]])
  #for the data lines (subset$data) in taggedLines, make a lookup vector to
  #associate the text descriptor of each line with a variable name. The first
  #length(vars) lines in subset$data have the text that correspond to the
  #variable names.
  varLookup = structure(vars, names = taggedLines$text[subset$data][1:length(vars)])

  #calculate the number of lines in each section of the file, based on the tagged lines
  taggedLines$nlines = c(taggedLines$line[2:nrow(taggedLines)], fileEnd) - taggedLines$line - 1
  #grab the data necessary to lookup the node ids for each element.  This gets
  #the "line" and "nlines" values for the "nodes" record in taggedLines,
  #converts them to a list, and renames the values according the the names in mapColumns.
  elementNodes =
    structure(
      as.integer(taggedLines[subset$nodes,mapColumns[1:2]]),
      names = names(mapColumns[1:2])
    )

  #create a defaultDataMap from the first block of lines in subset$data.  Again,
  #the first lenght(vars) lines in subset$data contain a complete data set.
  #This will be used as a base data map for all blocks in subset$zone because
  #any variables that don't change are not rewritten by HGS
  defaultDataMap = taggedLines[subset$data,][1:length(vars), mapColumns]
  row.names(defaultDataMap) = varLookup[defaultDataMap$text]
  names(defaultDataMap) = names(mapColumns)
  defaultVarLocation = rep("NODECENTERED", nrow(defaultDataMap))


  # caluculate the contents of each block (except for the DATAMAP) by parsing
  # the subset$zones lines
  blocks = getVarLists(taggedLines$text[subset$zones])

  # add the DATAMAPS to the blocks
  blocks = mapply(
    function(b, bStart, bEnd) {
      dataMap = defaultDataMap
      #extract subset$data from taggedLines
      taggedDataLines = taggedLines[subset$data, mapColumns]
      #determine which lines from subset$data are in the current zone; these are
      #the lines that need to be updated in defaultDataMap
      updatedData = taggedDataLines[taggedDataLines$line > bStart & taggedDataLines$line < bEnd,]
      #determine which variables need to be updated; other variables will not be changed in defaultDataMap
      varsToUpdate = varLookup[updatedData$text]
      #update the default dataMap
      dataMap[varsToUpdate,] = updatedData
      #append the varlocations, based on the VARLOCATION values in the block
      #(passed in as b argument)
      dataMap$varLocation = defaultVarLocation
      for (i in 1:length(b$VARLOCATION)) {
        dataMap$varLocation[ b$VARLOCATION[[1]] ] = names(b$VARLOCATION)[i]
      }
      #return the block, updated so that datamap is the first element
      c(list(DATAMAP = dataMap), b)
    },
    b = blocks,
    # bStart is a vector of lines numbers representing the start of each block
    bStart = taggedLines$line[subset$zones],
    # bEnd is a vector of lines number representing the start of the next block
    bEnd = c(taggedLines$line[subset$zones][-1], fileEnd),
    SIMPLIFY = F
  )

  # The following code determines the dimensions of the HGS space, assuming it's
  # rectangular.  In the rectangular mesh, all nodes are given a serial ID
  # starting at 1.  In plan view (x = left-right, y = up-down), ID=1 is in the
  # lower left.  From there IDs are assigned sequentially along the x
  # dimensions.  When the end of the x dimension is reached, the numbering moves
  # one row in the y dimension and numbers again along the x dimension. When all
  # nodes in a layer have an ID, the z dimension is incremented and number
  # continues as with the prior layer.
  #
  # Elements (the boxes defined by nodes) are assigned IDs similarly.  Starting
  # in the lower left, working along the x dimension, then the y dimension, then
  # the z dimension.
  #
  # Each element has 8 nodes.  Node order within each element starts at the min
  # x, y, and z value (first node), moving in the x direction to the second
  # node, then the y direction to the third node, then the negative x direction
  # to the forth node.
  #
  # If you map all this out, the node ID of the 4th node of the first element is
  # xDim + 1.  The node ID of the 5th node of the first element is yDim + 1.
  # Finally, the number of elements is equal to the number of rows in the Nodes
  # data block.  In each layer, there are (xDim - 1) * (yDim - 1) elements.  So
  # the number of elements in the z dimensions is nElements/((xDim-1)*(yDim-1)).
  # Finally, the number of nodes in the zDim is 1 more than the number of
  # elements in the zDim.  WHEW!!!
  FirstElementNodes = scan(file = x, skip = elementNodes[1], n=8, quiet = T)
  xDim = FirstElementNodes[4] - 1
  yDim = (FirstElementNodes[5] - 1)/xDim
  zDim = unname(elementNodes[2]/((xDim-1)*(yDim-1)) + 1)

  #return a list to be appended to the default file description (including fileinfo, description, and tagged lines)
  list(variables = vars, elementNodeLookup = elementNodes, dims = c(X = xDim, Y=yDim, Z = zDim), blocks = blocks)
}

getTaggedLines = function(filepath = file.choose(), tags) {
  #set up vectors that will form a data.frame in the end
  lines = character(0)
  lineTags = character(0)
  lineNums = integer(0)

  # line counter
  counter = 0

  #create a read only file connection
  con = file(filepath, "r")

  #read the file one line at a time
  while ( TRUE ) {
    line = readLines(con, n = 1)
    #stop at end of file
    if (length(line) == 0 ) {
      break
    }
    counter = counter + 1
    # for debug
    #    x<<-counter
    #    lineText<<-line

    # see if the line starts with any of the line tags
    isTagged = sapply(paste0("^", tags), grepl, x = line)
    # if so, update the vectors of the data.frame
    if(any(isTagged)) {
      #if more than one tag matches, take the first one.
      tagIdx = which(isTagged)[1]
      #remove quotes from the line
      line = gsub("\"", "", line)
      #trim white space
      line = trimws(line)
      #get the portion of the tag in front of the first space in the tag
      tagToRemove = regmatches(tags[tagIdx], regexpr(" ", tags[tagIdx]), invert = T)[[1]][1]
      #trim off the tagToRemove plus any following spaces or equal signs
      line = gsub(paste0("^", tagToRemove, "[ =]*"), "", line)
      #store the line, line number, and tag name
      lines = c(lines, line)
      lineNums = c(lineNums, counter)
      lineTags = c(lineTags, names(tags)[tagIdx])
    }
  }
  close(con)

  # create and return the data.frame of tagged lines, with the lineCount and tags used to create the data.frame as attributes.
  structure(data.frame(line = lineNums, tag = lineTags, text = lines, stringsAsFactors = F), lineCount = counter, tags = tags)
}


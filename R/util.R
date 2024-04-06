#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @export
print.HGSFile = function(x) {
  cat("HGS file with ", length(x$blocks), " data block", ifelse(length(x$blocks)>1, "s", ""), "\n", sep = "")
  cat("  Source file: ", x$fileInfo$path, "\n")
  cat("  Last modified: ", paste(x$fileInfo$mtime) , "\n", sep = "")
  cat("  Variables: '", paste(x$variables, collapse = "', '"), "'\n", sep = "")
  cat("  Model dimensions: ", paste0(names(x$modelDim), " = ", x$modelDim, collapse = ", "), " nodes", sep = "")
}

# gets the value that tag key rows in an HGS output file.  These tags are defined in .onload().

getTags = function(fileType, tagNames = NULL) {
  #get the tags for all file types and make sure the specified file type is legal.
  tags = getOption("HGSLineTags")
  if(!(fileType %in% names(tags))) stop("'", fileType, "' is not a recognized HGS file type. Recognized types are: '", paste0(names(tags), collapse = "', '"), "'")

  #get the tags for the specified file type
  tags = tags[[fileType]]
  #if no tags are specified, return all the tags
  if(is.null(tagNames)) return(tags)
  #if tags are specified, make sure the names are valid
  badNames = !(tagNames %in% names(tags))
  if(any(badNames)) stop("The following names are not associated with HGSReader tags: '", paste0(tagNames[badNames], collapse = "', '"), "'")
  #return the requested tags.
  tags[tagNames]
}

#' Display data associated with output blocks in an HGSFile.
#'
#' When HGS is run, the user requests output at specific solution times for key
#' data from the model.  For each requested time, there is a block of data in
#' the output file.  This command displays critical descriptors for each block
#' of data.
#'
#' @param HGSFile An S3 object of class "HGSFile" created by calling
#'  \code{\link{HGSFile}}.
#'
#' @param descriptors Either NULL, or a character vector of descriptor names.  See
#'  documentation of return value, below.
#'
#' @return When \code{descriptors} is \code{NULL}, return value is a \code{list} of
#' descriptor names available for each block. When \code{descriptors} is a single
#' descriptor name, return value will be a list of values, one value for each
#' block.  When \code{descriptors} is a vector of more than one descriptor name,
#' the return value is a \code{list} of \code{lists} of descriptor values.
#'
#' @examples
#' HGSF <- HGSFile("a_File_Name.pm.dat") # requires name of a legetimate pm file
#'
#' # returns the names of the information in each query block
#' HGSQueryBlocks(HGSF)
#'
#' # returns the simulation time for each block
#' HGSQueryBlocks(HGSF, "SOLUTIONTIME")
#'
#' # returns the simulation times for each simulation block followed by the variable locations for each simulation block
#' HGSQueryBlocks(HGSF, c("SOLUTIONTIME", "VARLOCATION"))
#'
#' @export
HGSQueryBlocks = function(HGSFile, descriptors = NULL) {
  if(is.null(descriptors)) {
    result = lapply(HGSFile$blocks, names)
  } else {
    result =
      sapply(
        descriptors,
        function(x) sapply(HGSFile$blocks, `[[`, x, simplify = F),
        simplify = F
      )
#    if(length(descriptors)>1) {
#      result = lapply(HGSFile$blocks, `[`, descriptors)
#    } else {
#      result = lapply(HGSFile$blocks, `[[` , descriptors)
#    }
  }
  return(result)
  #structure(result, names = paste0("Block_", 1:length(HGSFile$blocks)))
}


#' See supported file types and associated tags.
#'
#' These functions provide some basic information about the functionality of the HGSReader package.
#'
#' HGS outputs several different file types, differentiated by the extensions of
#' the file names.  Each file type has a set of "tags" that occur in the file to
#' demarcate the beginning of sections of the file that contain different types
#' of information.  These functions show which file types are supported and which
#' tags can be processed in each file type.
#'
#' @param quiet When set TRUE, will suppress printing of information to the
#'   screen.
#'
#' @return \code{HGSSupported} returns a character vector of file extensions
#'   (used by HGS to indicate the type of file).  If a file extension is in this
#'   vector, the associated HGS file type can be read by this package.
#'
#'   \code{HGSTags} returns a named list of character vectors where the names
#'   are the supported file types and the associated vectors contain the tags
#'   that are processed for each file type, and the data type associated with each tag.
#'
#'   Values are returned invisibly (without printing, see
#'   \code{\link{invisible}}).
#'
#' @examples
#'   # See supported file types
#'   HGSSupported()
#'
#'   # See tags that are processed in each file type
#'   HGSTags()
#'
#' @export
HGSSupported = function(quiet = F) {
  if (!quiet) cat(paste0("The currently supported HGS file types are: '", names(getOption("HGSLineTags")), collapse = "', '", "'"))
  invisible(names(getOption("HGSLineTags")))
}

#' @rdname HGSSupported
#' @export
HGSTags = function(quiet = F) {
  lineTagList = getOption("HGSLineTags")
  if (!quiet) {
    for(i in 1:length(lineTagList)) {
       cat(paste0("For file type '", names(lineTagList[i]), "', the tags are: \n  ", paste0(names(lineTagList[[i]]), " = '", lineTagList[[i]], collapse = "'\n  "), "'\n"))
    }
  }
  invisible(lineTagList)
}

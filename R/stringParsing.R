# Parse comma-separated "varname = value" pairs
#
# Convert a comma-separated strings of "varname = value" pairs (e.g., "a=2,
# b=apple, c=3.54E-02") to named lists.
#
# @param text A vector containing strings of comma separated "varname = value"
#   pairs.  Will be coersed to \code{charater}
#
# @param x A vector of "varname = value" pairs. Will be coersed to
#   \code{character}
#
# @return \code{getVarLists} returns a list equal in length to \code{text}.
#   Each element of the list is equal to the value returned by
#   \code{getVarValues(strsplit(text[i], ","))}.
#
#   getVarValues returns a named list equal in length to \code{x}.  The list
#   contains each value from \code{x}, named with the varnames from \code{x}.
getVarLists = function(text) {
  text = as.character(text)
  #replace all of the commas outside of brackets with
  text = gsubOutsideBrackets(",", "||,||", text)
  #split at the location of the commas outside of the brackets to create "VARNAME = VALUE" strings.
  vars = strsplit(text, "||,||", fixed = T)
  #parse each resulting "VARNAME = VALUE" string
  lapply(vars, getVarValues)
}

# @rdname getVarLists
getVarValues = function(x) {
  x = as.character(x)
  #make sure there is an equals sign in each element of x
  hasEquals = grepl("=", x)
  if(!all(hasEquals)) stop("Equals sign ('=') not found in variable definition (delimited by commas) in:\n     ", paste0(x, collapse = ", "))
  #split each element of x at the first equals sign and trim whitespace
  chopped = lapply(regmatches(x, regexpr("=", x), invert = T), trimws)
  #pars out the variable names (first elements of each list) and values (second elements of each list)
  varNames = sapply(chopped, `[`, 1)
  varValues = structure(lapply(chopped, `[`, 2), names = varNames)
  #convert any character representation of a number to a numeric.
  canBeNumber = sapply(varValues, function(x) grepl("^[1234567890.-]+(E[+-][1234567890]+)?$",x))
  varValues[canBeNumber] = lapply(varValues[canBeNumber], as.numeric)
  #strip quotes from around any non-numeric values
  #  varValues[!canBeNumber] = gsub("(^\")|(\"$)", "", varValues[!canBeNumber])

  # call formatZoneVariable for each variable while setting the class of the
  # text to the name of the variable, for dispatch (formatZoneVariable is a
  # generic function that dispatches on S3 classes, where the class is the
  # variable name.)
  mapply(
    function(x, nm) unclass(formatZoneVariable(structure(x, class = nm))),
    varValues,
    names(varValues),
    SIMPLIFY = F
  )
}



# Pattern replacement outside of bracketed text
#
# Identical to \code{gsub()} except replacement occurs only outside of any
# pairs of square brackets in the text.  If brackets are not well-formed (e.g.,
# if opening or closing brackets don't match in pair or are nested), this
# fuctions throws an error.
#
# @param pattern regular expression to match (see \code{pattern} parameter in
#   \code{gsub})
# @param replacement replacement text (see \code{replacement} parameter in
#   \code{gsub})
# @param pattern x text to be searched (see \code{x} parameter in \code{gsub})
# @param ... other parameters passed to \code{gsub}
#
# @return returns x, coerced to character, but with any text outside of
#   brackets that matches \code{pattern} replaced by \code{replacement}.  Text
#   in brackets remains unchanged.
gsubOutsideBrackets = function(pattern, replacement, x, ...) {
  x = as.character(x)
  #regular expression for text within brackets
  bracketRE = "\\[.*?\\]"
  #find the bracketed text locations
  matchData = gregexpr(bracketRE, x)
  #return unbracketed text and be sure there are no stray brackets in it...
  #Stray brackets indicate nest or unclosed brackets.  Note, "[][]" is a regular
  #expression to search for "]" or "["
  unBracketedText = regmatches(x, matchData, invert = T)
  stillHasBrackets = grepl("[][]", unBracketedText)
  if(any(stillHasBrackets)) stop("Nested or unpaired brackets not handled, but present in:\n", paste0("      ", x[stillHasBrackets], "\n"))

  #create new text to replace unBracketed text
  replacementText = lapply(unBracketedText, gsub, pattern = pattern, replacement = replacement, ...)
  #insert new text into original text
  mapply(
    function(p, r, x) {
      for(i in 1:length(p)) {
        x = sub(p[i],r[i], x, fixed = T)
      }
      return(x)
    },
    p = unBracketedText,
    r = replacementText,
    x = x
  )
}

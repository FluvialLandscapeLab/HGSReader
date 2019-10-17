# Most variables found (e.g., on "ZONE" lines in .pm.dat files) are set = to a
# specific value.  e.g.
#
# SOLUTIONTIME = 1.3456E5
#
# Some are more complex, e.g.:
#
# VARLOCATION = ([         4,      7,      8,      9]=CELLCENTERED)
#
# to deal with a more complex variable, simply write a routine called
# formatZoneVariable.<VARNAME> and have the function return a value that should
# be included as an element in the HGSFile list, associated with the VARNAME.
# The funtion should expect "x" to be a named character vetor with one element.
# The name of the element is the variable name.  The value contained in the
# vector is the text from the HGS file associated with the variable name.

formatZoneVariable = function(x) {
  UseMethod("formatZoneVariable")
}

formatZoneVariable.default = function(x) {
  return(x)
}

formatZoneVariable.VARSHARELIST = function(x) {
  #strip the leading and trailing brackets and convert to a vector.
  x = gsub("[][()]", "", x)
  as.numeric(unlist(strsplit(x, ",")))
}

formatZoneVariable.VARLOCATION = function(x) {
  #strip the leading and trailing parentheses.
  x = gsub("[()]", "", x)
  #split at the "=" sign
  x = unlist(strsplit(x, "="))
  #get rid of the leading and trailing brackets
  x[1] = gsub("[][]", "", x[1])
  #create a
  structure(list(as.numeric(unlist(strsplit(x[1], ",")))), names = x[2])
}

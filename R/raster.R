#' Variant of which.max
#'
#' @description helper function to find which raster layer from a stack contains data
#'
#' @param x vector.
#'
#' @return The ordinal position of the maximum element in x. Unlike `which.max`,
#' wmax returns NA if no element of x is positive definite.
wmax = function(x) {
  w = which.max(x) * ifelse(max(x)==0,NA,1)
  if (length(w)) w else NA
}


#' Save a raster to file.
#'
#' @description saves raster object to file. By default, asks permission to overwrite an existing file.
#'
#' @param x raster* object to save.
#' @param path path in which to save.  May be relative or absolute.
#' @param fname character vector, length one.
#' Base of file name (not including the extension, which is determined by file type).
#' If no file name is provided, default behaviour is to use the object name as the file name.
#' @param type file type to save.  Default is ".tif".  Accepts any type handled by raster::writeRaster.
#' @param overwrite logical. Whether to overwrite existing files of same name. Default
#' value of NULL causes function to ask for permission if an existing file would be overwritten.
#' @param ... optional parameters passed through to raster::writeRaster.
#'
#' @return invisibly returns the full name (including path) of the saved raster.
save_raster = function(x,  path='.', fname=NULL, type='tif', overwrite=NULL, ...) {
  xname = as.character(substitute(x))
  if (class(x) == 'character') {
    xname = x
    x = get(x)
  }
  if (is.null(fname)) fname = xname
  path = sub('/$', '', path)
  fname = paste0(path, '/', fname, '.', type)
  fname = path.expand(fname)
  if (file.exists(fname) & missing(overwrite)) {
    overwrite = tcltk::tk_messageBox('yesno', paste(fname, ': Overwrite existing file?'))
    overwrite = c(TRUE,FALSE)[match(overwrite, c('yes','no'))]
    if (overwrite) {
      warning(paste(fname, 'already exists.  Original has been overwritten.'))
      raster::writeRaster(x, fname, overwrite = overwrite, ...)
    } else {
      warning(paste(fname, 'already exists.  Not saved.'))
    }
  } else {
    raster::writeRaster(x, fname, overwrite = overwrite, ...)
  }
  invisible(fname)
}

#' Find ID in raster attribute table
#'
#' @description Given a raster and attribute value, finds the corresponding raster value
#' for that attribute.  Essentially a "vlookup" for raster attribute tables.
#'
#' @param r raster* object - must have an associated raster attribute table (RAT).
#' @param val value of the attribute to lookup.
#' @param id.col column in the RAT containing the ID to lookup.
#' @param val.col column in the RAT containing the attribute to match.
#'
#' @return Returns the ID value corresponding to the attribute value
which.level = function(r, val, id.col=1, val.col=2) {
  if (!raster::is.factor(r)) {
    warning('Raster has no RAT. Returning NA')
    return(NA)
  }
  l = raster::levels(r)[[1]]
  row.num = match(val, l[, val.col])
  return(l[row.num, id.col])
}


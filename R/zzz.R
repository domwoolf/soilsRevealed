.onLoad <- function(libname, pkgname) {
  choose_directory = function(default = "", caption = 'Select data directory') {
    if (exists('utils::choose.dir')) {
      utils::choose.dir(default = default, caption = caption)
    } else {
      tcltk::tk_choose.dir(default = default, caption = caption)
    }
  }
  gis.dir = "/home/dominic/Box Sync/projects/SoilsRevealed_IPCC_shared/gis"
  if (!dir.exists(gis.dir)) gis.dir = choose_directory(caption = "Select GIS directory")
  op <- options()
  op.soilsRevealed <- list(
    soilsRevealed.gis.path = gis.dir
  )
  toset <- !(names(op.soilsRevealed) %in% names(op))
  if(any(toset)) options(op.soilsRevealed[toset])
  invisible()
}

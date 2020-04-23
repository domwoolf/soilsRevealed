climate_classes = factor(c(
  "Tropical Montane", 'Tropical Wet', 'Tropical Moist', 'Tropical Dry',
  'Warm Temperate Moist', 'Warm Temperate Dry',
  'Cool Temperate Moist', 'Cool Temperate Dry',
  'Polar Moist', 'Polar Dry',
  'Boreal Moist','Boreal Dry'))

#' Assign local climate to IPCC category
#'
#' @param mat Mean annual temperature (degrees C).
#' @param maxmt Maximum mean monthly temperature (degrees C).
#' @param map Mean annual precipitation (mm).
#' @param pet Mean potential evapotranspiration (mm).
#' @param elev Elevation (m).
#' @param frost Mean frost days per year.
#'
#' @return The IPPC climate zone based on MAT, MAP, PET, frost, and elevation.
classify_climate = function (mat, maxmt, map, pet, elev, frost) {
  if (mat > 18 & frost <= 7) {
    if (elev > 1000) {
      class = "Tropical Montane"
    } else {
      if (map > 2000) {
        class = 'Tropical Wet'
      } else {
        if (map > 1000) {
          class = 'Tropical Moist'
        } else {
          class = 'Tropical Dry'
        }
      }
    }
  } else {
    if (mat > 10) {
      if (map > pet) {
        class = 'Warm Temperate Moist'
      } else {
        class = 'Warm Temperate Dry'
      }
    } else {
      if (mat > 0) {
        if (map > pet) {
          class = 'Cool Temperate Moist'
        } else {
          class = 'Cool Temperate Dry'
        }
      } else {
        if (maxmt < 10) {
          if (map > pet) {
            class = 'Polar Moist'
          } else {
            class = 'Polar Dry'
          }
        } else {
          if (map > pet) {
            class = 'Boreal Moist'
          } else {
            class = 'Boreal Dry'
          }
        }
      }
    }
  }
  return(match(class, climate_classes))
}

#' Classification scheme for mineral soil types based on World Reference Base
#' for Soil Resources (WRB) classification.
#'
#' @param sand sand content (%).
#' @param clay clay content (%).
#' @param wrb_class World Reference Base for Soil Resources (WRB) classification
#'
#' @return The IPPC soil type.
classify_soil_WRB = function (sand, clay, wrb_class) {
  hi_activity_clays = c('Leptosol', 'Vertisol', 'Kastanozem',
    'Chernozem', 'Phaeozem', 'Luvisol', 'Alisol', 'Albeluvisol', 'Solonetz',
    'Calcisol', 'Gypsisol', 'Umbrisol', 'Cambisol', 'Regosol')
  if (sand > 70 & clay < 8) {
    class = 'Sandy'
  } else {
    if (s <- match(wrb_class, c('Gleysol', 'Andisol', 'Podsol'), nomatch = FALSE)) {
      class = c('Wetland', 'Volcanic', 'Spodic')[s]
    } else {
      if (wrb_class %in% hi_activity_clays) {
        class = 'High activity clay'
      } else {
        class = 'Low activity clay'
      }
    }
  }
  return(class)
}


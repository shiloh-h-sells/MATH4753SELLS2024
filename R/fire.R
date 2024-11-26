# Load necessary package
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}

# Read in the data set
fire <- read.csv("FIREDAM.csv")

# Check if the data is loaded correctly
if (exists("fire") && !is.null(fire)) {
  # Save the data to the "data" directory as an .rda file
  usethis::use_data(fire, overwrite = TRUE)
} else {
  stop("The data 'fire' was not loaded correctly.")
}

#' Fire Damage Data
#'
#' A dataset containing information about fire damage incidents.
#'
#' @format A data frame with X rows and Y columns:
#' \describe{
#'   \item{incident_id}{Unique identifier for each incident}
#'   \item{date}{Date of the incident}
#'   \item{location}{Location of the incident}
#'   \item{damage_cost}{Estimated cost of damage}
#'   \item{cause}{Cause of the fire}
#'   \item{injuries}{Number of injuries}
#'   \item{fatalities}{Number of fatalities}
#' }
#' @source <source of the data, if applicable>
"fire"


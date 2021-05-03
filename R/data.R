#' The compiled ERA dataset
#'
#' \describe{
#'   \item{Index}{unique row identity}
#'   \item{Code}{unique publication identity}
#'   \item{Author}{primary author of the publication}
#'   ...
#' }
#' @source *To be added*
"ERA.Compiled"
#' Practice Codes
#'
#' \describe{ The organizational hierarchy of improved farming practice concepts considered in ERA and their descriptions.
#'   \item{Code}{unique alpha numeric code}
#'   \item{Theme}{concepts in the highest level in the practice hierarchy}
#'   \item{Theme.Code}{currently a duplicate of theme}
#'   \item{Practice}{concepts in the intermediate level of the practice hierarchy. Practice is nested under Theme.}
#'   \item{Practice.Code}{short code corresponding to the practice name for use in plotting}
#'   \item{Subpractice}{concepts in the lowest and most detailed level of the practice hierarchy. Subpractice is nested under practice.}
#'   \item{Subpractice.Code}{short code corresponding to the subpractice name for use in plotting}
#'   \item{Subpractice.S}{shorter name for subpractice}
#'   ...
#' }
#' @source *To be added*
"PracticeCodes"

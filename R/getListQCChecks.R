#' get list of datasets
#'
#' This API allows the user to have a list of all the datasets IDs that can be screened in the DATSU API.
#'
#' @param datasetverID the dataset ID
#' @param recordType string name of the record type, optional
#'
#' @return The list of Datasets that can be screened in DATSU with the IDs
#'
#' @examples
#' \dontrun{
#' getListQCChecks(145)
#' getListQCChecks(145, "VE")
#' }
#' @export
getListQCChecks <- function(datasetverID, recordType = NULL) {
  if (is.null(recordType)) {
    url <-
      datsu_api(
        paste0("getListQCChecks/", datasetverID)
      )
  } else {
    url <-
      datsu_api(
        paste0("getListQCChecks/", datasetverID),
        RecordType = recordType
      )
  }

  # perform request
  out <- datsu_get(url)

  out
}

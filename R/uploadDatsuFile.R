#' get list of records for a dataset
#'
#' This API allows the user to screen a file using the API.
#'
#' @param fileToUpload the filename of the file to upload
#' @param dataSetVerID The version of the dataset
#' @param emailAddress alternative email address of the user, if NULL, email address from ICES token
#'   is used
#' @param sendEmail it is set to TRUE by default, this will specify if the user will to receive an
#'   email when the session is finished
#' @param errorLimit does not need to be specified, it is 30000 by default
#' @param verbose get verbose output from the POST request, default FALSE
#'
#' @return The ID of the DATSU session
#'
#' @examples
#' \dontrun{
#' filename <- system.file("test_files/vms_test.csv", package = "icesDatsu")
#' id <- uploadDatsuFileFireAndForget(filename, 145)
#' getScreeningSessionDetails(id)
#' messages <- getScreeningSessionMessages(id)
#' head(messages)
#'
#' sessionID <- uploadDatsuFile(filename, 145, sendEmail = FALSE)
#' messages <- getScreeningSessionMessages(sessionID)
#' messages
#' }
#' @export
#' @importFrom httr upload_file content
#' @importFrom jsonlite toJSON
#' @importFrom icesConnect decode_token
uploadDatsuFile <- function(
    fileToUpload, dataSetVerID, emailAddress = NULL, sendEmail = TRUE, errorLimit = 30000, verbose = FALSE) {
  if (is.null(emailAddress)) {
    emailAddress <- decode_token()$email
  }
  body <- list(fileToUpload = upload_file(fileToUpload))
  resp <- datsu_post(
    datsu_api("UploadDATSUFile", EmailAddress = emailAddress, DataSetVerID = dataSetVerID, SendEmail = sendEmail,
        ErrorLimit = errorLimit),
    body = body,
    retry = TRUE, verbose = verbose, use_token = TRUE
  )
  content(resp)
}

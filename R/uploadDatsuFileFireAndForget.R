#' get list of records for a dataset
#'
#' This API allows the user to screen a file using the API.
#'
#' @param fileToUpload (stream accepted as IFormFile, part of the body of the request)
#' @param dataSetVerID The version of the dataset
#' @param emailAddress alternative email address of the user, if NULL, email address from ICES token
#'   is used
#' @param sendEmail it is set to TRUE by default, this will specify if the user will to receive an
#'   email when the session is finished
#' @param errorLimit does not need to be specified, it is 30000 by default
#'
#' @return The ID of the DATSU session
#'
#' @examples
#'
#' filename <- system.file("test_files/vms_test.csv", package = "icesDatsu")
#' uploadDatsuFileFireAndForget(filename, "colin.millar@ices.dk", 145)
#'
#' @export
#' @importFrom httr upload_file content
#' @importFrom jsonlite toJSON
uploadDatsuFileFireAndForget <- function(fileToUpload, dataSetVerID, emailAddress = NULL, sendEmail = TRUE, errorLimit = 30000) {

  if (is.null(emailAddress)) {
    emailAddress <- icesConnect::decode_token()$UserEmail
  }

  # form content
  body <-
    list(
      fileToUpload = upload_file(fileToUpload),
      EmailAddress = emailAddress,
      DataSetVerID = dataSetVerID,
      SendEmail = sendEmail,
      ErrorLimit = errorLimit
    )

  # perform request
  resp <-
    datsu_post(
      datsu_api("UploadDATSUFileFireAndForget"),
      body = body, retry = TRUE, verbose = TRUE
    )

  # get results
  content(resp)
}

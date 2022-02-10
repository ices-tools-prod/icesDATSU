# * get list of field names for DATSU format
#
# datsuFieldNames(145, "VE")
datsuFieldNames <- function(datasetverID, RecordType) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  datsuFields$fieldcode
}

# * get list of field names for DATSU format
#
# datsuFieldTypes(145, "VE")
datsuFieldTypes <- function(datasetverID, RecordType) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  fieldType <- gsub("[(][0-9]*[)]", "", datsuFields$name)
  mode <-
    ifelse(
      fieldType %in% c("nvarchar", "varchar", "char", "ntext", "text"), "character",
      ifelse(
        fieldType %in% c("int", "numeric"), "integer",
        ifelse(
          fieldType == "float",
          "numeric",
          ifelse(
            fieldType == "datetime",
            "character",
            "unknown"
          )
        )
      )
    )

  mode
}

# * get if field is mandatory
#
#
# datsuIsMandatory(145, "VE", "AverageVesselLength")
datsuIsMandatory <- function(datasetverID, RecordType, FieldName) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  datsuFields[datsuFields$fieldcode == FieldName, "mandatory"]
}

# * get if field has vocabulary
#
#
# datsuHasVocabulary(145, "VE", "AverageVesselLength")
# datsuHasVocabulary(145, "VE", "CountryCode")
datsuHasVocabulary <- function(datasetverID, RecordType, FieldName = NULL) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  if (is.null(FieldName)) {
    codeType <- datsuFields$codeGroup
  } else {
    codeType <- datsuFields[datsuFields$fieldcode == FieldName, "codeGroup"]
  }

  !is.na(codeType)
}


# * get vocabulary for field

#
#
# datsuGetVocabulary(145, "VE", "VesselLengthRange")
datsuGetVocabulary <- function(datasetverID, RecordType, FieldName = NULL) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  if (is.null(FieldName)) {
    codeType <- datsuFields$codeGroup
  } else {
    codeType <- datsuFields[datsuFields$fieldcode == FieldName, "codeGroup"]
  }

  codeList <- icesVocab::getCodeList(codeType)

  message("This is the full vocabulary, some entries may not be valid for your dataset")
  codeList[!codeList$Deprecated, "Key"]
}

# * check vector against a vocabulary
#
#
# datsuGetVocabulary(145, "VE", "AverageVesselLength")
#' @importFrom icesVocab getCodeList
datsuGetVocabulary <- function(datasetverID, RecordType, FieldName) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  codeType <- datsuFields[datsuFields$fieldcode == FieldName, "codeGroup"]

  codeList <- getCodeList(codeType)

  codeList[!codeList$Deprecated, "Key"]
}


makeEmptyTable <- function(datasetverID, RecordType) {
  df <-
    lapply(
      datsuFieldTypes(datasetverID, RecordType),
      function(x) {
        FUN <- match.fun(x)
        FUN(0)
      }
    )
  names(df) <- datsuFieldNames(datasetverID, RecordType)
  data.frame(df, check.names = FALSE)
}


# * check vector against a vocabulary
#
#
# makeExampleDatsuTable(145, "VE", 20)
#' @importFrom stats runif
makeExampleDatsuTable <- function(datasetverID, recordType, n = 10) {
  good <- as.list(makeEmptyTable(datasetverID, recordType))

  for (i in which(datsuFieldTypes(datasetverID, recordType) == "integer")) {
    good[[i]] <- sample(1:n, n, replace = TRUE)
  }

  for (i in which(datsuFieldTypes(datasetverID, recordType) == "numeric")) {
    good[[i]] <- runif(n)
  }

  for (i in which(datsuFieldTypes(datasetverID, recordType) == "character")) {
    good[[i]] <- rep("non-vocabulary", n)
  }

  for (i in which(datsuHasVocabulary(datasetverID, recordType))) {
    good[[i]] <-
      sample(
        datsuGetVocabulary(datasetverID, recordType, datsuFieldNames(datasetverID, recordType)[i]),
        n,
        replace = TRUE
      )
  }

  good$RecordType <- recordType

  good <- data.frame(good, check.names = FALSE)
  good
}



# * check a data.frame against a DATSU format
#    - check feild names
#    - check mandatory feilds for NULL etc.
#   - check vocabularied feilds against vocab
#   - check order of feilds
#   - check for missing feilds
#   - check for extra feilds
checkDatsuTable <- function(df, datasetverID, RecordType) {

  # check for missing feild names
  warnings <- character(0)
  df_names <- names(df)
  datsu_names <- datsuFieldNames(datasetverID, RecordType)
  if (!all(datsu_names %in% df_names)) {
    warnings <-
      c(
        warnings,
        paste0(
          "Missing names in table: ",
          paste0(
            datsu_names[!datsu_names %in% df_names],
            collapse = ", "
          )
        )
      )
  }

  # check for extra feild names
  if (any(!df_names %in% datsu_names)) {
        warnings <-
          c(
            warnings,
            paste0(
              "Extra names in table: ",
              paste0(
                df_names[!df_names %in% datsu_names],
                collapse = ", "
              )
            )
          )
  }

  # check for the ordering of the feild names
  if (
      all(datsu_names %in% df_names) &&
      !any(!df_names %in% datsu_names) &&
      !identical(df_names, datsu_names)
    ) {
    warnings <-
      c(
        warnings,
        paste0(
          "Column names are not in the correct order ",
          "see datsuFieldNames(", datasetverID, ",", RecordType, ")"
        )
      )
  }


  if (length(warnings)) {
    cat(warnings, sep = "\n")
    return(FALSE)
  }

  # check vocabularied feilds against vocab


#    - check mandatory feilds for NA or empty etc.
#   - check order of feilds
#   - check for missing feilds
#   - check for extra feilds

  return(TRUE)
}

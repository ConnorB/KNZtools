#' Clean Weir Data
#'
#' Processes the dataframe returned by `load_knz_weirs` to clean and structure the data.
#'
#' @param knz_weirs A dataframe returned by the `load_knz_weirs` function.
#' @return A cleaned dataframe with consistent 15-minute intervals, interpolated values, and renamed columns.
#' @importFrom dplyr mutate case_when select group_by arrange ungroup rename filter if_else
#' @importFrom lubridate ymd_hm floor_date minute
#' @importFrom zoo na.approx
#' @importFrom tidyr complete
#' @importFrom rlang .data
#' @export
clean_knz_weirs <- function(knz_weirs) {
  knz_weirs <- knz_weirs |>
    dplyr::mutate(
      RecDay = dplyr::if_else(.data$RecDay == 0, 1, .data$RecDay),  # Replace 0 with 1 for RecDay
      RecHour = dplyr::case_when(
        nchar(.data$RecHour) == 1 ~ paste0("000", .data$RecHour),
        nchar(.data$RecHour) == 2 ~ paste0("00", .data$RecHour),
        nchar(.data$RecHour) == 3 ~ paste0("0", .data$RecHour),
        nchar(.data$RecHour) == 4 ~ .data$RecHour,
        TRUE ~ NA_character_
      ),
      dateTime = lubridate::ymd_hm(paste(.data$RecYear, .data$RecMonth, .data$RecDay, .data$RecHour), tz = "UTC")
    )

  # Define the 15-minute interval sequence for each watershed
  knz_weirs <- knz_weirs |>
    dplyr::select(.data$Watershed, .data$dateTime, .data$Discharge, .data$Sheight,
                  .data$CorrectedSheight, .data$Height, .data$LogFlag, .data$QualFlag) |>
    dplyr::group_by(.data$Watershed) |>
    tidyr::complete(
      dateTime = seq(lubridate::floor_date(min(.data$dateTime), "15 min"),
                     lubridate::floor_date(max(.data$dateTime), "15 min"), by = "15 min"),
      fill = list(
        Discharge = NA, CorrectedSheight = NA, Height = NA, Sheight = NA, LogFlag = NA, QualFlag = NA
      )
    ) |>
    dplyr::arrange(.data$dateTime) |>
    dplyr::ungroup()

  # Interpolate missing numeric values and fill qual flags
  knz_weirs <- knz_weirs |>
    dplyr::arrange(.data$Watershed, .data$dateTime) |>
    dplyr::group_by(.data$Watershed) |>
    dplyr::mutate(
      Discharge = zoo::na.approx(.data$Discharge, na.rm = FALSE, maxgap = 96),
      CorrectedSheight = zoo::na.approx(.data$CorrectedSheight, na.rm = FALSE, maxgap = 96),
      Height = zoo::na.approx(.data$Height, na.rm = FALSE, maxgap = 96),
      Sheight = zoo::na.approx(.data$Sheight, na.rm = FALSE, maxgap = 96)
    ) |>
    dplyr::mutate(
      LogFlag = dplyr::if_else(is.na(.data$LogFlag), "NA", .data$LogFlag),
      QualFlag = dplyr::if_else(is.na(.data$QualFlag), "NA", .data$QualFlag)
    ) |>
    # Keep only 15-minute data
    dplyr::filter(lubridate::minute(.data$dateTime) %% 15 == 0) |>
    dplyr::ungroup() |>
    dplyr::rename(
      Q_cms = .data$Discharge,
      stageHeight_cm = .data$Sheight,
      corrStageHeight_cm = .data$CorrectedSheight,
      manualStage_cm = .data$Height
    )

  return(knz_weirs)
}

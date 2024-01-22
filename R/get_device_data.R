#' Get data from an Arable device
#' 
#' @description 
#' Get all data from Arable's data server for a single device.
#' This function is a relatively thin wrapper for Arable's data API.
#' See [Arable's documentation](https://developer.arable.com/guide/data.html) 
#' for more details.
#' 
#' @param api_key Character. API key.
#' @param temporal_resolution Character. One of: daily, hourly.
#' @param device_id Character. Device ID for the target device. 
#' Corresponds to `device` parameter in Arable's docs.
#' @param start_time Character. Beginning of query window for data. 
#' ISO-formatted string. Corresponds to `start_time` in Arable's docs.
#' @param end_time Character. End of query window for data. 
#' ISO-formatted string. Corresponds to `end_time` in Arable's docs.
#' @param data_time_zone. Character. Time zone in which to express device
#' the device's timestamps. Corresponds to `local_time` in Arable's docs.
#' 
#' @importFrom httr2 request req_headers req_perform req_error resp_body_json
#' @importFrom dplyr %>% bind_rows mutate
#' 
#' @export
get_device_data <- function(
  api_key,
  device_id,
  temporal_resolution, 
  start_time,
  end_time,
  data_time_zone = "UTC"
) {

  # compose request
  request <- httr2::request(
    base_url = paste0(
      # base URL for data
      "https://api.arable.cloud/api/v2/data/",
      # add query parameters to base URL
      temporal_resolution, "?",
      "device=", device_id,
      "&start_time=", start_time, 
      "&end_time=", end_time, "&",
      "local_time=", data_time_zone
    )) %>%
    httr2::req_headers(
      Authorization = paste0("Apikey ", api_key)
    )

  # perform request
  # erroring if HTTP status code is not 200 or 404
  # not erroring for 404 in case device ID is wrong or device doesn't have data
  response <- request %>%
    httr2::req_error(
      is_error = \(resp)  ! httr2::resp_status(resp) %in% c(200, 404)
    ) %>%
    httr2::req_perform()

  # convert the response into a data frame with the device ID
  df <- response %>%
    # parse the JSON body into a list
    httr2::resp_body_json() %>%
    # transform the list into a data frame
    dplyr::bind_rows() %>%
    dplyr::mutate(device_id = device_id)

  return(df)

}

#' @rdname get_device_data
#' @export
get_daily_device_data <- function(
  api_key,
  device_id,
  start_time,
  end_time,
  data_time_zone = "UTC"
) {

  df <- get_device_data(
    api_key,
    device_id,
    temporal_resolution = "daily",
    start_time,
    end_time,
    data_time_zone = data_time_zone
  )

  return(df)

}

#' @rdname get_device_data
#' @export
get_hourly_device_data <- function(
  api_key,
  device_id,
  start_time,
  end_time,
  data_time_zone = "UTC"
) {

  # compute number of days between `start_time` and `end_time`

  # create N segments of <= 100 days, since Arable hourly endpoint
  # returns hourly data for only the past 100 days per request

  # iterate over requests for each temporal segment

  # bind together data for the collection of all segments

  df <- get_device_data(
    api_key,
    device_id,
    temporal_resolution = "hourly",
    start_time,
    end_time,
    data_time_zone = data_time_zone
  )

  return(df)

}

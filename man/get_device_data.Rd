% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_device_data.R
\name{get_device_data}
\alias{get_device_data}
\alias{get_daily_device_data}
\alias{get_hourly_device_data}
\title{Get data from an Arable device}
\usage{
get_device_data(
  api_key,
  device_id,
  temporal_resolution,
  start_time,
  end_time,
  data_time_zone = "UTC"
)

get_daily_device_data(
  api_key,
  device_id,
  start_time,
  end_time,
  data_time_zone = "UTC"
)

get_hourly_device_data(
  api_key,
  device_id,
  start_time,
  end_time,
  data_time_zone = "UTC"
)
}
\arguments{
\item{api_key}{Character. API key.}

\item{device_id}{Character. Device ID for the target device.
Corresponds to \code{device} parameter in Arable's docs.}

\item{temporal_resolution}{Character. One of: daily, hourly.}

\item{start_time}{Character. Beginning of query window for data.
ISO-formatted string. Corresponds to \code{start_time} in Arable's docs.}

\item{end_time}{Character. End of query window for data.
ISO-formatted string. Corresponds to \code{end_time} in Arable's docs.}

\item{data_time_zone.}{Character. Time zone in which to express device
the device's timestamps. Corresponds to \code{local_time} in Arable's docs.}
}
\description{
Get all data from Arable's data server for a single device.
This function is a relatively thin wrapper for Arable's data API.
See \href{https://developer.arable.com/guide/data.html}{Arable's documentation}
for more details.
}

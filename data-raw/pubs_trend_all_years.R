## code to prepare `pubs_trend_all_years` dataset goes here
start_date <- 1781

end_date <- stringr::str_sub(string = Sys.Date(), start = 1, end = 4)
end_date <- as.numeric(end_date)

pubs_trend_all_years <- europepmc::epmc_hits_trend(query = "*",
                                                   period = start_date:end_date)

usethis::use_data(pubs_trend_all_years, overwrite = TRUE)

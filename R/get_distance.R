#' @title Scrape track distance data
#'
#' @description This function allows you to scrape distance events data from the IAAF website.
#' @param event The event to scrape ("3k", "5k", "10k", "steeple")
#' @param gender The gender (men or women) to scrape. Defaults to women.
#' @param year Year to scrape. (Data only available from 2001).
#' @keywords track
#' @export
#' @examples
#' get_distance(event = "10k", gender = "men", year = 2012)
#'
#' get_marathon(event = "5k", gender = "women", year = 2018)
#' @import dplyr
#' @import xml2
#' @import rvest
#'

get_distance <- function(event = "10k", gender = "women", year = 2018) {

  # Check if year is available

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (!(year %in% c(2001:current_year))) {
    stop('Data is not available for that year.')
  }

  # Get event

  if (event == "5k") {
    event = "5000-metres"
  } else if (event == "10k") {
    event = "10000-metres"
  } else if (event == "3k") {
    event = "3000-metres"
  } else if (event == "steeple") {
    event = "3000-metres-steeplechase"
  } else {
    stop('Please provide a valid event.')
  }

  # Grab last page number of results ----------------------------------------

  url <- paste0("https://www.iaaf.org/records/toplists/middlelong/", event, "/outdoor/", gender, "/senior/", year, "?regionType=world&page=1&bestResultsOnly=false")

  page_one_info <- xml2::read_html(url)

  last_page <- page_one_info %>%
    rvest::html_node(".pag--show") %>%
    xml2::xml_attr("data-page") %>%
    as.integer()

  if (is.na(last_page)) {
    last_page <- page_one_info %>%
      rvest::html_nodes("a.btn--number") %>%
      xml2::xml_attr("data-page") %>%
      as.integer() %>%
      max()
  }

  pages <- seq(from = 1, to = last_page)

  pb <- progress::progress_bar$new(
    format = "fetching [:bar] :percent eta: :eta",
    total = last_page, clear = FALSE, width = 60)

  list_of_dfs <- list()


  # Download data -----------------------------------------------------------

  for (page in seq_along(pages)) {
    url <- paste0("https://www.iaaf.org/records/toplists/middlelong/", event, "/outdoor/", gender, "/senior/", year, "?regionType=world&page=", page, "&bestResultsOnly=false")

                  pb$tick()

                  seq(from = 30, to = 35, by = 0.001) %>%
                  sample(1) %>%
                  Sys.sleep()

                  tables <- rvest::read_html(url) %>%
                  rvest::html_nodes("table")

                  df <- tables %>%
                  .[[3]] %>% rvest::html_table(fill = TRUE)


                  list_of_dfs[[page]] <- df
  }


                  # Save results as a data frame --------------------------------------------

                  # results <- as.data.frame(bind_rows(list_of_dfs))

                  results <- do.call(rbind, list_of_dfs)

                  results <- results[ , !duplicated(colnames(results))]

                  results[7] <- NULL

                  results <- results %>%
                  dplyr::select(Result = Mark, Competitor, DOB, Nat, Venue, Date)

                  return(results)
}

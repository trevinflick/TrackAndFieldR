#' @title Scrape marathon data
#'
#' @description This function allows you to scrape marathon and half marathon data from the IAAF website.
#' @param event The event (marathon or half marathon) to scrape. Defaults to marathon.
#' @param gender The gender (men or women) to scrape. Defaults to women.
#' @param year Year to scrape. (Data only available from 2001).
#' @keywords track
#' @export
#' @examples
#' get_marathon(event = "half-marathon", gender = "men", year = 2012)
#'
#' get_marathon(event = "marathon", gender = "women", year = 2018)
#' @import dplyr
#'

get_marathon <- function(event = "marathon", gender = "women", year = 2018) {

  # Check if year is available

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (!(year %in% c(2001:current_year))) {
    stop('Data is not available for that year.')
  }


  # Grab last page number of results ----------------------------------------

  url <- paste0("https://www.iaaf.org/records/toplists/road-running/", event, "/outdoor/", gender, "/senior/", year, "?regionType=world&drop=regular&fiftyPercentRule=regular&page=1&bestResultsOnly=false")

  page_one_info <- read_html(url)

  last_page <- page_one_info %>%
    html_node(".pag--show") %>%
    xml_attr("data-page") %>%
    as.integer()

  if (is.na(last_page)) {
    last_page <- page_one_info %>%
      html_nodes("a.btn--number") %>%
      xml_attr("data-page") %>%
      as.integer() %>%
      max()
  }

  pages <- seq(from = 1, to = last_page)

  pb <- progress_bar$new(
    format = "fetching [:bar] :percent eta: :eta",
    total = last_page, clear = FALSE, width = 60)

  list_of_dfs <- list()


  # Download data -----------------------------------------------------------

  for (page in seq_along(pages)) {
    url <- paste0("https://www.iaaf.org/records/toplists/road-running/", event, "/outdoor/", gender, "/senior/", year, "?regionType=world&drop=regular&fiftyPercentRule=regular&page=", page, "&bestResultsOnly=false")

    pb$tick()

    seq(from = 30, to = 35, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()

    tables <- read_html(url) %>%
      html_nodes("table")

    df <- tables %>%
      .[[3]] %>% html_table(fill = TRUE)


    list_of_dfs[[page]] <- df
  }


  # Save results as a data frame --------------------------------------------

  # results <- as.data.frame(bind_rows(list_of_dfs))

  results <- do.call(rbind, list_of_dfs)

  results <- results[ , !duplicated(colnames(results))]

  results[7] <- NULL

  results <- results %>%
    select(Result = Mark, Competitor, DOB, Nat, Venue, Date)

  return(results)
}

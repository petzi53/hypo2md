library(httr2)
library(jsonlite)
library(tidyverse)
library(glue)
library(clipr)

api_url = "https://hypothes.is/api/"
api_docs1 = "https://h.readthedocs.io/en/latest/api-reference/v1/"
api_docs2 = "https://h.readthedocs.io/en/latest/api-reference/v2/"

# example: fetch a public annotation
# id of the public annotation: tDwC-N4oEe2l9PMJ6ie-Tw
# url to compose: https://api.hypothes.is/api/annotations/tDwC-N4oEe2l9PMJ6ie-Tw
# req <- request(h_api) |> req_url_path_append("annotations") |> req_url_path_append("tDwC-N4oEe2l9PMJ6ie-Tw")


api_url = "https://api.hypothes.is/api"
my_url <- "https://r4ds.hadley.nz/workflow-help.html"


hypo_search <- function(
        limit        = NULL,
        sort         = NULL,
        search_after = NULL,
        offset       = NULL,
        order        = NULL,
        uri          = NULL,
        uri_parts    = NULL,
        wildcard_uri = NULL,
        user         = NULL,
        group        = NULL,
        tag          = NULL,
        tags         = NULL,
        any          = NULL,
        quote        = NULL,
        references   = NULL,
        text         = NULL
) {
    url <- httr2::url_parse(api_url)
    url$path <- "/api/search"
    url$query <-  list(
        limit        = limit,
        sort         = sort,
        search_after = search_after,
        offset       = offset,
        order        = order,
        uri          = uri,
        uri_parts    = uri_parts,
        wildcard_uri = wildcard_uri,
        user         = user,
        group        = group,
        tag          = tag,
        tags         = tags,
        any          = any,
        quote        = quote,
        references   = references,
        text         = text
    )

    # build request and perform request
    # see: https://httr2.r-lib.org/articles/httr2.html
    httr2::request(httr2::url_build(url))  |>
        httr2::req_headers(Accept = "application/vnd.hypothesis.v1+json") |>
        httr2::req_auth_bearer_token(token) |>
        httr2::req_perform() |>

        # transform API response to tibble
        httr2::resp_body_json() |>
        purrr::pluck("rows") |>
        purrr::transpose() |>
        tibble::as_tibble() |>

        # unlist all levels and create new char columns appending "_<number>"
        tidyr::unnest_wider(col = c("tags", "permissions":"user_info"), names_sep = "_") |>
        tidyr::unnest_wider(col = c("permissions_read":"document_title"), names_sep = "_") |>
        tidyr::unnest_wider(col = "target_1_selector", names_sep = "_") |>
        tidyr::unnest_wider(col = tidyselect::contains("selector"), names_sep = "_") |>
        # convert finally one level list to char string
        dplyr::mutate(dplyr::across(tidyselect::where(is.list), unlist)) |>

        # rename long names for easier work
        dplyr::rename(selection = "target_1_selector_3_exact") |>
        dplyr::rename(position = "target_1_selector_2_start") |>
        dplyr::rename(title = "document_title_1") |>

        # prepare df for easier work

        # replace NA value of page note with 0 to become first position
        # and sort by document order (= position of highlight)
        tidyr::replace_na(list("position" = 0)) |>
        dplyr::arrange("position")
}


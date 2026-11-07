# Requires the shared HTTP/date helpers and meeting_cutoff().
county_agenda_host <- "https://www.co.lancaster.pa.us"
county_commissioner_sources <- c(
  "3" = "County-Commissioners-Public-Meetings-3",
  "4" = "County-Commissioners-Work-Sessions-4"
)
county_commissioner_labels <- c("3" = "Public Meeting", "4" = "Work Session")

county_commissioner_url <- function(category) {
  paste0(county_agenda_host, "/AgendaCenter/", county_commissioner_sources[[category]])
}

fetch_county_agenda_year <- function(category, year) {
  # This read-only POST is the Agenda Center's own year-tab request.
  request(paste0(county_agenda_host, "/AgendaCenter/UpdateCategoryList")) |>
    req_body_form(year = year, catID = category, startDate = "", endDate = "",
                  term = "", prevVersionScreen = "false") |>
    req_timeout(30) |> req_retry(max_tries = 3) |> req_perform() |> resp_body_html()
}

county_agenda_tab <- function(page, category) {
  section <- html_element(page, paste0("#section", category))
  if (inherits(section, "xml_missing")) stop("Commissioners category missing: ", category)
  selected <- suppressWarnings(as.integer(html_text2(html_element(section, "li.current a"))))
  if (is.na(selected)) stop("Commissioners year tab not recognized: ", category)
  years <- suppressWarnings(as.integer(html_text2(html_elements(section, 'a[href*="changeYear("]'))))
  list(selected = selected, years = unique(c(selected, years[!is.na(years)])))
}

county_agenda_notice <- function(row) {
  h <- html_element(row, "h3")
  xml_remove(html_elements(h, "strong, a"))
  label <- gsub("[[:space:]\u2009\u00a0]+", " ", html_text2(h), perl = TRUE)
  parts <- regmatches(label, regexec("(Posted|Amended) ([A-Za-z]{3}) ([0-9]{1,2}), ([0-9]{4})(?: ([0-9]{1,2}:[0-9]{2} [AP]M))?", label, perl = TRUE))[[1]]
  if (!length(parts)) return("")
  month <- match(parts[3], month.abb)
  date <- sprintf("%04d-%02d-%02d", as.integer(parts[5]), month, as.integer(parts[4]))
  if (is.na(publication_time(date))) return("")
  paste0(parts[2], " by county: ", feed_display_date(date),
         if (nzchar(parts[6])) paste(" · ", parts[6], " ET", sep = "") else "")
}

parse_county_commissioner_documents <- function(page, category, year) {
  table <- html_element(page, paste0("#table", category))
  if (inherits(table, "xml_missing")) stop("Commissioners agenda table missing: ", category)
  rows <- html_elements(table, "tr.catAgendaRow")
  if (!length(rows)) stop("No Commissioners meeting rows recognized for ", category, " in ", year)
  records <- list()
  for (row in rows) {
    links <- html_elements(row, "a[href]")
    hrefs <- html_attr(links, "href")
    kinds <- c("Agenda", "Minutes")
    recognized <- FALSE
    notice <- county_agenda_notice(row)
    for (kind in kinds) {
      pattern <- paste0("^/AgendaCenter/ViewFile/", kind, "/_([0-9]{8})-([0-9]+)$")
      # The agenda also appears in the Download menu; it is one document.
      candidates <- unique(hrefs[grepl(pattern, hrefs)])
      if (!length(candidates)) next
      if (length(candidates) != 1L) stop("Multiple distinct ", kind, " links in one Commissioners row")
      matched <- regmatches(candidates, regexec(pattern, candidates))[[1]]
      stamp <- matched[2]
      date <- paste(substr(stamp, 5, 8), substr(stamp, 1, 2), substr(stamp, 3, 4), sep = "-")
      if (is.na(publication_time(date)) || as.integer(substr(date, 1, 4)) != year) {
        stop("Unexpected Commissioners document date: ", candidates)
      }
      id <- paste("lancaster-county-commissioners", category, matched[3], tolower(kind), sep = ":")
      records[[id]] <- list(id = id, category = category, kind = kind, date = date,
        url = paste0(county_agenda_host, candidates),
        notice = if (kind == "Agenda") notice else "")
      recognized <- TRUE
    }
    if (!recognized) stop("No direct Agenda or Minutes link in a Commissioners row")
  }
  records
}

write_county_commissioner_rss <- function(records, path) {
  rss <- xml_new_root("rss", version = "2.0")
  channel <- xml_add_child(rss, "channel")
  xml_add_child(channel, "title", "Lancaster County — Commissioners’ Meeting Documents")
  xml_add_child(channel, "link", paste0(county_agenda_host, "/agendacenter"))
  xml_add_child(channel, "description", "Direct agenda and minutes PDFs for County Commissioners public meetings and work sessions. Covers the last six calendar months and future meetings with documents posted.")
  xml_add_child(channel, "language", "en-us")
  # A newly posted set of minutes is its own item. An amended agenda keeps its
  # GUID but can move forward when the county's displayed amendment changes.
  ordered <- lapply(records, function(r) { r$first_seen <- r$activity_at; r })
  ordered <- order_feed_records(ordered, function(r) publication_time(r$date))
  for (r in ordered) {
    item <- xml_add_child(channel, "item")
    label <- county_commissioner_labels[[r$category]]
    day <- feed_display_date(r$date)
    xml_add_child(item, "title", paste(r$kind, label, day, sep = " — "))
    xml_add_child(item, "link", r$url)
    xml_add_child(item, "guid", r$id, isPermaLink = "false")
    xml_add_child(item, "pubDate", rss_date(r$feed_sort_time))
    xml_add_child(item, "category", label)
    xml_add_child(item, "category", r$kind)
    body <- xml_new_root("div")
    xml_add_child(body, "p", paste0("Meeting date: ", day))
    if (nzchar(r$notice)) xml_add_child(body, "p", r$notice)
    p <- xml_add_child(body, "p")
    xml_add_child(p, "a", paste("Open", r$kind, "PDF"), href = r$url)
    p <- xml_add_child(body, "p")
    xml_add_child(p, "a", paste("View County Commissioners", label, "listing"),
                  href = county_commissioner_url(r$category))
    xml_add_child(xml_add_child(item, "description"), xml_cdata(as.character(body)))
  }
  tmp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  write_xml(rss, tmp, options = "format")
  read_xml(tmp)
  if (!file.rename(tmp, path)) stop("Could not replace Commissioners feed")
}

collect_county_commissioner_documents <- function(root, now = as.numeric(Sys.time()),
  fetch_category = function(category) resp_body_html(fetch_page(county_commissioner_url(category))),
  fetch_year = fetch_county_agenda_year) {
  today <- as.Date(as.POSIXct(now, origin = "1970-01-01", tz = "America/New_York"), tz = "America/New_York")
  cutoff <- meeting_cutoff(today)
  years <- seq.int(as.integer(format(cutoff, "%Y")), as.integer(format(today, "%Y")))
  state_path <- file.path(root, "state", "county-commissioners.json")
  old <- if (file.exists(state_path)) read_json(state_path, simplifyVector = FALSE)$items else list()
  records <- list()
  for (category in names(county_commissioner_sources)) {
    page <- fetch_category(category)
    tab <- county_agenda_tab(page, category)
    # Always include the displayed year (it can already be next year).
    for (year in unique(c(tab$selected, intersect(years, tab$years)))) {
      current <- if (year == tab$selected) page else fetch_year(category, year)
      parsed <- parse_county_commissioner_documents(current, category, year)
      for (r in parsed) {
        if (as.Date(r$date) < cutoff) next
        previous <- old[[r$id]]
        r$first_seen <- if (is.null(previous)) now else previous$first_seen
        r$activity_at <- if (is.null(previous) || !identical(r$notice, previous$notice) ||
                              !identical(r$url, previous$url)) now else previous$activity_at
        records[[r$id]] <- r
      }
    }
  }
  # Only publish after both categories and all required year tabs succeed.
  write_county_commissioner_rss(records, file.path(root, "lancaster_county_commissioners.xml"))
  atomic_json(list(version = 1L, items = records), state_path)
  message("Wrote ", length(records), " county commissioner documents")
  invisible(records)
}

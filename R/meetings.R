# Requires R/news.R for HTTP, RSS date formatting and atomic state writes.
meeting_sources <- c(
  "Board of Supervisors" = "https://www.twp.lancaster.pa.us/government/elected_officials/board_of_supervisors/agendas___minutes.php",
  "Planning Commission" = "https://www.twp.lancaster.pa.us/government/boards___commissions/planning_commission/agendas___minutes.php",
  "Zoning Hearing Board" = "https://www.twp.lancaster.pa.us/government/boards___commissions/zoning_hearing_board/agendas___minutes.php",
  "Traffic Committee" = "https://www.twp.lancaster.pa.us/government/agendas___minutes/traffic_committee_agendas_and_minutes.php"
)

parse_meeting_documents <- function(page, entity, page_url) {
  base <- html_element(page, "base[href]") |> html_attr("href")
  if (is.na(base)) base <- page_url
  base <- url_absolute(base, page_url)
  # These pages use one table row per dated meeting, with PDF links in later cells.
  rows <- html_elements(page, "tr")
  if (!length(rows)) stop("No meeting rows found for ", entity, "; previous feed retained")
  records <- list()
  dated_rows <- 0L
  for (row in rows) {
    cells <- xml_find_all(row, "./td")
    if (!length(cells)) next
    text <- html_text2(cells[[1]])
    match <- regexpr("\\b[0-9]{1,2}/[0-9]{1,2}/([0-9]{4}|[0-9]{2})\\b", text, perl = TRUE)
    if (match[1] < 0L) next
    raw_date <- regmatches(text, match)
    bits <- strsplit(raw_date, "/", fixed = TRUE)[[1]]
    year <- as.integer(bits[3]); if (nchar(bits[3]) == 2L) year <- 2000L + year
    iso <- sprintf("%04d-%02d-%02d", year, as.integer(bits[1]), as.integer(bits[2]))
    date <- publication_time(iso)
    if (is.na(date)) stop("Invalid meeting date for ", entity, ": ", raw_date)
    dated_rows <- dated_rows + 1L
    for (a in html_elements(row, "a[href]")) {
      href <- html_attr(a, "href")
      label <- trimws(html_text2(a))
      type <- if (grepl("agenda", label, ignore.case = TRUE)) "Agenda" else if (grepl("minutes", label, ignore.case = TRUE)) "Minutes" else if (grepl("packet", label, ignore.case = TRUE)) "Packet" else NA_character_
      if (is.na(type) || !grepl("\\.pdf([?#]|$)", href, ignore.case = TRUE)) next
      url <- url_absolute(URLencode(href, reserved = FALSE), base)
      if (is.na(url) || !startsWith(url, "https://www.twp.lancaster.pa.us/")) stop("Unexpected PDF URL: ", href)
      # Ignore query timestamps in identity; keep the complete URL for opening.
      canonical <- sub("[?#].*$", "", url)
      id <- paste(entity, iso, type, canonical, sep = "|")
      records[[id]] <- list(id = id, entity = entity, meeting_date = iso,
        type = type, url = url, page_url = page_url, row_label = text)
    }
  }
  if (!dated_rows) stop("No dated meetings recognized for ", entity)
  records
}

write_meeting_rss <- function(records, path) {
  if (!length(records)) stop("No meeting PDFs; previous feed retained")
  rss <- xml_new_root("rss", version = "2.0")
  channel <- xml_add_child(rss, "channel")
  xml_add_child(channel, "title", "Lancaster Township — Meeting Documents")
  xml_add_child(channel, "link", "https://www.twp.lancaster.pa.us/government/agendas___minutes/index.php")
  xml_add_child(channel, "description", "Agendas, minutes and packets for the Board of Supervisors, Planning Commission, Zoning Hearing Board and Traffic Committee. Item dates indicate first detection, not meeting dates or publication dates.")
  xml_add_child(channel, "language", "en-us")
  xml_add_child(channel, "ttl", "720")
  # Bootstrap items share one discovery time; use meeting date to order that batch.
  records <- records[order(vapply(records, function(r) r$first_seen, numeric(1)),
                           vapply(records, function(r) r$meeting_date, character(1)), decreasing = TRUE)]
  for (r in records) {
    item <- xml_add_child(channel, "item")
    xml_add_child(item, "title", paste(r$type, r$entity, r$meeting_date, sep = " — "))
    xml_add_child(item, "link", r$url)
    xml_add_child(item, "guid", r$id, isPermaLink = "false")
    xml_add_child(item, "category", r$entity)
    xml_add_child(item, "category", r$type)
    xml_add_child(item, "pubDate", rss_date(r$first_seen))
    # Construct HTML with xml2 so URLs containing & and other special characters are escaped.
    body <- xml_new_root("div")
    xml_add_child(body, "p", paste0(r$entity, " · ", r$type, " · Meeting date: ", r$meeting_date))
    xml_add_child(body, "p", paste0("Listed as: ", r$row_label))
    xml_add_child(body, "p", paste0("First detected: ", format(as.POSIXct(r$first_seen, origin = "1970-01-01", tz = "America/New_York"), "%Y-%m-%d %H:%M %Z"), ". Publication date unavailable."))
    p <- xml_add_child(body, "p")
    xml_add_child(p, "a", paste("Open", r$type, "PDF"), href = r$url)
    p <- xml_add_child(body, "p")
    xml_add_child(p, "a", "View township document listing", href = r$page_url)
    description <- xml_add_child(item, "description")
    xml_add_child(description, xml_cdata(as.character(body)))
  }
  tmp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  write_xml(rss, tmp, options = "format")
  read_xml(tmp)
  if (!file.rename(tmp, path)) stop("Could not replace ", path)
}

collect_meeting_documents <- function(root, now = as.numeric(Sys.time()), lookback_days = 366L) {
  path <- file.path(root, "state", "township-meetings.json")
  state <- if (file.exists(path)) read_json(path, simplifyVector = FALSE) else list(items = list())
  cache <- if (is.list(state$items)) state$items else list()
  today <- as.Date(format(as.POSIXct(now, origin = "1970-01-01", tz = "America/New_York"), "%Y-%m-%d"))
  records <- list()
  for (entity in names(meeting_sources)) {
    url <- unname(meeting_sources[entity])
    page <- fetch_page(url) |> resp_body_html()
    parsed <- parse_meeting_documents(page, entity, url)
    message(entity, ": ", length(parsed), " PDF links found")
    for (id in names(parsed)) {
      r <- parsed[[id]]
      # Monitor a rolling year so late-posted minutes are still detected.
      if (as.Date(r$meeting_date) < today - lookback_days) next
      old <- cache[[id]]
      r$first_seen <- if (is.null(old)) now else old$first_seen
      cache[[id]] <- r
      records[[id]] <- r
    }
    Sys.sleep(0.3)
  }
  # All four sources must succeed before replacing output or state.
  write_meeting_rss(records, file.path(root, "lancaster_township_meetings.xml"))
  atomic_json(list(version = 1L, last_checked = now, items = cache), path)
  message("Wrote ", length(records), " meeting-document items")
  invisible(records)
}

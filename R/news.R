# Shared news-feed helpers. Listing years are inferred and explicitly labeled.
library(httr2)
library(rvest)
library(xml2)
library(jsonlite)

rss_date <- function(x) {
  old <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"),
         "%a, %d %b %Y %H:%M:%S +0000", tz = "UTC")
}

# Human-readable feed labels; machine dates, identifiers and RSS pubDate
# keep their own formats.
feed_display_date <- function(x, include_time = FALSE) {
  if (is.character(x)) x <- publication_time(x)
  local <- as.POSIXct(x, origin = "1970-01-01", tz = "America/New_York")
  stamp <- paste0(month.abb[as.integer(format(local, "%m"))], format(local, " %d, %Y"))
  if (include_time) paste(stamp, format(local, "%H:%M %Z")) else stamp
}

feed_display_clock <- function(x) {
  local <- as.POSIXct(x, origin = "1970-01-01", tz = "America/New_York")
  hour <- as.integer(format(local, "%H"))
  paste0(sprintf("%02d", (hour + 11L) %% 12L + 1L), format(local, ":%M "),
         if (hour < 12L) "AM " else "PM ", format(local, "%Z"))
}

feed_display_interval <- function(start, end) {
  first <- feed_display_date(start)
  last <- feed_display_date(end)
  paste0(first, " · ", feed_display_clock(start), " – ",
         if (first != last) paste0(last, " · ") else "", feed_display_clock(end))
}

# Date-only values mean local midnight, not midnight UTC converted to local time.
publication_time <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(NA_real_)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    t <- as.POSIXct(x, format = "%Y-%m-%d", tz = "America/New_York")
    if (is.na(t) || format(t, "%Y-%m-%d", tz = "America/New_York") != x) return(NA_real_)
    return(as.numeric(t))
  }
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?(Z|[+-]\\d{2}:?\\d{2})?$", x)) return(NA_real_)
  x <- sub("Z$", "+0000", x)
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  x <- sub("T", " ", x, fixed = TRUE)
  zoned <- grepl("[+-][0-9]{4}$", x)
  fmt <- if (grepl(" [0-9]{2}:[0-9]{2}:", x)) "%Y-%m-%d %H:%M:%OS" else "%Y-%m-%d %H:%M"
  if (zoned) fmt <- paste0(fmt, "%z")
  as.numeric(as.POSIXct(strptime(x, fmt, tz = if (zoned) "UTC" else "America/New_York")))
}

# Each listing date is independent: a pinned or reordered card cannot
# make a later card appear a year older.
infer_listing_dates <- function(dates, today) {
  today <- as.Date(today)
  month_day <- regmatches(trimws(dates), regexec("^([A-Za-z]{3}) +([0-9]{1,2})$", trimws(dates)))
  vapply(month_day, function(parts) {
    if (!length(parts)) return(NA_real_)
    month <- match(tolower(parts[2]), tolower(month.abb))
    if (is.na(month)) return(NA_real_)
    year <- as.integer(format(today, "%Y"))
    for (y in seq.int(year, year - 8L)) {
      candidate <- sprintf("%04d-%02d-%02d", y, month, as.integer(parts[3]))
      value <- publication_time(candidate)
      if (!is.na(value) && candidate <= format(today, "%Y-%m-%d")) return(value)
    }
    NA_real_
  }, numeric(1))
}

township_news_url <- "https://www.twp.lancaster.pa.us/newslist.php"

# The township renders its cards with JavaScript. Read the page's data source
# and page size without running JavaScript or visiting a second listing page.
township_news_index <- function(fetch) {
  page <- resp_body_html(fetch(township_news_url))
  scripts <- html_text(html_elements(page, "script"))
  config <- scripts[grepl("dataSource: newslistItems", scripts, fixed = TRUE)]
  if (length(config) != 1L) stop("Township news pagination configuration not found")
  capture <- function(pattern) {
    m <- regmatches(config, regexec(pattern, config, perl = TRUE))[[1]]
    if (length(m) != 2L) stop("Unexpected township news page configuration")
    m[2]
  }
  asset <- capture("url[[:space:]]*:[[:space:]]*['\"]([^'\"]+)['\"]")
  page_size <- as.integer(capture("pageSize[[:space:]]*:[[:space:]]*([0-9]+)"))
  asset_url <- xml2::url_absolute(asset, township_news_url)
  if (!startsWith(asset_url, "https://www.twp.lancaster.pa.us/") ||
      is.na(page_size) || page_size < 1L || page_size > 100L) stop("Invalid township news page configuration")
  index <- fromJSON(resp_body_string(fetch(asset_url)))
  if (!is.data.frame(index) || !all(c("date", "link") %in% names(index))) {
    stop("Unexpected township news index")
  }
  # Take the site's first page before filtering; cards without Read More links
  # must not pull articles forward from page two.
  index <- head(index, page_size)
  index <- index[!is.na(index$link) & nzchar(index$link) & index$link != "#", , drop = FALSE]
  index$url <- xml2::url_absolute(index$link, township_news_url)
  if (anyNA(index$url) || any(!startsWith(index$url, "https://www.twp.lancaster.pa.us/"))) {
    stop("Unexpected township news article URL")
  }
  index[!duplicated(index$url), , drop = FALSE]
}

clean_article <- function(page, url) {
  post <- html_element(page, "#post")
  if (inherits(post, "xml_missing")) stop("Expected article selector #post was not found: ", url)
  xml_remove(html_elements(post, "script, style, iframe, object, embed, form, noscript, .rzBtn, .rzAdmin"))
  for (node in html_elements(post, "*")) {
    attrs <- names(xml_attrs(node))
    for (a in attrs[grepl("^on", attrs, ignore.case = TRUE)]) xml_set_attr(node, a, NULL)
    # srcset can otherwise leave relative images behind; use the ordinary src.
    xml_set_attr(node, "srcset", NULL)
    xml_set_attr(node, "style", NULL)
    for (a in c("href", "src", "poster")) {
      value <- xml_attr(node, a)
      if (is.na(value) || !nzchar(value)) next
      if (grepl("^\\s*(javascript|vbscript|data):", value, ignore.case = TRUE)) {
        xml_set_attr(node, a, NULL)
      } else {
        absolute <- url_absolute(URLencode(value, reserved = FALSE), url)
        if (is.na(absolute)) stop("Could not resolve article URL: ", value)
        xml_set_attr(node, a, absolute)
      }
    }
  }
  body <- paste(as.character(xml_contents(post)), collapse = "\n")
  if (!nzchar(trimws(html_text2(post))) && !length(html_elements(post, "img"))) {
    body <- '<p>No article text was available. Open the original announcement for details.</p>'
  }
  body
}

fetch_page <- function(url) {
  request(url) |>
    req_user_agent("LancasterCommunityRSS/0.2 (personal community feeds)") |>
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_perform()
}

atomic_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  if (!file.rename(tmp, path)) stop("Could not replace ", path)
}

# RSS readers generally sort by pubDate rather than XML position. Encode the
# requested two-level order in that single date: Eastern detection DAY, then
# publication/meeting date. Seconds after midnight are ordering slots, not
# actual detection/publication times; those are kept separately in the body/state.
order_feed_records <- function(records, secondary) {
  if (!length(records)) return(records)
  days <- vapply(records, function(r) format(as.POSIXct(r$first_seen,
    origin = "1970-01-01", tz = "America/New_York"), "%Y-%m-%d"), character(1))
  dates <- vapply(records, secondary, numeric(1))
  ids <- vapply(records, function(r) if (!is.null(r$id)) r$id else r$url, character(1))
  indices <- order(days, dates, ids, decreasing = TRUE)
  records <- records[indices]
  days <- days[indices]
  for (day in unique(days)) {
    group <- which(days == day)
    if (length(group) >= 86400L) stop("Too many items for a single feed detection day")
    start <- as.numeric(as.POSIXct(day, format = "%Y-%m-%d", tz = "America/New_York"))
    for (j in seq_along(group)) records[[group[j]]]$feed_sort_time <- start + length(group) - j
  }
  records
}

write_news_rss <- function(records, path) {
  rss <- xml_new_root("rss", version = "2.0")
  channel <- xml_add_child(rss, "channel")
  xml_add_child(channel, "title", "Lancaster Township News")
  xml_add_child(channel, "link", "https://www.twp.lancaster.pa.us/newslist.php")
  xml_add_child(channel, "description", "Articles from page one of Lancaster Township’s news listing, newest first by the township’s listed date. Years are inferred.")
  xml_add_child(channel, "language", "en-us")
  xml_add_child(channel, "ttl", "720")
  dates <- vapply(records, function(r) r$date, numeric(1))
  records <- records[order(-dates, seq_along(dates))]
  for (r in records) {
    item <- xml_add_child(channel, "item")
    xml_add_child(item, "title", r$title)
    xml_add_child(item, "link", r$url)
    xml_add_child(item, "guid", r$url, isPermaLink = "true")
    xml_add_child(item, "category", "News")
    xml_add_child(item, "pubDate", rss_date(r$date))
    heading <- paste0("<p><em>", feed_display_date(r$date), "</em></p>\n")
    description <- xml_add_child(item, "description")
    xml_add_child(description, xml_cdata(paste0(heading, r$body)))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  write_xml(rss, tmp, options = "format")
  read_xml(tmp) # Check well-formedness before replacing the working feed.
  if (!file.rename(tmp, path)) stop("Could not replace feed: ", path)
}

collect_township_news <- function(root, now = Sys.time(), fetch = fetch_page) {
  index <- township_news_index(fetch)
  today <- as.Date(as.POSIXct(now, origin = "1970-01-01", tz = "America/New_York"), tz = "America/New_York")
  dates <- infer_listing_dates(index$date, today)
  if (anyNA(dates)) stop("Unrecognized listing date for: ", paste(index$url[is.na(dates)], collapse = ", "))
  records <- setNames(lapply(seq_len(nrow(index)), function(i) {
    url <- index$url[i]
    page <- resp_body_html(fetch(url))
    title <- trimws(html_text2(html_element(page, "h1#page-title")))
    if (is.na(title) || !nzchar(title)) stop("Article title not found: ", url)
    list(url = url, title = title, body = clean_article(page, url), date = dates[i])
  }), index$url)
  # Publish only after the entire current page has been retrieved. There is
  # no carryover from earlier runs; the URL GUID keeps each story's identity.
  path <- file.path(root, "lancaster_township_news.xml")
  write_news_rss(records, path)
  message("Wrote ", length(records), " news items to ", path)
  invisible(records)
}

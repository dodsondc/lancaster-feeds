# Offline behavioral checks. Run from the repository root.
source("R/news.R")
stopifnot(rss_date(publication_time("2026-09-16")) == "Wed, 16 Sep 2026 04:00:00 +0000")
stopifnot(rss_date(publication_time("2026-09-16T10:30:00-04:00")) == "Wed, 16 Sep 2026 14:30:00 +0000")
stopifnot(is.na(publication_time("Sep 16")), is.na(publication_time("2026-02-30")))
fixture <- read_html('<html><head><script type="application/ld+json">{"@type":"NewsArticle","datePublished":"2026-09-16"}</script></head><body><div id="post"><script>bad()</script><p onclick="bad()">News <a href="docs/a file.pdf">document</a></p><img src="images/a.jpg"></div></body></html>')
stopifnot(published_at(fixture) == publication_time("2026-09-16"))
body <- clean_article(fixture, "https://example.org/news/story.html")
stopifnot(!grepl("<script|onclick", body), grepl('https://example.org/news/docs/a%20file.pdf', body, fixed = TRUE), grepl('https://example.org/news/images/a.jpg', body, fixed = TRUE))
# Event dates and modified timestamps must not become news publication dates.
other <- read_html('<script type="application/ld+json">{"@type":"Event","datePublished":"2020-01-01"}</script><meta property="article:modified_time" content="2026-01-01">')
stopifnot(is.na(published_at(other)))

root <- tempfile("news-check-")
dir.create(root)
fixture_response <- httr2::response(200, headers = list(`content-type` = "text/html"), body = charToRaw('<div id="post"><p>Public announcement</p></div>'))
index_response <- httr2::response(200, body = charToRaw('[{"title":"Test announcement","link":"_T22_R1.php","date":"Unknown","image":"image.jpg?t=20260916"}]'))
fetch_page <- function(url) if (grepl("newslist.json", url, fixed = TRUE)) index_response else fixture_response
first <- collect_township_news(root)
second <- collect_township_news(root)
stopifnot(identical(rss_date(first[[1]]$date), rss_date(second[[1]]$date)), first[[1]]$date_source == "first_collected")
feed_path <- file.path(root, "lancaster_township_news.xml")
feed <- read_xml(feed_path)
stopifnot(length(xml_find_all(feed, "//item")) == 1L, grepl("First collected", xml_text(xml_find_first(feed, "//description[ancestor::item]"))))
before <- readBin(feed_path, "raw", n = file.info(feed_path)$size)
fetch_page <- function(url) if (grepl("newslist.json", url, fixed = TRUE)) index_response else stop("Simulated network failure")
failed <- tryCatch({suppressWarnings(collect_township_news(root, refresh_hours = 0)); FALSE}, error = function(e) TRUE)
after <- readBin(feed_path, "raw", n = file.info(feed_path)$size)
stopifnot(failed, identical(before, after))
# Year rollover, equal dates, malformed dates, leap days and persistent anchors.
urls <- paste0("https://example.org/", 1:5)
inferred <- infer_listing_dates(c("Jan 02", "Dec 30", "Dec 30", "Unknown", "Nov 15"), urls, today = as.Date("2027-01-05"))
stopifnot(identical(inferred[c(1, 2, 3, 5)], vapply(c("2027-01-02", "2026-12-30", "2026-12-30", "2026-11-15"), publication_time, numeric(1), USE.NAMES = FALSE)), is.na(inferred[4]))
stopifnot(infer_listing_dates("Feb 29", "u", today = as.Date("2025-03-01")) == publication_time("2024-02-29"))
anchor <- list(date_source = "estimated_listing", date = publication_time("2026-12-30"))
stable <- infer_listing_dates(c("Dec 30", "Dec 01"), urls[1:2], setNames(list(anchor), urls[1]), today = as.Date("2028-01-05"))
stopifnot(stable[1] == anchor$date, stable[2] == publication_time("2026-12-01"))

# Migrate existing fallback records without resetting first_seen or GUIDs.
old_seen <- first[[1]]$first_seen
index_response <- httr2::response(200, body = charToRaw('[{"title":"Test announcement","link":"_T22_R1.php","date":"Jan 01"}]'))
fetch_page <- function(url) if (grepl("newslist.json", url, fixed = TRUE)) index_response else fixture_response
migrated <- collect_township_news(root)
stopifnot(migrated[[1]]$date_source == "estimated_listing", abs(migrated[[1]]$first_seen - old_seen) < 0.01, migrated[[1]]$url == first[[1]]$url)
feed <- read_xml(feed_path)
stopifnot(grepl("Publication date estimated from listing", xml_text(xml_find_first(feed, "//description[ancestor::item]"))))

# Only page 1's 10 articles may be fetched/published from a larger index.
index_data <- data.frame(title = paste("Story", 1:12), link = paste0("_T22_R", 1:12, ".php"), date = "Jan 01")
index_response <- httr2::response(200, body = charToRaw(toJSON(index_data)))
requested <- character()
fetch_page <- function(url) {
  if (grepl("newslist.json", url, fixed = TRUE)) return(index_response)
  requested <<- c(requested, url)
  fixture_response
}
ten <- collect_township_news(root)
stopifnot(length(ten) == 10L, length(xml_find_all(read_xml(feed_path), "//item")) == 10L, !any(grepl("_R(11|12)\\.php", requested)))
unlink(root, recursive = TRUE)
cat("Passed: explicit dates, timezone handling, HTML cleanup, absolute links, stable fallback dates, failure retention, year inference, migration, and page-1 limit.\n")

# Reader-visible RSS dates and XML order must agree on detection day, then
# publication date. Different detection times on the same day must not dominate.
make_record <- function(id, seen, date) list(url = paste0("https://example.org/", id),
  title = id, first_seen = publication_time(seen), date = publication_time(date),
  date_source = "estimated_listing", body = "<p>Example</p>")
records <- list(
  make_record("older-published-later-detected", "2026-10-17T15:00:00-04:00", "2026-08-01"),
  make_record("newer-published", "2026-10-17T09:00:00-04:00", "2026-09-01"),
  make_record("new-detection-day", "2026-10-18T09:00:00-04:00", "2025-01-01"))
sorted <- order_feed_records(records, function(r) r$date)
stopifnot(identical(vapply(sorted, function(r) r$title, character(1)), c("new-detection-day", "newer-published", "older-published-later-detected")),
  all(diff(vapply(sorted, function(r) r$feed_sort_time, numeric(1))) < 0),
  sorted[[2]]$first_seen == records[[2]]$first_seen)
# A full news RSS file has the same descending pubDate sequence.
f <- tempfile(fileext = ".xml")
write_news_rss(records, f)
written <- read_xml(f)
stopifnot(identical(xml_text(xml_find_all(written, "//item/title")), vapply(sorted, function(r) r$title, character(1))))
stopifnot(identical(xml_text(xml_find_all(written, "//item/pubDate")), vapply(sorted, function(r) rss_date(r$feed_sort_time), character(1))))
unlink(f)
cat("Passed reader-visible detection-day/publication-date ordering.\n")

source("R/news.R")
source("R/meetings.R")
fixtures <- setNames(c("bos", "planning", "zoning", "traffic"), names(meeting_sources))
parsed <- list()
for (entity in names(meeting_sources)) {
  page <- read_html(file.path("tests", "fixtures", paste0("meetings-", fixtures[[entity]], ".html")))
  docs <- parse_meeting_documents(page, entity, meeting_sources[[entity]])
  stopifnot(length(docs) > 0L)
  for (r in docs) {
    stopifnot(r$entity == entity, r$type %in% c("Agenda", "Minutes", "Packet"),
      grepl("^https://www.twp.lancaster.pa.us/", r$url), !grepl("youtube", r$url),
      !is.na(as.Date(r$meeting_date)), !grepl("\\?", r$id))
  }
  parsed[[entity]] <- docs
}
# Respect <base>, rather than resolving incorrectly against the source directory.
bos <- parsed[["Board of Supervisors"]][[1]]
stopifnot(startsWith(bos$url, "https://www.twp.lancaster.pa.us/10-12-2026"))
# Canceled ZHB rows without PDFs should not become items.
stopifnot(!any(vapply(parsed[["Zoning Hearing Board"]], function(r) r$meeting_date == "2026-10-27", logical(1))))
# A changed cache-busting timestamp is still the same document identity.
p1 <- read_html('<base href="https://www.twp.lancaster.pa.us/"><table><tr><td>10/12/26 Board</td><td><a href="a.pdf?t=1">Agenda</a><a href="https://youtube.com/example">Video</a></td></tr></table>')
p2 <- read_html(gsub("t=1", "t=2", as.character(p1), fixed = TRUE))
a <- parse_meeting_documents(p1, "Board", "https://www.twp.lancaster.pa.us/folder/page.php")
b <- parse_meeting_documents(p2, "Board", "https://www.twp.lancaster.pa.us/folder/page.php")
stopifnot(length(a) == 1L, identical(names(a), names(b)), a[[1]]$url != b[[1]]$url)

root <- tempfile("meeting-test-"); dir.create(root)
fetch_page <- function(url) {
  entity <- names(meeting_sources)[match(url, meeting_sources)]
  content <- paste(readLines(file.path("tests", "fixtures", paste0("meetings-", fixtures[[entity]], ".html")), warn = FALSE), collapse = "\n")
  httr2::response(200, headers = list(`content-type` = "text/html"), body = charToRaw(content))
}
first <- collect_meeting_documents(root, now = publication_time("2026-10-17"))
second <- collect_meeting_documents(root, now = publication_time("2026-10-18"))
stopifnot(identical(names(first), names(second)), all(vapply(second, function(r) r$first_seen == publication_time("2026-10-17"), logical(1))))
path <- file.path(root, "lancaster_township_meetings.xml")
feed <- read_xml(path)
stopifnot(length(xml_find_all(feed, "//item")) == length(first), length(unique(xml_text(xml_find_all(feed, "//item/guid")))) == length(first))
for (d in xml_text(xml_find_all(feed, "//item/description"))) {
  html <- read_html(d)
  stopifnot(length(html_elements(html, 'a[href]')) == 2L, grepl("First detected", html_text2(html)))
}
before <- readBin(path, "raw", n = file.info(path)$size)
fetch_page <- function(url) stop("Simulated page failure")
failed <- tryCatch({collect_meeting_documents(root); FALSE}, error = function(e) TRUE)
stopifnot(failed, identical(before, readBin(path, "raw", n = file.info(path)$size)))
unlink(root, recursive = TRUE)
cat("Passed meeting checks: all four page layouts, meeting dates/types, base URLs, PDF filtering, stable IDs/discovery dates, RSS links, failure retention.\n")

# Within a shared detection day the meeting date breaks ties in reader dates.
r <- list(
  list(id = "older", first_seen = publication_time("2026-10-17T15:00:00-04:00"), meeting_date = "2026-08-10"),
  list(id = "newer", first_seen = publication_time("2026-10-17T09:00:00-04:00"), meeting_date = "2026-10-12"))
r <- order_feed_records(r, function(x) publication_time(x$meeting_date))
stopifnot(r[[1]]$id == "newer", r[[1]]$feed_sort_time > r[[2]]$feed_sort_time)
cat("Passed meeting-date tie breaking within detection day.\n")

# Run from the repository root.
source("R/news.R")
stopifnot(rss_date(publication_time("2026-09-16")) == "Wed, 16 Sep 2026 04:00:00 +0000")
stopifnot(rss_date(publication_time("2026-09-16T10:30:00-04:00")) == "Wed, 16 Sep 2026 14:30:00 +0000")
stopifnot(is.na(publication_time("Sep 16")), is.na(publication_time("2026-02-30")))
now <- publication_time("2026-11-08T02:00:00Z") # Still Nov 7 in the township.
dates <- infer_listing_dates(c("Sep 16", "Oct 09", "Nov 08", "Feb 29", "Feb 30", NA), as.Date("2026-11-07"))
expected <- vapply(c("2026-09-16", "2026-10-09", "2025-11-08", "2024-02-29"), publication_time, numeric(1), USE.NAMES = FALSE)
stopifnot(identical(dates[1:4], expected), all(is.na(dates[5:6])))

root <- tempfile("news-check-")
dir.create(root)
feed_path <- file.path(root, "lancaster_township_news.xml")
base <- "https://www.twp.lancaster.pa.us/"
asset <- paste0(base, "_assets_/newslist.json")
links <- paste0("_T22_R", 1:4, ".php")
index <- data.frame(link = links, date = c("Sep 16", "Oct 09", "Nov 08", "Nov 07"), title = "Stale listing title")
response <- function(body) httr2::response(200, headers = list(`content-type` = "text/html; charset=UTF-8"), body = charToRaw(body))
requests <- character()
fetch <- function(url) {
  requests <<- c(requests, url)
  if (url == township_news_url) return(response('<div id="newslist-container"></div><script>
    $.ajax({url: "_assets_/newslist.json", success: function(response) {
      newsPager.pagination({dataSource: newslistItems, pageSize: 3});
    }});
  </script>'))
  if (url == asset) return(response(toJSON(index)))
  id <- match(url, paste0(base, links))
  if (is.na(id)) stop("Unexpected article URL: ", url)
  response(paste0('<meta property="article:published_time" content="2020-01-01">',
    '<h1 id="page-title">Article ', id, '</h1>',
    '<p>Outside article</p><div id="post"><script>bad()</script>',
    '<p onclick="bad()" style="color:red">Full article <a href="docs/a file.pdf">document</a></p>',
    '<img src="images/a.jpg"></div>'))
}
collect_township_news(root, now, fetch)
stopifnot(identical(requests, c(township_news_url, asset, paste0(base, links[1:3]))))
feed <- read_xml(feed_path)
field <- function(doc, name) xml_text(xml_find_all(doc, paste0("/rss/channel/item/", name)))
stopifnot(identical(field(feed, "title"), paste("Article", c(2, 1, 3))),
  identical(field(feed, "guid"), paste0(base, links[c(2, 1, 3)])),
  identical(field(feed, "pubDate"), vapply(expected[c(2, 1, 3)], rss_date, character(1))))
body <- field(feed, "description")[1]
stopifnot(startsWith(body, "<p><em>Oct 09, 2026</em></p>"),
  grepl("Full article", body, fixed = TRUE),
  grepl(paste0(base, "docs/a%20file.pdf"), body, fixed = TRUE),
  grepl(paste0(base, "images/a.jpg"), body, fixed = TRUE),
  !grepl("<script|onclick|style=|Outside article|First collected", body))
# A different front page replaces the old set; placeholder cards don't pull
# page-two items forward. Surviving stories keep the same GUID.
index <- rbind(index[c(2, 4), ], data.frame(link = "#", date = "Oct 08", title = "No Read More"), index[1, ])
requests <- character()
collect_township_news(root, now, fetch)
stopifnot(identical(requests, c(township_news_url, asset, paste0(base, links[c(2, 4)]))),
  identical(field(read_xml(feed_path), "guid"), paste0(base, links[c(4, 2)])))
before <- readLines(feed_path)
broken <- function(url) if (url == paste0(base, links[4])) stop("Simulated article failure") else fetch(url)
stopifnot(inherits(try(collect_township_news(root, now, broken), silent = TRUE), "try-error"),
  identical(readLines(feed_path), before))
unlink(root, recursive = TRUE)
cat("Township news checks passed: page-one requests and removal, article titles and content, independent date inference, RSS sort dates, stable GUIDs, and failure retention.\n")

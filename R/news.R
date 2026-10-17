# Shared news-feed helpers. Only explicit publication dates are trusted.
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

published_at <- function(page) {
  candidates <- html_elements(page, 'meta[property="article:published_time"], meta[itemprop="datePublished"], meta[name="datePublished"]') |>
    html_attr("content")
  candidates <- c(candidates, html_elements(page, 'time[itemprop="datePublished"]') |> html_attr("datetime"))
  # Read datePublished only from article/news objects, never an event or dateModified.
  visit <- function(x) {
    if (!is.list(x)) return(character())
    type <- unlist(x[["@type"]], use.names = FALSE)
    own <- if (any(type %in% c("NewsArticle", "Article", "BlogPosting"))) x[["datePublished"]] else NULL
    c(if (is.character(own)) own else character(), unlist(lapply(x[vapply(x, is.list, logical(1))], visit), use.names = FALSE))
  }
  for (s in html_elements(page, 'script[type="application/ld+json"]') |> html_text()) {
    obj <- tryCatch(fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    candidates <- c(candidates, visit(obj))
  }
  for (x in candidates) {
    t <- publication_time(x)
    if (!is.na(t)) return(t)
  }
  NA_real_
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
    req_user_agent("LancasterCommunityRSS/0.2 (personal news feed; twice-daily checks)") |>
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

write_news_rss <- function(records, path) {
  if (!length(records)) stop("No articles: refusing to replace the existing feed")
  rss <- xml_new_root("rss", version = "2.0")
  channel <- xml_add_child(rss, "channel")
  xml_add_child(channel, "title", "Lancaster Township News")
  xml_add_child(channel, "link", "https://www.twp.lancaster.pa.us/newslist.php")
  xml_add_child(channel, "description", "News and announcements from Lancaster Township, Pennsylvania. Dates are verified publication dates or the first collection time, as labeled in each item.")
  xml_add_child(channel, "language", "en-us")
  xml_add_child(channel, "ttl", "720")
  records <- records[order(vapply(records, function(r) r$date, numeric(1)), decreasing = TRUE)]
  for (r in records) {
    item <- xml_add_child(channel, "item")
    xml_add_child(item, "title", r$title)
    xml_add_child(item, "link", r$url)
    xml_add_child(item, "guid", r$url, isPermaLink = "true")
    xml_add_child(item, "category", "News")
    xml_add_child(item, "pubDate", rss_date(r$date))
    label <- if (r$date_source == "published") "Published" else "First collected (publication date unavailable)"
    stamp <- format(as.POSIXct(r$date, origin = "1970-01-01", tz = "America/New_York"), "%Y-%m-%d %H:%M %Z")
    heading <- paste0("<p><em>", label, ": ", stamp, "</em></p>\n")
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

collect_township_news <- function(root, refresh_hours = 24) {
  state_path <- file.path(root, "state", "lancaster-township.json")
  state <- if (file.exists(state_path)) read_json(state_path, simplifyVector = FALSE) else list(items = list())
  cache <- if (is.list(state$items)) state$items else list()
  now <- as.numeric(Sys.time())
  index <- fetch_page("https://www.twp.lancaster.pa.us/_assets_/newslist.json") |>
    resp_body_string() |> fromJSON()
  if (!is.data.frame(index) || !all(c("title", "link") %in% names(index)) || !nrow(index)) stop("Unexpected/empty news index; previous feed retained")
  index$title <- vapply(index$title, function(title) html_text2(read_html(paste0("<p>", title, "</p>"))), character(1))
  index$url <- url_absolute(index$link, "https://www.twp.lancaster.pa.us/")
  index <- index[!duplicated(index$url), , drop = FALSE]
  failures <- character()
  records <- list()
  for (i in seq_len(nrow(index))) {
    url <- index$url[i]
    if (!startsWith(url, "https://www.twp.lancaster.pa.us/")) stop("Unexpected article host: ", url)
    old <- cache[[url]]
    fresh <- !is.null(old) && now - old$fetched_at < refresh_hours * 3600
    if (fresh) {
      record <- old
    } else {
      message(sprintf("[%d/%d] %s", i, nrow(index), index$title[i]))
      record <- tryCatch({
        page <- fetch_page(url) |> resp_body_html()
        body <- clean_article(page, url)
        published <- published_at(page)
        first_seen <- if (is.null(old)) now else old$first_seen
        # Preserve the first chosen item date even if metadata later disappears.
        date <- if (!is.null(old)) old$date else if (!is.na(published)) published else first_seen
        source <- if (!is.null(old)) old$date_source else if (!is.na(published)) "published" else "first_collected"
        list(url = url, title = index$title[i], body = body, date = date,
             date_source = source, first_seen = first_seen, fetched_at = now)
      }, error = function(e) {
        warning(url, ": ", conditionMessage(e))
        failures <<- c(failures, url)
        old
      })
      Sys.sleep(0.3)
    }
    if (!is.null(record)) {
      record$title <- index$title[i]
      cache[[url]] <- record
      records[[url]] <- record
    }
  }
  # A partial retrieval must not publish a partial feed or lose prior content.
  if (length(failures)) stop(length(failures), " article requests failed; previous feed and state retained. Rerun to retry.")
  write_news_rss(records, file.path(root, "lancaster_township_news.xml"))
  atomic_json(list(version = 1L, last_checked = now, items = cache), state_path)
  message("Wrote ", length(records), " news items to ", file.path(root, "lancaster_township_news.xml"))
  invisible(records)
}

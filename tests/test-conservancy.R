source("R/news.R")
source("R/conservancy.R")
fixture <- function(name) read_html(file.path("tests","fixtures",name))
page <- fixture("conservancy-listing.html")
events <- parse_conservancy_listing(page,as.Date("2026-10-17"))
stopifnot(length(events)==6L)
page <- fixture("conservancy-listing.html")
body <- html_element(page, "body")
xml_add_child(body, read_html('<div class="vc_inner"><h2>MORE EVENTS COMING SOON!</h2></div>') |>
  html_element(".vc_inner"))
stopifnot(identical(names(parse_conservancy_listing(page, as.Date("2026-10-17"))), names(events)))
xml_add_child(body, read_html('<div class="vc_inner"><h2>Actual event with no date</h2></div>') |>
  html_element(".vc_inner"))
missing_date <- tryCatch(parse_conservancy_listing(page, as.Date("2026-10-17")), error = conditionMessage)
stopifnot(identical(missing_date, "Missing date for Actual event with no date"))
writing <- events[["eb-1984568437620"]]
writing <- add_event_details(writing,fixture("conservancy-1984568437620.html"))
stopifnot(writing$event_date=="2026-10-24",writing$start==publication_time("2026-10-24T09:00:00-04:00"),grepl("Chester",writing$location),grepl("23.18",writing$price))
night <- add_event_details(events[["eb-1998964456500"]],fixture("conservancy-1998964456500.html"))
stopifnot(night$start==publication_time("2026-11-14T22:00:00Z")) # EST, not EDT.
stopifnot(listing_event_date("Friday, January 1",as.Date("2026-12-20"))=="2027-01-01")
missing_time <- Filter(function(e)is.null(e$start),events)
stopifnot(length(missing_time)==1L,grepl("ExtraGive",missing_time[[1]]$title))

# UTF-8 folds must be <=75 bytes and unfold to the exact original content.
long <- paste0("DESCRIPTION:",paste(rep("é🌳, text;",30),collapse=""))
folded <- ics_fold(long)
stopifnot(all(nchar(strsplit(folded,"\r\n",fixed=TRUE)[[1]],type="bytes")<=75L),
 identical(gsub("\r\n ","",folded,fixed=TRUE),long),
 identical(ics_escape("a,b;c\\d\ne"),"a\\,b\\;c\\\\d\\ne"))

root <- tempfile("conservancy-test-");dir.create(root)
fetch_page <- function(url) {
 name <- if(identical(url,conservancy_url))"conservancy-listing.html" else paste0("conservancy-",sub(".*-([0-9]+)$","\\1",url),".html")
 body<-paste(readLines(file.path("tests","fixtures",name),warn=FALSE),collapse="\n")
 httr2::response(200,headers=list(`content-type`="text/html"),body=charToRaw(body))
}
first<-collect_conservancy_events(root,now=publication_time("2026-10-17T12:00:00-04:00"))
second<-collect_conservancy_events(root,now=publication_time("2026-10-17T18:00:00-04:00"))
stopifnot(length(first)==6L,identical(names(first),names(second)),all(vapply(second,function(e)e$sequence==0L,logical(1))),
 all(vapply(second,function(e)e$updated_at==publication_time("2026-10-17T12:00:00-04:00"),logical(1))))
f<-file.path(root,"docs","calendar","eb-1984568437620.ics")
bytes<-readBin(f,"raw",n=file.info(f)$size);text<-rawToChar(bytes)
stopifnot(grepl("DTSTART:20261024T130000Z\r\n",text,fixed=TRUE),grepl("DTEND:20261024T150000Z",text,fixed=TRUE),
 !grepl("(?<!\\r)\\n",text,perl=TRUE),all(nchar(strsplit(text,"\r\n",fixed=TRUE)[[1]],type="bytes")<=75L))
reminder<-Filter(function(e)is.null(e$start),first)[[1]]
t<-paste(readLines(file.path(root,"docs","calendar",paste0(reminder$id,".ics")),warn=FALSE),collapse="\n")
stopifnot(grepl("DTSTART;VALUE=DATE:20261120",t,fixed=TRUE),grepl("DTEND;VALUE=DATE:20261121",t,fixed=TRUE),grepl("STATUS:TENTATIVE",t,fixed=TRUE))
feed<-read_xml(file.path(root,"lancaster_conservancy_events.xml"))
stopifnot(length(xml_find_all(feed,"//item"))==6L,
  any(startsWith(xml_text(xml_find_all(feed,"//item/title")), "Nov 20, 2026 — ")))
for(d in xml_text(xml_find_all(feed,"//item/description")))stopifnot(any(grepl("\\.ics$",html_elements(read_html(d),"a")|>html_attr("href"))))
# Failed detail extraction must leave the last feed unchanged.
p<-file.path(root,"lancaster_conservancy_events.xml")
before<-readBin(p,"raw",n=file.info(p)$size)
fetch_page<-function(url)stop("Simulated source failure")
failed<-tryCatch({collect_conservancy_events(root);FALSE},error=function(e)TRUE)
stopifnot(failed,identical(before,readBin(p,"raw",n=file.info(p)$size)))
unlink(root,recursive=TRUE)
cat("Passed Conservancy checks: six events, structured details, inferred years, DST, missing-time reminders, stable IDs/revisions, UTF-8 folding, escaping, CRLF, RSS calendar links, failure retention.\n")

# Opting out of Eventbrite must require only the Conservancy page.
root <- tempfile("conservancy-only-");dir.create(root)
fetch_page <- function(url) {
 stopifnot(identical(url,conservancy_url))
 body<-paste(readLines(file.path("tests","fixtures","conservancy-listing.html"),warn=FALSE),collapse="\n")
 httr2::response(200,headers=list(`content-type`="text/html"),body=charToRaw(body))
}
plain <- collect_conservancy_events(root,now=publication_time("2026-10-17"),enrich_eventbrite=FALSE)
stopifnot(length(plain)==6L,all(vapply(plain,function(e)e$price=="Not published; check registration",logical(1))))
unlink(root,recursive=TRUE)
cat("Passed Conservancy-only mode: no Eventbrite requests.\n")


# A valid listing may have no upcoming events. Unrecognized pages still fail.
root <- tempfile("conservancy-empty-")
dir.create(root)
response_html <- function(body) httr2::response(200,
  headers = list(`content-type` = "text/html"), body = charToRaw(body))
fetch_page <- function(url) response_html(paste(readLines(
  file.path("tests", "fixtures", "conservancy-listing.html"), warn = FALSE), collapse = "\n"))
now <- publication_time("2026-11-22")
empty <- collect_conservancy_events(root, now, enrich_eventbrite = FALSE)
feed_path <- file.path(root, "lancaster_conservancy_events.xml")
stopifnot(!length(empty), !length(xml_find_all(read_xml(feed_path), "//item")))
fetch_page <- function(url) response_html('<div class="vc_inner"><h2>MORE EVENTS COMING SOON!</h2></div>')
stopifnot(!length(collect_conservancy_events(root, now, enrich_eventbrite = FALSE)))
before <- readLines(feed_path)
fetch_page <- function(url) response_html("<p>Unexpected page layout</p>")
failure <- tryCatch(collect_conservancy_events(root, now, enrich_eventbrite = FALSE),
  error = conditionMessage)
stopifnot(grepl("No Conservancy events recognized", failure, fixed = TRUE),
  identical(before, readLines(feed_path)))
unlink(root, recursive = TRUE)

# Pruning preserves live links, recent events, and unrelated files.
root <- tempfile("calendar-prune-")
dir.create(root)
calendar <- file.path(root, "docs", "calendar")
expired <- first[[1]]
expired$id <- paste0("discovery-", paste(rep("a", 32), collapse = ""), "-20261024T130000")
expired_path <- file.path(calendar, paste0(expired$id, ".ics"))
write_event_ics(expired, expired_path)
linked <- first[[1]]
linked_path <- file.path(calendar, paste0(linked$id, ".ics"))
write_event_ics(linked, linked_path)
write_conservancy_rss(list(linked), file.path(root, "lancaster_conservancy_events.xml"))
date_only <- reminder
date_only$event_date <- "2026-10-01"
date_only$id <- "lc-old-reminder"
date_path <- file.path(calendar, "lc-old-reminder.ics")
write_event_ics(date_only, date_path)
recent <- reminder
recent$id <- "lc-recent-reminder"
recent_path <- file.path(calendar, "lc-recent-reminder.ics")
write_event_ics(recent, recent_path)
other_path <- file.path(calendar, "lc-custom.ics")
file.copy(date_path, other_path)
as_of <- as.Date("2026-12-10")
stopifnot(setequal(prune_expired_calendar_files(root, as_of, dry_run = TRUE), c(expired_path, date_path)),
  file.exists(expired_path), file.exists(date_path))
prune_expired_calendar_files(root, as_of)
stopifnot(!file.exists(expired_path), !file.exists(date_path), file.exists(linked_path),
  file.exists(recent_path), file.exists(other_path))
unlink(root, recursive = TRUE)

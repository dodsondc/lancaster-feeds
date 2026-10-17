conservancy_url <- "https://www.lancasterconservancy.org/events/"
calendar_base_url <- "https://dodsondc.github.io/lancaster-feeds/calendar/"

text_value <- function(x, fallback = "") {
  if (is.null(x) || !is.character(x) || !length(x) || is.na(x[1])) fallback else x[1]
}

# Use explicit years when present. Otherwise choose the nearest matching
# month/day/weekday around today, and label the year as inferred in the feed.
listing_event_date <- function(text, today) {
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  months <- paste(month.name, collapse = "|")
  m <- regexpr(paste0("(", months, ") +[0-9]{1,2}"), text, perl = TRUE)
  if (m[1] < 0) stop("Unrecognized event date: ", text)
  md <- regmatches(text, m)
  y <- regexpr("\\b20[0-9]{2}\\b", text)
  years <- if (y[1] > 0) as.integer(regmatches(text, y)) else as.integer(format(today, "%Y")) + c(-1L, 0L, 1L)
  candidates <- as.Date(strptime(paste(md, years), "%B %d %Y", tz = "UTC"))
  w <- regexpr(paste(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), collapse="|"), text)
  if (w[1] > 0) candidates <- candidates[!is.na(candidates) & format(candidates, "%A") == regmatches(text, w)]
  candidates <- candidates[!is.na(candidates)]
  if (!length(candidates)) stop("Event date/weekday could not be reconciled: ", text)
  format(candidates[which.min(abs(as.numeric(candidates - today)))], "%Y-%m-%d")
}

parse_conservancy_listing <- function(page, today) {
  result <- list()
  for (row in html_elements(page, ".vc_inner")) {
    heading <- html_elements(row, "h2")
    if (length(heading) != 1L) next
    title <- html_text2(heading)
    column <- html_element(row, ".wpb_text_column")
    raw_date <- html_element(column, "strong") |> html_text2()
    if (is.na(raw_date)) stop("Missing date for ", title)
    date <- listing_event_date(raw_date, today)
    links <- html_elements(row, "a[href]") |> html_attr("href")
    links <- links[grepl("^https://", links)]
    url <- if (length(links)) sub("\\?.*$", "", links[1]) else conservancy_url
    # Stable Eventbrite identity survives date changes. No-link announcements use title.
    id <- if (grepl("eventbrite.com/e/.*-[0-9]+$", url)) paste0("eb-", sub(".*-([0-9]+)$", "\\1", url)) else paste0("lc-", gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(iconv(title, to = "ASCII//TRANSLIT")))))
    # For non-Eventbrite entries, preserve explicit clock ranges from the listing.
    clock_text <- gsub("[–—]", "-", tolower(raw_date))
    clocks <- regmatches(clock_text, gregexpr("[0-9]{1,2}(:[0-9]{2})? *[ap]m", clock_text, perl=TRUE))[[1]]
    start <- end <- NULL
    if (length(clocks) == 2L) {
      parse_clock <- function(clock) {
        clock <- gsub(" ", "", clock, fixed=TRUE)
        fmt <- if (grepl(":",clock,fixed=TRUE)) "%Y-%m-%d %I:%M%p" else "%Y-%m-%d %I%p"
        as.numeric(as.POSIXct(strptime(paste(date,clock),fmt,tz="America/New_York")))
      }
      start <- parse_clock(clocks[1]); end <- parse_clock(clocks[2])
      if(is.na(start)||is.na(end)||end<=start)stop("Invalid listed clock range: ",raw_date)
    }
    xml_remove(html_elements(column, "h2, strong"))
    description <- trimws(html_text2(column))
    result[[id]] <- list(id=id, title=title, event_date=date, raw_date=raw_date,
      description=description, url=url, location="Not announced in listing",
      price="Not published; check registration", start=start, end=end,
      date_source="Year inferred from listing month/day/weekday", status="TENTATIVE")
  }
  if (!length(result)) stop("No Conservancy events recognized; previous feed retained")
  result
}

find_schema_event <- function(page) {
  visit <- function(x) {
    if (!is.list(x)) return(NULL)
    types <- unlist(x[["@type"]], use.names=FALSE)
    if (any(grepl("Event$", types)) && !is.null(x$startDate)) return(x)
    for (child in x[vapply(x, is.list, logical(1))]) {
      found <- visit(child); if (!is.null(found)) return(found)
    }
    NULL
  }
  for (text in html_elements(page, 'script[type="application/ld+json"]') |> html_text()) {
    x <- tryCatch(fromJSON(text, simplifyVector=FALSE), error=function(e) NULL)
    found <- visit(x); if (!is.null(found)) return(found)
  }
  stop("No structured event details found")
}

add_event_details <- function(event, page) {
  x <- find_schema_event(page)
  organizer <- text_value(x$organizer$name)
  if (!grepl("Lancaster Conservancy", organizer, fixed=TRUE)) stop("Unexpected event organizer: ", organizer)
  start <- publication_time(x$startDate)
  end <- publication_time(x$endDate)
  if (is.na(start) || is.na(end) || end <= start) stop("Invalid event start/end times")
  structured_date <- format(as.POSIXct(start, origin="1970-01-01", tz="America/New_York"), "%Y-%m-%d")
  # Reject conflicting sources rather than silently creating a wrong calendar entry.
  if (substr(structured_date,6,10) != substr(event$event_date,6,10)) stop("Listing/registration date conflict for ", event$title)
  event$event_date <- structured_date
  event$start <- start; event$end <- end
  event$date_source <- "Date and time from public registration data"
  event$status <- if (grepl("Cancelled$", text_value(x$eventStatus))) "CANCELLED" else "CONFIRMED"
  address <- x$location$address
  event$location <- paste(unique(Filter(nzchar, c(text_value(x$location$name),
    text_value(address$streetAddress), text_value(address$addressLocality), text_value(address$addressRegion), text_value(address$postalCode)))), collapse=", ")
  if (!nzchar(event$location)) event$location <- "Location not published; check registration"
  description <- text_value(x$description)
  if (nzchar(description)) event$description <- html_text2(read_html(paste0("<div>",description,"</div>")))
  offers <- x$offers
  if (!is.null(offers[["@type"]])) offers <- list(offers)
  prices <- character()
  for (offer in offers) {
    low <- text_value(offer$lowPrice, text_value(offer$price))
    high <- text_value(offer$highPrice, low)
    currency <- text_value(offer$priceCurrency)
    if (nzchar(low)) prices <- c(prices, if (!is.na(suppressWarnings(as.numeric(low))) && as.numeric(low)==0 && identical(high,low)) "Free" else paste(currency, if (identical(low,high)) low else paste(low,high,sep="–")))
  }
  if (length(prices)) event$price <- paste(unique(prices), collapse="; ")
  event
}

ics_escape <- function(text) {
  chars <- strsplit(gsub("\r\n?", "\n", enc2utf8(text)), "", fixed=TRUE)[[1]]
  paste0(vapply(chars, function(x) switch(x, "\\"="\\\\", ";"="\\;", ","="\\,", "\n"="\\n", x), character(1)), collapse="")
}

# Fold at 75 UTF-8 octets, keeping multi-byte characters intact (RFC 5545).
ics_fold <- function(line) {
  lines <- character(); current <- ""
  for (char in strsplit(enc2utf8(line), "", fixed=TRUE)[[1]]) {
    if (nchar(paste0(current,char), type="bytes") > 75L) {
      lines <- c(lines,current); current <- " "
    }
    current <- paste0(current,char)
  }
  paste(c(lines,current),collapse="\r\n")
}

write_event_ics <- function(event, path) {
  utc <- function(t) format(as.POSIXct(t, origin="1970-01-01",tz="UTC"),"%Y%m%dT%H%M%SZ",tz="UTC")
  timed <- !is.null(event$start) && !is.null(event$end)
  summary <- if (timed) event$title else paste0(event$title," (date reminder; verify time)")
  description <- paste(event$description, paste("Price:",event$price),
    paste("Schedule listed:",event$raw_date), event$date_source,
    if (!timed) "Date reminder only: time has not been announced. This is not an all-day activity." else "Times converted from America/New_York to UTC for calendar compatibility.",
    paste("Registration:",event$url), "Adding this calendar item does not register you. Check the original listing for changes; imported events do not automatically stay in sync.",sep="\n\n")
  lines <- c("BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//Lancaster Community Feeds//Conservancy Events//EN", "CALSCALE:GREGORIAN",
    "BEGIN:VEVENT",paste0("UID:",event$id,"@lancaster-feeds.dodsondc.github.io"),
    paste0("DTSTAMP:",utc(event$updated_at)),paste0("LAST-MODIFIED:",utc(event$updated_at)),paste0("SEQUENCE:",event$sequence),
    if(timed) c(paste0("DTSTART:",utc(event$start)),paste0("DTEND:",utc(event$end))) else c(paste0("DTSTART;VALUE=DATE:",gsub("-","",event$event_date)),paste0("DTEND;VALUE=DATE:",format(as.Date(event$event_date)+1,"%Y%m%d"))),
    paste0("SUMMARY:",ics_escape(summary)),paste0("DESCRIPTION:",ics_escape(description)),
    paste0("LOCATION:",ics_escape(event$location)),paste0("URL:",event$url),
    paste0("STATUS:",event$status),"TRANSP:TRANSPARENT","END:VEVENT","END:VCALENDAR")
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  tmp <- tempfile(tmpdir=dirname(path)); on.exit(unlink(tmp),add=TRUE)
  writeBin(charToRaw(paste0(paste(vapply(lines,ics_fold,character(1)),collapse="\r\n"),"\r\n")),tmp)
  if(!file.rename(tmp,path))stop("Could not replace calendar file")
}

write_conservancy_rss <- function(events, path) {
  rss <- xml_new_root("rss",version="2.0"); channel <- xml_add_child(rss,"channel")
  xml_add_child(channel,"title","Lancaster Conservancy — Events")
  xml_add_child(channel,"link",conservancy_url)
  xml_add_child(channel,"description","Upcoming Lancaster Conservancy events with registration and individual calendar-file links. Dates, prices and locations come from public listings; missing details are labeled.")
  xml_add_child(channel,"language","en-us")
  events <- order_feed_records(events,function(e) publication_time(e$event_date))
  for(e in events) {
    item<-xml_add_child(channel,"item")
    xml_add_child(item,"title",paste(e$event_date,e$title,sep=" — "))
    xml_add_child(item,"link",e$url);xml_add_child(item,"guid",paste0("conservancy:",e$id),isPermaLink="false")
    xml_add_child(item,"pubDate",rss_date(e$feed_sort_time));xml_add_child(item,"category","Events")
    body<-xml_new_root("div")
    when <- if(!is.null(e$start)) paste(format(as.POSIXct(e$start,origin="1970-01-01",tz="America/New_York"),"%A, %B %d, %Y · %I:%M %p %Z"),"–",format(as.POSIXct(e$end,origin="1970-01-01",tz="America/New_York"),"%I:%M %p %Z")) else paste(e$event_date,e$raw_date,sep=" · ")
    for(text in c(when,e$date_source,paste("Location:",e$location),paste("Price:",e$price),e$description)) xml_add_child(body,"p",text)
    p<-xml_add_child(body,"p");xml_add_child(p,"a",if(!is.null(e$start)) "Add to Calendar (.ics)" else "Add date reminder to Calendar (.ics; verify time)",href=paste0(calendar_base_url,e$id,".ics"))
    p<-xml_add_child(body,"p");xml_add_child(p,"a","Event details and registration",href=e$url)
    xml_add_child(body,"p","Adding to your calendar does not register you. Imported events do not automatically update; check the source for changes.")
    xml_add_child(body,"p",paste("First detected:",format(as.POSIXct(e$first_seen,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d %H:%M %Z")))
    d<-xml_add_child(item,"description");xml_add_child(d,xml_cdata(as.character(body)))
  }
  tmp<-tempfile(tmpdir=dirname(path));on.exit(unlink(tmp),add=TRUE)
  write_xml(rss,tmp,options="format");read_xml(tmp)
  if(!file.rename(tmp,path))stop("Could not replace Conservancy feed")
}

collect_conservancy_events <- function(root, now=as.numeric(Sys.time()), enrich_eventbrite = tolower(Sys.getenv("CONSERVANCY_EVENTBRITE", "true")) == "true") {
  path<-file.path(root,"state","conservancy-events.json")
  state<-if(file.exists(path))read_json(path,simplifyVector=FALSE) else list(items=list())
  cache<-state$items
  today<-as.Date(format(as.POSIXct(now,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d"))
  events<-parse_conservancy_listing(fetch_page(conservancy_url)|>resp_body_html(),today)
  result<-list()
  for(id in names(events)) {
    e<-events[[id]];old<-cache[[id]]
    # Fetch public structured registration data; never require account credentials.
    if(enrich_eventbrite && startsWith(e$url,"https://www.eventbrite.com/e/")) {
      e<-add_event_details(e,fetch_page(e$url)|>resp_body_html());Sys.sleep(0.3)
    }
    if(as.Date(e$event_date)<today)next
    e$first_seen<-if(is.null(old))now else old$first_seen
    signature<-as.character(toJSON(e,auto_unbox=TRUE,null="null",digits=NA))
    changed<-is.null(old)||!identical(old$signature,signature)
    e$updated_at<-if(changed)now else old$updated_at
    e$sequence<-if(is.null(old))0L else old$sequence+as.integer(changed)
    e$signature<-signature
    result[[id]]<-e;cache[[id]]<-e
  }
  # Zero recognized upcoming entries is a visible failure, not a silently empty feed.
  if(!length(result))stop("No upcoming Conservancy events; previous feed retained")
  # Fetch and validate all source data before publishing any generated files.
  for(e in result)write_event_ics(e,file.path(root,"docs","calendar",paste0(e$id,".ics")))
  write_conservancy_rss(result,file.path(root,"lancaster_conservancy_events.xml"))
  atomic_json(list(version=1L,last_checked=now,items=cache),path)
  message("Wrote ",length(result)," Conservancy events and individual calendar files")
  invisible(result)
}

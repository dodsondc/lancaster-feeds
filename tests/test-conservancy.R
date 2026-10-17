source("R/news.R")
source("R/conservancy.R")
fixture <- function(name) read_html(file.path("tests","fixtures",name))
page <- fixture("conservancy-listing.html")
events <- parse_conservancy_listing(page,as.Date("2026-10-17"))
stopifnot(length(events)==6L)
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
stopifnot(length(xml_find_all(feed,"//item"))==6L)
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

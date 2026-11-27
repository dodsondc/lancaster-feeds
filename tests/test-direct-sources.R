source('R/news.R');source('R/conservancy.R');source('R/discovery-preferences.R');source('R/discoveries.R');source('R/direct-sources.R')
local_day<-function(t)format(as.POSIXct(t,origin='1970-01-01',tz='America/New_York'),'%Y-%m-%d %H:%M:%S')
r<-direct_date_range('October 30–November 1, 2026')
stopifnot(local_day(r$start)=='2026-10-30 00:00:00',local_day(r$end)=='2026-11-01 23:59:59')
r<-direct_date_range('December 28 - January 3, 2027')
stopifnot(substr(local_day(r$start),1,10)=='2026-12-28',substr(local_day(r$end),1,10)=='2027-01-03')
stopifnot(inherits(try(direct_date_range('November 20'),silent=TRUE),'try-error'))
stopifnot(direct_performance_excerpt('Ages 10-16\nJanuary 3-21 (Rehearsals)\nJanuary 22-24 (Performances)')=='January 22-24',direct_performance_excerpt('Rehearsals: January 3-28\nPerformances: January 29 - February 7')=='January 29 - February 7')
ware<-function(title='Heartland Marimba Festival – FREE',date='Friday, November 6, 2026',category='Performing Arts',body='A concert.',timing='Doors: 6pm | Begins: 7pm')read_html(paste0('<div id="RhpEventsSingle"><div id="eventTitle"><h1>',title,'</h1></div><div class="eventStDate">',date,'</div><div class="eventTagLine">',category,'</div><div class="singleEventDescription">',body,'</div><div class="eventDoorStartDate">',timing,'</div><a class="venueLink">The Ware Center</a><div class="eventCost">Free</div></div>'))
e<-parse_direct_ware(ware(),'https://example.org/marimba')
stopifnot(!e$all_day,!e$end_known,e$start==e$end,local_day(e$start)=='2026-11-06 19:00:00')
ex<-parse_direct_ware(ware('Prison Portrait Project','Friday, October 2, 2026','Gallery Exhibit','October 2–30: Portraits.'),'https://example.org/gallery')
stopifnot(ex$all_day,substr(local_day(ex$end),1,10)=='2026-10-30')
# The direct event enriches an existing aggregator story without changing its GUID.
now<-publication_time('2026-10-26T12:00:00-04:00')
a<-e;a$direct<-FALSE;a$source<-'Visit Lancaster City';a$series<-'city-marimba';a$url<-'https://example.org/city';a$title<-'We the People First Friday: Heartland Marimba Festival';a$location<-'Ware Center for the Arts';a$end<-a$start+3600;a$price<-'$99'
before<-build_discoveries(list(a),discovery_preferences,now-86400)
after<-build_discoveries(list(a,e),discovery_preferences,now,before$events)
stopifnot(length(after$events)==1L,identical(names(before$events),names(after$events)),after$events[[1]]$first_seen==now-86400,after$events[[1]]$price=='Free',!after$events[[1]]$occurrences[[1]]$end_known)
root<-tempfile();dir.create(root)
write_discovery_rss(after$events,file.path(root,'feed.xml'),root)
ics<-readLines(list.files(file.path(root,'docs','calendar'),full.names=TRUE)[1])
stopifnot(any(grepl('DTSTART:20261107T000000Z',ics,fixed=TRUE)),!any(startsWith(ics,'DTEND')),grepl('end time not published',discovery_when(e),fixed=TRUE))
# A source failure retains its own last good records without blocking others.
z<-collect_source_safely('ware','Ware',root,now,function()list(e))
z2<-collect_source_safely('ware','Ware',root,now+1,function()stop('Fixture failure'))
stopifnot(z$health$status=='ok',z2$health$status=='stale',length(z2$events)==1L,z2$events[[1]]$title==e$title)
z3<-collect_source_safely('new','New',root,now,function()stop('Fixture failure'))
stopifnot(z3$health$status=='failed',length(z3$events)==0L)
seed_source_cache('city','Visit Lancaster City',root,before$events,now-86400)
migrated<-collect_source_safely('city','Visit Lancaster City',root,now,function()stop('Upgrade fixture failure'))
stopifnot(migrated$health$status=='stale',length(migrated$events)==1L,migrated$events[[1]]$series=='city-marimba')
private<-e;private$title<-'Private Event';stopifnot(!discovery_match(private,discovery_preferences)$keep)
stopifnot(discovery_text('<style>bad css</style><p>Good text</p><script>bad script</script>')=='Good text')
# Explicit season years place January in the next year, regardless of run date.
cfg<-direct_source_registry()$fulton
page<-read_html('<body><h1>2026/2027 Season</h1><div class="show-tile"><span class="ft-screen-reader">Annie</span><div class="show-tile__dates">November 20 - December 31</div><a href="https://example.org/shows/annie/">More Info</a></div><div class="show-tile"><span class="ft-screen-reader">Peter Pan</span><div class="show-tile__dates">January 22 - February 14</div><a href="https://example.org/shows/pan/">More Info</a></div></body>')
shows<-parse_direct_fulton(page,cfg,function(url)read_html('<div class="basic-text__text_rich">A musical production.</div>'))
stopifnot(substr(local_day(shows[[1]]$start),1,10)=='2026-11-20',substr(local_day(shows[[2]]$start),1,10)=='2027-01-22')
# EPAC links both season URL styles and repeats links with different hosts.
cfg <- direct_source_registry()$epac
epac_home <- read_html('<a href="/2026season/">Current</a><a href="/2027-season/">Next</a>
  <a href="https://epactheatre.org/2027-season/">Next again</a><a href="/events/">Special</a>')
epac_old <- read_html('<div class="et_pb_text_inner"><h3>Cinderella</h3><h5>December 4-20, 2026</h5></div>')
epac_new <- read_html('<div class="et_pb_column">
  <div class="et_pb_heading"><h5>Anastasia</h5></div>
  <div class="et_pb_heading"><h6>Young Performers Production</h6></div>
  <div class="et_pb_heading"><h5>December 3-23</h5></div></div>')
epac_special <- read_html('<div class="et_pb_text_inner"><h3>The Woman in Black</h3>
  <h3>October 30-November 1, 2026</h3></div>
  <div class="et_pb_text_inner"><h3>Undated year</h3><h3>December 23</h3></div>')
shows <- suppressMessages(fetch_direct_epac(cfg, function(url) {
  if (identical(url, cfg$url)) return(epac_home)
  switch(sub(cfg$url, "", url, fixed = TRUE),
    "2026season/" = epac_old, "2027-season/" = epac_new, "events/" = epac_special,
    stop("Unexpected EPAC URL: ", url))
}))
stopifnot(
  identical(vapply(shows, `[[`, "", "title"), c("Cinderella", "Anastasia", "The Woman in Black")),
  identical(vapply(shows, function(e) substr(local_day(e$start), 1, 10), ""),
    c("2026-12-04", "2027-12-03", "2026-10-30"))
)
# Both parenthetical and prefix performance labels survive nested strong/br HTML.
cfg<-direct_source_registry()$servant
card<-'<div class="summary-item"><a class="summary-title-link" href="/shows/2027/honk">Honk! JR</a><div class="summary-excerpt"><p><strong>Ages 10-16</strong><br><strong>January 3-21 (Rehearsals)<br>January 22-24 (Performances)</strong></p></div></div>'
shows<-fetch_direct_servant(cfg,function(url)if(identical(url,cfg$url))list(mainContent=card) else list(item=list(id='honk',body='Public performances.')))
stopifnot(substr(local_day(shows[[1]]$start),1,10)=='2027-01-22',substr(local_day(shows[[1]]$end),1,10)=='2027-01-24')
# Upcoming company schedules retain advertised dates and ignore season promos.
cfg <- direct_source_registry()$gretna
gretna <- read_html('<div class="wixui-repeater__item"><h5>Season announcement</h5>
  <h6>Summer 2027</h6><a href="/shows/announcement">More info</a></div>
  <div class="wixui-repeater__item"><h5>Example Show</h5>
  <h6>May 27th - June 6th, 2027</h6><a href="/shows/example">More info</a></div>')
shows <- parse_direct_gretna(gretna, cfg)
stopifnot(length(shows) == 1L, substr(local_day(shows[[1]]$start), 1, 10) == "2027-05-27",
  substr(local_day(shows[[1]]$end), 1, 10) == "2027-06-06")
# Dutch Apple uses the year in each calendar URL for dates without a year.
cfg <- direct_source_registry()$dutch_apple
dutch_home <- read_html('<a href="/our-shows/main-theatre-2027/">Main Stage</a>
  <a href="/our-shows/concerts-2027/">Concerts</a>
  <a href="/our-shows/childrens-theatre-2027/">Children’s Theatre</a>')
dutch_calendar <- function(title, slug, date) read_html(sprintf(
  '<a href="/shows/%s/"><img alt="%s"><div class="show-info"><p class="dates">%s</p></div></a>',
  slug, title, date))
shows <- fetch_direct_dutch_apple(cfg, function(url) {
  if (url == cfg$url) return(dutch_home)
  switch(basename(sub("/$", "", url)),
    "main-theatre-2027" = dutch_calendar("Main Stage Show", "show", "January 15 - February 20"),
    "concerts-2027" = dutch_calendar("March Concert", "concert", "March 22 - March 23"),
    "childrens-theatre-2027" = dutch_calendar("Family Show", "family", "April 23 - May 14"),
    stop("Unexpected Dutch Apple URL: ", url))
})
stopifnot(identical(vapply(shows, `[[`, "", "title"), c("Main Stage Show", "March Concert", "Family Show")),
  substr(local_day(shows[[2]]$start), 1, 10) == "2027-03-22",
  substr(local_day(shows[[2]]$end), 1, 10) == "2027-03-23")
# A weekly source stays available on daily runs, with an explicit refresh override.
refresh_count <- 0L
refresh_fixture <- function() {
  refresh_count <<- refresh_count + 1L
  list(e)
}
collect_weekly <- function(at, force = FALSE) {
  collect_source_safely("weekly", "Example Arts", root, at, refresh_fixture,
    refresh_days = 7L, force = force)
}
first_week <- collect_weekly(now)
next_day <- collect_weekly(now + 86400)
stopifnot(refresh_count == 1L, next_day$health$status == "ok",
  identical(first_week$health$last_success_utc, next_day$health$last_success_utc),
  length(next_day$events) == 1L)
invisible(collect_weekly(now + 7L * 86400))
invisible(collect_weekly(now + 8L * 86400, force = TRUE))
stopifnot(refresh_count == 3L)
# West Art uses the display category, which can differ from the document type.
west_cfg <- direct_source_registry()$west_art
west_entry <- function(id, category, title, day, time, manual = list(), excluded = list(),
    frequency = "No") {
  list(id = id, url = paste0("/event/", id), type = "event", data = list(type = category,
    title = title, date = day, time = time, manual_dates = manual, excluded_dates = excluded,
    repeated = frequency, location = "City Hall"))
}
west_documents <- list(
  west_entry("quartet", "Concerts", "West Art Presents: String Quartet", "2026-11-28",
    "6:00 - 7:30 PM & 8:30 - 10:00 PM"),
  west_entry("swap", "Events", "Sunday Swap", "2026-10-18", "10:00 - 3:00 PM",
    manual = list(list(date = "2026-12-06"))),
  west_entry("mic", "Events", "Open Mic", "2026-11-18", "7:00 - 9:00 PM",
    excluded = list(list(date = "2026-12-02")), frequency = "Weekly"),
  west_entry("watercolor", "Clubs", "Watercolor Club", "2026-11-28", "10 AM"),
  west_entry("yoga", "Wellness", "Yoga", "2026-11-28", "10 AM"))
west_json <- jsonlite::toJSON(list(events = west_documents), auto_unbox = TRUE)
west_script <- jsonlite::toJSON(list(1L, paste0("24:", west_json)), auto_unbox = TRUE)
west_page <- read_html(paste0("<script>self.__next_f.push(", west_script, ")</script>"))
west <- parse_direct_west_art(west_page, west_cfg, as.Date("2026-11-27"), as.Date("2026-12-10"))
stopifnot(identical(vapply(west, function(e) local_day(e$start), ""),
  c("2026-11-28 18:00:00", "2026-11-28 20:30:00", "2026-12-06 10:00:00", "2026-12-09 19:00:00")))
venue <- west[[1]]
venue$arts_role <- west_cfg$arts_role
tourism <- venue
tourism$direct <- FALSE
tourism$arts_role <- NULL
tourism$source <- "Discover Lancaster"
tourism$title <- "String Quartet at West Art"
tourism$location <- "West Art Lancaster"
tourism$categories <- "Music & Dance"
tourism$url <- tourism$series <- "https://www.discoverlancaster.com/events/string-quartet/"
workshop <- tourism
workshop$title <- "Watercolor workshop"
workshop$categories <- "Classes & Workshops"
workshop$url <- workshop$series <- "https://www.discoverlancaster.com/events/watercolor/"
west_now <- publication_time("2026-11-27")
west_built <- build_discoveries(list(tourism, workshop, venue), discovery_preferences,
  west_now, arts_sources = list(west_art = west_cfg))$events
stopifnot(length(west_built) == 2L,
  sum(vapply(west_built, function(e) identical(e$arts_role, "venue") && length(e$sources) == 2L, logical(1))) == 1L)
company <- venue
company$source <- "Lancaster Symphony Orchestra"
company$arts_role <- "company"
company$url <- company$series <- "https://www.lancastersymphony.org/quartet"
west_overlap <- build_discoveries(list(tourism, venue, company), discovery_preferences,
  west_now, arts_sources = direct_source_registry()[c("west_art", "symphony")])$events
stopifnot(length(west_overlap) == 1L, isTRUE(west_overlap[[1]]$covered_company))
unlink(root,recursive=TRUE)
cat('Direct-source tests passed: date ranges/DST, rehearsal exclusion, start-only calendars, enrichment/deduplication, stable identity, source failure fallback, and season years.\n')

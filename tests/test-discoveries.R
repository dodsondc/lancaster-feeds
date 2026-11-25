source('R/news.R');source('R/conservancy.R');source('R/discovery-preferences.R');source('R/discoveries.R');source('R/direct-sources.R')
now<-publication_time('2026-10-18T12:00:00-04:00')
p<-discovery_preferences
fixture<-function(title='Chamber Music Concert',source='Visit Lancaster City',series='series1',start='2026-11-06T19:00:00-05:00',end='2026-11-06T21:00:00-05:00',venue='Ware Center for the Arts, 42 N Prince St') {
 list(source=source,source_id=paste(series,start),series=series,title=title,description='Description with dance rhythms.',categories='',url=paste0('https://example.org/',series),location=venue,price='$20',start=publication_time(start),end=publication_time(end),all_day=FALSE,status='CONFIRMED')
}
e<-fixture()
stopifnot(discovery_match(e,p)$keep)
dance<-fixture('Contemporary Dance Performance',venue='Other Hall');stopifnot(!discovery_match(dance,p)$keep)
party<-fixture('Party at the Market',venue='Other Hall');stopifnot(!discovery_match(party,p)$keep)
p$always_include_title<-'Contemporary Dance';stopifnot(discovery_match(dance,p)$keep);p<-discovery_preferences
# Venue preferences catch opaque production names, and deliberately override dance.
annie<-fixture('Annie',venue='Fulton Theatre, 12 North Prince Street')
stopifnot(discovery_match(annie,p)$keep,grepl('Preferred venue',discovery_match(annie,p)$reason))
stopifnot(discovery_match(fixture('Ballet',venue='Ware Center'),p)$keep)
for(v in c('Green Dragon Farmers Market', 'Eden Resort and Suites', 'Lancaster Central Market',
 'Old Windmill Farm', 'Plough Restaurant', "Root&#8217;s Country Market and Auction",
 'Strasburg Rail Road', 'The Barn at Paradise Station')) {
 rejected<-discovery_match(fixture('Chamber Music Concert',venue=v),p)
 stopifnot(!rejected$keep,grepl('Excluded venue',rejected$reason))
}
for(v in c('Demuth Musuem','LancasterHistory','Lancaster History'))stopifnot(discovery_match(fixture('New Show',venue=v),p)$keep)
# Description-only interests are visible in the audit; vague boilerplate is not enough.
obscure<-fixture('Annie',venue='Other Hall');obscure$description<-'Enjoy this beloved musical.'
stopifnot(discovery_match(obscure,p)$keep,grepl('Description:',discovery_match(obscure,p)$reason))
for(t in c('A guided tour of the collection','Join a walking tour downtown')) {
 obscure$description<-t;stopifnot(discovery_match(obscure,p)$keep)
}
obscure$description<-'Visit our historic downtown location.';stopifnot(!discovery_match(obscure,p)$keep)
# Explicit inclusion can allow a one-off exception at a generally omitted venue.
p$always_include_title<-'Special Concert';stopifnot(discovery_match(fixture('Special Concert',venue='Plough Restaurant'),p)$keep);p<-discovery_preferences
# Same concert across sources collapses; all provenance links survive.
e2<-fixture(source='LancasterPA',series='other',venue='The Ware Center, 42 North Prince Street')
b<-build_discoveries(list(e,e2),p,now);stopifnot(length(b$events)==1L,length(b$events[[1]]$sources)==2L,length(b$events[[1]]$occurrences)==1L)
# A less precise all-day duplicate contributes provenance, not a second calendar item.
e3<-e2;e3$start<-publication_time('2026-11-06');e3$end<-e3$start+86399;e3$all_day<-TRUE;e3$price<-'$25'
bb<-build_discoveries(list(e,e3),p,now);stopifnot(length(bb$events)==1L,length(bb$events[[1]]$occurrences)==1L,bb$events[[1]]$price=='$20; $25')
# Different venues or non-overlapping dates across sources must not collapse.
e2$location<-'Other Hall';stopifnot(length(build_discoveries(list(e,e2),p,now)$events)==2L)
e2<-fixture(source='LancasterPA',series='other',start='2026-11-08T19:00:00-05:00',end='2026-11-08T21:00:00-05:00');stopifnot(length(build_discoveries(list(e,e2),p,now)$events)==2L)
# Three dated performances become one engagement, retaining gaps in the schedule.
repeaters<-lapply(c('06','13','20'),function(d)fixture(start=paste0('2026-11-',d,'T19:00:00-05:00'),end=paste0('2026-11-',d,'T21:00:00-05:00')))
b<-build_discoveries(repeaters,p,now);stopifnot(length(b$events)==1L,b$events[[1]]$kind=='Engagements',length(b$events[[1]]$occurrences)==3L)
# State round trip preserves GUID/detection/sequence; rolling performance stable.
f<-tempfile();write_json(list(items=b$events),f,auto_unbox=TRUE,null='null',digits=NA);old<-read_json(f,simplifyVector=FALSE)$items;unlink(f)
b2<-build_discoveries(repeaters,p,now+86400,old);stopifnot(identical(names(b$events),names(b2$events)),b2$events[[1]]$first_seen==now,b2$events[[1]]$sequence==0L)
b3<-build_discoveries(repeaters[-1],p,now+86400,old);stopifnot(identical(names(b$events),names(b3$events)))
# Range events must not create a weeks-long .ics block.
span<-fixture('History Exhibition',start='2026-10-01T10:00:00-04:00',end='2026-12-01T17:00:00-05:00')
b<-build_discoveries(list(span),p,now);stopifnot(b$events[[1]]$kind=='Engagements')
tmp<-tempfile();dir.create(tmp);write_discovery_rss(b$events,file.path(tmp,'feed.xml'),tmp)
stopifnot(!length(list.files(file.path(tmp,'docs','calendar'))))
# Timed occurrences get discrete calendar entries, correct Eastern->UTC conversion.
b<-build_discoveries(repeaters,p,now);write_discovery_rss(b$events,file.path(tmp,'feed.xml'),tmp)
headline<-xml_text(xml_find_first(read_xml(file.path(tmp,'feed.xml')), '//item/title'))
stopifnot(startsWith(headline, 'Nov 06, 2026 to Nov 20, 2026 — '))
ics<-list.files(file.path(tmp,'docs','calendar'),full.names=TRUE);stopifnot(length(ics)==3L)
stopifnot(any(grepl('DTSTART:20261107T000000Z',readLines(ics[1]),fixed=TRUE)))
# Validate actual source shapes, HTML entity decoding and explicit UTC dates.
city<-parse_discovery_city(read_xml('tests/fixtures/discoveries-city.xml'))
pa<-parse_discovery_pa(read_json('tests/fixtures/discoveries-pa.json',simplifyVector=FALSE))
stopifnot(length(city)>0L,length(pa)>0L,all(vapply(c(city,pa),function(e)is.finite(e$start)&&is.finite(e$end),logical(1))))
# Valid empty match set writes an empty channel, rather than leaving old matches.
write_discovery_rss(list(),file.path(tmp,'empty.xml'),tmp);stopifnot(length(xml_find_all(read_xml(file.path(tmp,'empty.xml')),'/rss/channel/item'))==0L)
# Broken pagination must fail rather than publish an incomplete source.
mock_rss<-function()read_xml('<rss><channel/></rss>')
mock_json<-function(url)list(events=list(list(id=1)),total=2,total_pages=1)
err<-try(fetch_discovery_pa(as.Date('2026-10-18'),as.Date('2027-01-16'),mock_json,mock_rss),silent=TRUE)
stopifnot(inherits(err,'try-error'))
# Build both feeds from the same groups, including touring dates and an early season listing.
company <- fixture("Winter Concert", "Example Company", "company")
company$arts_role <- "company"
company$direct <- TRUE
company$poster <- "https://example.org/poster.png"
company$ticket_url <- "https://example.org/tickets"
venue <- fixture("Winter Concert", "Example Venue", "venue")
venue$arts_role <- "venue"
venue$direct <- TRUE
tourism <- fixture("Winter Concert", series = "tourism")
other <- fixture("Music Festival", series = "tourism-only")
far_venue <- fixture("Future Venue Concert", "Example Venue", "far-venue",
  "2027-03-01", "2027-03-01T20:00:00-05:00")
far_venue$arts_role <- "venue"
far_venue$direct <- TRUE
far_company <- far_venue
far_company$source <- "Example Company"
far_company$series <- "far-company"
far_company$title <- "Future Company Concert"
far_company$arts_role <- "company"
before_follow <- build_discoveries(list(tourism), p, now)$events
built <- build_discoveries(list(venue, tourism, company, other, far_venue, far_company), p, now, before_follow)
performing <- Filter(function(e) !is.null(e$arts_role), built$events)
discoveries <- Filter(function(e) is.null(e$arts_role), built$events)
stopifnot(length(performing) == 2L, length(discoveries) == 1L,
  discoveries[[1]]$title == other$title, names(before_follow) %in% names(performing))
merged <- performing[[names(before_follow)[1]]]
stopifnot(merged$arts_organization == company$source, length(merged$sources) == 3L)
write_discovery_rss(performing, file.path(tmp, "arts.xml"), tmp, performing = TRUE)
channel <- xml_find_first(read_xml(file.path(tmp, "arts.xml")), "/rss/channel")
stopifnot(endsWith(xml_text(xml_find_first(channel, "./link")), "/performing-arts/"))
article <- xml_find_first(channel, "./item[contains(title, 'Winter Concert')]")
stopifnot(grepl("🎭 Example Company", xml_text(xml_find_first(article, "./title")), fixed = TRUE))
body <- read_html(xml_text(xml_find_first(article, "./description")))
stopifnot(html_attr(html_element(body, "img"), "src") == company$poster,
  company$ticket_url %in% html_attr(html_elements(body, "a"), "href"),
  any(grepl("\\.ics$", html_attr(html_elements(body, "a"), "href"))))
# An orchestra listing matches the tourism title that spells out the performer.
lso <- fixture("Tales & Legends", "Lancaster Symphony Orchestra", "orchestra", venue = "Gardner Theatre, Lancaster")
lso$arts_role <- "company"
lso$direct <- TRUE
lso_tourism <- fixture("Tales & Legends with the Lancaster Symphony Orchestra",
  venue = "Gardner Theatre at Lancaster Country Day School, Lancaster")
matched <- build_discoveries(list(lso_tourism, lso), p, now)$events
stopifnot(length(matched) == 1L, length(matched[[1]]$sources) == 2L,
  matched[[1]]$arts_organization == lso$source)
# Covered organizations and venues do not need matching event titles.
sources <- direct_source_registry()[c("ware", "symphony", "mickey")]
venue_only <- fixture("Chamber Concert at the Arts Center", series = "tourism-venue",
  venue = "Ware Center for the Arts, 42 North Prince Street")
winter_only <- fixture("Music Concert", series = "tourism-winter", venue = "Winter Center, Millersville")
organizer_only <- fixture("Chamber Orchestra Concert", series = "tourism-organizer",
  venue = "Other Hall")
organizer_only$organizers <- "Lancaster Symphony Orchestra"
website_only <- fixture("Jazz Recital", series = "tourism-website", venue = "Another Hall")
website_only$website <- "https://www.mickeysblackbox.com/events/jazz/"
uncovered <- fixture("Piano Recital", series = "tourism-uncovered", venue = "Other Hall")
uncovered$description <- "The pianist previously performed with Lancaster Symphony Orchestra."
results <- build_discoveries(list(venue_only, winter_only, organizer_only, website_only, uncovered),
  p, now, arts_sources = sources)
stopifnot(length(results$events) == 1L, results$events[[1]]$title == uncovered$title,
  sum(results$audit$reason == "Covered by Performing Arts") == 4L)
reinstated <- build_discoveries(list(venue_only), p, now, arts_sources = sources["symphony"])
stopifnot(length(reinstated$events) == 1L)
merged_arts <- build_discoveries(list(lso, lso_tourism), p, now, arts_sources = sources)
stopifnot(length(merged_arts$events) == 1L, length(merged_arts$events[[1]]$sources) == 2L)
# Discover Lancaster reads the public calendar search and keeps date ranges.
index_page <- read_html('<div data-listings=""></div>')
listing <- html_element(index_page, "[data-listings]")
xml_set_attr(listing, "data-info", '{"appId":"EXAMPLE","index":"calendar","apiKey":"public-test"}')
xml_set_attr(listing, "data-config", '{"filters":"calendarName:Default"}')
searches <- 0L
hit <- list(id=42, title="Snowshoe Hares in Pennsylvania", uri="/snowshoe-harés/",
  address=list("Middle Creek Wildlife Management Area"),
  startDate=now + 86400, endDate=now + 86400 + 3600, eventCategories=list("Outdoor"))
discovery_hits <- fetch_discover_lancaster(as.Date("2026-10-18"), as.Date("2026-11-18"),
  get_page=function(url) index_page, search=function(req) {
    stopifnot(req$url == "https://example-dsn.algolia.net/1/indexes/calendar/query")
    searches <<- searches + 1L
    other <- hit
    other$id <- 43
    other$eventCategories <- list("Shopping")
    list(hits=list(if (searches == 1L) hit else other), nbPages=2L, nbHits=2L)
  })
stopifnot(searches == 2L, length(discovery_hits) == 1L,
  discovery_hits[[1]]$url == paste0(discover_lancaster_url, "snowshoe-har%C3%A9s/"),
  isTRUE(discovery_hits[[1]]$all_day),
  discovery_match(discovery_hits[[1]], p)$keep)
# The season grid uses company listings, including dates past the feed window.
holiday <- fixture("Holiday Run", "Example Company", "holiday",
  "2026-12-29T19:00:00-05:00", "2027-01-03T21:00:00-05:00")
holiday$arts_role <- "company"
holiday$direct <- TRUE
grid_path <- file.path(tmp, "docs", "performing-arts", "seasons.html")
ending_today <- fixture("Ongoing Run", "Example Company", "ongoing",
  "2026-11-19T19:00:00-05:00", "2026-11-22T21:00:00-05:00")
ending_today$arts_role <- "company"
ending_today$direct <- TRUE
write_company_season_grid(list(venue, company, holiday, holiday, far_company, ending_today),
  c("Example Company", "Company Without Listings"), p, as.Date("2026-11-22"), grid_path)
grid <- read_html(grid_path)
links <- function(query) html_text2(xml_find_all(grid, query))
stopifnot(
  length(xml_find_all(grid, "//table")) == 1L,
  identical(links("//tbody/tr/th"),
    c("November 2026", "December 2026", "January 2027", "February 2027", "March 2027")),
  identical(links("//thead/tr[1]/th")[1:3], c("Month", "Example Company", "Company Without Listings")),
  length(links("//tbody//a[contains(., 'Holiday Run')]")) == 2L,
  identical(links("//tbody//a[contains(., 'Ongoing Run')]"), "Ongoing Run · Nov 19–22"),
  !length(links("//tbody//a[contains(., 'Winter Concert')]")),
  identical(links("//tbody/tr[th='March 2027']/td[1]//a"),
    "Future Company Concert · Mar 1"),
  !any(grepl("Example Venue", links("//th"), fixed = TRUE)),
  html_attr(xml_find_first(grid, "//a[contains(., 'Holiday Run')]"), "href") == holiday$url
)
write_company_season_grid(list(), "Example Company", p, as.Date("2026-11-30"), grid_path)
grid <- read_html(grid_path)
stopifnot(identical(links("//tbody/tr/th"), "November 2026"))
write_company_season_grid(list(), "Example Company", p, as.Date("2026-12-01"), grid_path)
grid <- read_html(grid_path)
stopifnot(identical(links("//tbody/tr/th"), "December 2026"))
# The normal collection path puts covered programs in one RSS article.
local({
  root <- file.path(tmp, "collection")
  dir.create(root)
  company$source <- "Fulton Theatre"
  venue$title <- "Venue-only Concert"
  direct_source_registry <- function() list(fulton = list(arts_role = "company", name = company$source))
  collect_source_safely <- function(id, name, root, now, collect) {
    list(events = if (id == "city") list(tourism, other) else list(),
      health = data.frame(source_id = id, source = name, status = "ok"))
  }
  collect_direct_sources <- function(root, today, through, now, force) {
    list(fulton = list(events = list(company, venue),
      health = data.frame(source_id = "fulton", source = company$source, status = "ok")))
  }
  collect <- collect_discoveries
  environment(collect) <- environment()
  collect(root, now, force_arts = FALSE)
  arts <- read_xml(file.path(root, "lancaster_performing_arts.xml"))
  items <- xml_find_all(arts, "/rss/channel/item")
  stopifnot(length(items) == 2L,
    xml_text(xml_find_first(items[[1]], "guid")) == "lancaster:performing-arts-season-grid",
    grepl("Venue-only Concert", xml_text(xml_find_first(items[[2]], "title")), fixed = TRUE))
  article <- read_html(xml_text(xml_find_first(items[[1]], "description")))
  table <- html_element(article, "table")
  stopifnot(!inherits(table, "xml_missing"),
    length(xml_find_all(table, ".//a[contains(., 'Winter Concert')]")) == 1L,
    company$url %in% html_attr(html_elements(table, "a"), "href"))
  discoveries <- read_xml(file.path(root, "lancaster_discoveries.xml"))
  stopifnot(length(xml_find_all(discoveries, "/rss/channel/item")) == 1L,
    grepl(other$title, xml_text(xml_find_first(discoveries, "//item/title")), fixed = TRUE))
})
unlink(tmp,recursive=TRUE)
cat('Discovery tests passed\n')

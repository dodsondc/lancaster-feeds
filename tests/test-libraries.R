source('R/news.R');source('R/conservancy.R');source('R/discovery-preferences.R');source('R/discoveries.R');source('R/libraries.R')
p<-discovery_preferences
listing<-read_html('tests/fixtures/libraries-listing.html');parsed<-parse_library_listing(listing)
stopifnot(length(parsed$events)==4L,is.null(parsed$next_url),parsed$events[[1]]$source_id=='187728')
# Cross-month icons can omit the day; accessibility labels preserve the full date.
cross<-read_html('tests/fixtures/libraries-listing.html');cross<-html_element(cross,'.lc-event--upcoming > article')
invisible(xml_set_attr(html_element(cross,'.lc-event__title a'),'aria-label','View Details on Monday, December 21, 2026 @ 12:00am'))
xml_remove(html_element(cross,'.lc-date-icon__item--day'))
invisible(xml_set_text(html_element(cross,'.lc-date-icon__item--month'),'Dec-Jan'))
stopifnot(library_listing_date(cross)=='2026-12-21')
# Multi-day icons show a day range; the first day is the listing boundary.
range_card<-html_element(listing,'.lc-event--upcoming > article');day_node<-html_element(range_card,'.lc-date-icon__item--day');invisible(xml_set_attr(html_element(range_card,'.lc-event__title a'),'aria-label',NULL));invisible(xml_set_text(day_node,'19 - 23'))
stopifnot(library_listing_date(range_card)=='2026-10-19')
e<-parsed$events[[1]];e$title<-'A Guided Tour';stopifnot(discovery_match(e,p)$keep)
e$age_groups<-list('Teens','Adults');stopifnot(!discovery_match(e,p)$keep)
e$age_groups<-list('Everyone');p$libraries$include_everyone<-FALSE;stopifnot(!discovery_match(e,p)$keep);p<-discovery_preferences
for(age in c('Babies','Toddlers','Children','Tweens','Teens')){e$age_groups<-list(age);stopifnot(!discovery_match(e,p)$keep)}
e$age_groups<-list('Adults','Seniors');e$title<-'A wonderful evening';e$description<-'';e$categories<-'Music';e$program_types<-list('Music');stopifnot(discovery_match(e,p)$keep)
e$program_types<-list('Technology Class');e$categories<-'Technology Class';stopifnot(!discovery_match(e,p)$keep)
e$age_groups<-list();stopifnot(!discovery_match(e,p)$keep)
e$age_groups<-list('Adults');e$program_types<-list('Presentation');e$categories<-'Presentation';e$title<-'Mills of Lancaster County';e$description<-'An afternoon with historian Donald Kautz.';stopifnot(discovery_match(e,p)$keep)
e$title<-'MTPL Plant Swap!';e$description<-'';stopifnot(discovery_match(e,p)$keep)
e$title<-'A New Program';e$description<-'Learn about the history of Lancaster.';stopifnot(discovery_match(e,p)$keep)
e$description<-'';stopifnot(!discovery_match(e,p)$keep)
e$age_groups<-list('Everyone');e$title<-'Concert for Toddlers';e$program_types<-list('Music');stopifnot(!discovery_match(e,p)$keep)
e$age_groups<-list('Adults');e$title<-'Children of Men';e$program_types<-list('Movie & Film');stopifnot(discovery_match(e,p)$keep)
e$age_groups<-list('Everyone');e$program_types<-list('Music');e$title<-'Concert';p$libraries$excluded_program_types<-'Music';stopifnot(!discovery_match(e,p)$keep);p<-discovery_preferences
p$libraries$included_libraries<-'Lititz';stopifnot(!discovery_match(e,p)$keep);p<-discovery_preferences
# Full descriptions, exact timezone-aware dates, address, and verified age tags.
detailed<-add_library_details(parsed$events[[1]],read_html('tests/fixtures/libraries-event.html'))
stopifnot(detailed$start==publication_time('2026-10-19T09:00:00-04:00'),grepl('151 North Queen Street',detailed$location),grepl('homelessness',detailed$description),'Everyone' %in% unlist(detailed$age_groups),!detailed$all_day)
e$location<-'Lancaster Library, LPL Offsite Location, 438 N. Queen Street';stopifnot(!discovery_match(e,p)$keep)
p$libraries$included_libraries<-character()
# Cache reuse must not cause detail HTTP calls; rejected youth events need none.
tmp<-tempfile();dir.create(tmp);calls<-0L
get_page<-function(url) {
 calls<<-calls+1L
 if(grepl('/events/upcoming',url,fixed=TRUE))listing else read_html('tests/fixtures/libraries-event.html')
}
now<-publication_time('2026-10-18T12:00:00-04:00')
x<-fetch_library_events(tmp,as.Date('2026-10-18'),as.Date('2027-01-16'),p,get_page,now)
first_calls<-calls;calls<-0L
y<-fetch_library_events(tmp,as.Date('2026-10-18'),as.Date('2027-01-16'),p,get_page,now+60)
stopifnot(calls==1L,first_calls==4L,length(x)==length(y))
# Missing source structure and looping pagination are visible failures.
stopifnot(inherits(try(parse_library_listing(read_html('<html><body>Error</body></html>')),silent=TRUE),'try-error'))
loop<-read_html('tests/fixtures/libraries-listing.html');f<-html_element(loop,'form');li<-xml_add_child(f,'div',class='pager__item--next');xml_add_child(li,'a',href='?age_groups%5B54%5D=54&age_groups%5B231%5D=231&age_groups%5B55%5D=55',rel='next')
err<-try(fetch_library_events(tmp,as.Date('2026-10-18'),as.Date('2027-01-16'),p,function(url)loop,now),silent=TRUE);stopifnot(inherits(err,'try-error'))
unlink(tmp,recursive=TRUE)
cat('Library tests passed\n')

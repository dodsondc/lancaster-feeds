source('R/news.R');source('R/conservancy.R');source('R/discovery-preferences.R');source('R/discoveries.R')
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
unlink(tmp,recursive=TRUE)
cat('Discovery tests passed\n')

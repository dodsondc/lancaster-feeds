# Public venue sources. Each adapter returns the same event records as the
# discovery collectors. Missing times/prices remain explicit, never guessed.
direct_source_registry <- function() list(
  ware=list(name="Ware / Millersville",type="ware",url="https://artsmu.com/events/feed/"),
  prima=list(name="Prima Theatre",type="squarespace",url="https://www.primatheatre.org/events?format=json",venue="Prima Theatre, 941 Wheatland Avenue, Lancaster, PA"),
  demuth=list(name="Demuth Museum",type="squarespace",url="https://www.demuth.org/events?format=json"),
  demuth_exhibitions=list(name="Demuth Exhibitions",type="squarespace",url="https://www.demuth.org/exhibitions?format=json"),
  fulton=list(name="Fulton Theatre",type="fulton",url="https://thefulton.org/shows/",venue="Fulton Theatre, 12 North Prince Street, Lancaster, PA"),
  epac=list(name="Ephrata Performing Arts Center",type="epac",url="https://www.ephrataperformingartscenter.com/",venue="Ephrata Performing Arts Center, 320 Cocalico Street, Ephrata, PA"),
  servant=list(name="Servant Stage",type="servant",url="https://servantstage.org/upcoming-shows?format=json",venue="Multiple touring venues; see the performance itinerary below"),
  mickey=list(name="Mickey's Black Box",type="tec",url="https://mickeysblackbox.com/wp-json/tribe/events/v1/events"),
  birdclub=list(name="Lancaster County Bird Club",type="tec",url="https://lancasterbirdclub.org/wp-json/tribe/events/v1/events")
)

# Explicit year from source, or a year supplied by its season/show URL. Numeric
# ranges are actual advertised date ranges; no weekly recurrence is invented.
direct_date_range <- function(text,year=NULL) {
  old<-Sys.getlocale("LC_TIME");on.exit(Sys.setlocale("LC_TIME",old));Sys.setlocale("LC_TIME","C")
  text<-gsub("[–—]","-",text);text<-gsub("([0-9])(st|nd|rd|th)\\b","\\1",text,ignore.case=TRUE,perl=TRUE)
  months<-paste(month.name,collapse="|")
  pattern<-paste0("(",months,") +([0-9]{1,2})(?:,? +(20[0-9]{2}))?(?: *- *(?:(",months,") +)?([0-9]{1,2})(?:,? +(20[0-9]{2}))?)?")
  m<-regmatches(text,regexec(pattern,text,ignore.case=TRUE,perl=TRUE))[[1]]
  if(!length(m))stop("No explicit month/day in: ",text)
  start_year<-if(nzchar(m[4]))as.integer(m[4]) else if(nzchar(m[7]))as.integer(m[7]) else year
  if(is.null(start_year)||is.na(start_year))stop("No source year for: ",text)
  sm<-match(tolower(m[2]),tolower(month.name));em<-if(nzchar(m[5]))match(tolower(m[5]),tolower(month.name)) else sm
  ey<-if(nzchar(m[7]))as.integer(m[7]) else start_year+as.integer(em<sm)
  if(!nzchar(m[4])&&nzchar(m[7])&&em<sm)start_year<-ey-1L
  sd<-sprintf("%04d-%02d-%02d",start_year,sm,as.integer(m[3]));ed<-sprintf("%04d-%02d-%02d",ey,em,if(nzchar(m[6]))as.integer(m[6]) else as.integer(m[3]))
  if(is.na(as.Date(sd))||is.na(as.Date(ed))||ed<sd)stop("Invalid source date range: ",text)
  list(start=publication_time(sd),end=publication_time(as.character(as.Date(ed)+1L))-1,date=sd,raw=m[1])
}

direct_event <- function(source,id,title,url,description,start,end,location="",price="Not supplied by this source; check event details",all_day=TRUE,end_known=TRUE) {
  if(!nzchar(title)||!is.finite(start)||!is.finite(end)||end<start)stop("Invalid direct event: ",url)
  list(source=source,source_id=as.character(id),series=url,title=discovery_text(title),url=url,
    description=discovery_text(description),categories="",start=start,end=end,
    location=if(nzchar(location))discovery_text(location) else "Not announced; check source",price=price,
    all_day=all_day,end_known=end_known,status="CONFIRMED",direct=TRUE)
}

parse_direct_squarespace <- function(x,cfg) {
  if(is.null(x$upcoming)||is.null(x$collection$id))stop("Unrecognized Squarespace calendar: ",cfg$url)
  host<-paste0(httr2::url_parse(cfg$url)$scheme,"://",httr2::url_parse(cfg$url)$hostname)
  xs<-c(x$upcoming,x$past)
  lapply(xs,function(e) {
    loc<-e$location
    location<-paste(unique(Filter(nzchar,vapply(c("addressTitle","addressLine1","addressLine2"),function(k)text_value(loc[[k]]),character(1)))),collapse=", ")
    if(!nzchar(location))location<-text_value(cfg$venue)
    # Millisecond fractional noise in Squarespace isn't meaningful schedule data.
    start<-floor(e$startDate/1000);end<-floor(e$endDate/1000)
    a<-direct_event(cfg$name,e$id,e$title,xml2::url_absolute(e$fullUrl,host),e$body,start,end,location,all_day=end-start>86400)
    a$categories<-paste(unlist(e$categories),collapse=", ")
    a
  })
}

fetch_direct_tec <- function(cfg,today,through,get_json) {
  result<-list();page<-1L
  repeat {
    url<-(request(cfg$url)|>req_url_query(per_page=50,page=page,start_date=as.character(today),end_date=as.character(through)))$url
    x<-get_json(url)
    if(is.null(x$events)||is.null(x$total_pages))stop("Unrecognized calendar API: ",cfg$url)
    es<-parse_discovery_pa(x$events)
    for(i in seq_along(es)){es[[i]]$source<-cfg$name;es[[i]]$direct<-TRUE;es[[i]]$end_known<-TRUE}
    result<-c(result,es)
    if(page>=as.integer(x$total_pages))break
    page<-page+1L;if(page>50L)stop("Direct calendar pagination limit: ",cfg$name)
  }
  result
}

parse_direct_ware <- function(page,url,source="Ware / Millersville") {
  title<-html_text2(html_element(page,"#RhpEventsSingle #eventTitle h1"))
  date<-html_text2(html_element(page,"#RhpEventsSingle .eventStDate"))
  desc<-html_text2(html_element(page,"#RhpEventsSingle .singleEventDescription"))
  if(is.na(title)||is.na(date)||is.na(desc))stop("Unrecognized Ware event page: ",url)
  r<-direct_date_range(date);year<-as.integer(substr(r$date,1,4))
  category<-html_text2(html_element(page,"#RhpEventsSingle .eventTagLine"))
  # Exhibitions advertise a run in their description, not only opening night.
  if(!is.na(category)&&grepl("Gallery",category,ignore.case=TRUE))r<-direct_date_range(desc,year)
  timing<-html_text2(html_element(page,"#RhpEventsSingle .eventDoorStartDate"))
  start<-r$start;end<-r$end;all_day<-TRUE;end_known<-TRUE
  if(!is.na(timing)&&grepl("Begins:",timing,fixed=TRUE)&&!grepl("Gallery",text_value(category),ignore.case=TRUE)) {
    clock<-regmatches(timing,regexec("Begins: *([0-9]{1,2}(?::[0-9]{2})? *[ap]m)",timing,ignore.case=TRUE,perl=TRUE))[[1]]
    if(length(clock)) {
      value<-gsub(" ","",tolower(clock[2]));fmt<-if(grepl(":",value))"%Y-%m-%d %I:%M%p" else "%Y-%m-%d %I%p"
      start<-as.numeric(as.POSIXct(strptime(paste(r$date,value),fmt,tz="America/New_York")))
      end<-start;all_day<-FALSE;end_known<-FALSE
    }
  }
  venue<-html_text2(html_element(page,"#RhpEventsSingle .venueLink"))
  if(is.na(venue)||!nzchar(venue))stop("Ware venue missing: ",url)
  price<-html_text2(html_element(page,"#RhpEventsSingle .eventCost"))
  if(is.na(price)||!nzchar(price))price<-"Not supplied by this source; check event details"
  e<-direct_event(source,url,title,url,desc,start,end,venue,price,all_day,end_known);e$categories<-text_value(category);e
}

fetch_direct_ware <- function(cfg,get_page) {
  rss<-get_page(cfg$url,xml=TRUE);urls<-unique(xml_text(xml_find_all(rss,"/rss/channel/item/link")))
  if(!length(urls))stop("Ware RSS contains no event links")
  lapply(urls,function(url)parse_direct_ware(get_page(url),url,cfg$name))
}

parse_direct_fulton <- function(listing,cfg,get_page) {
  text<-html_text2(html_element(listing,"body"))
  season<-regmatches(text,regexec("(20[0-9]{2})/(20[0-9]{2})",text))[[1]]
  if(!length(season)||as.integer(season[3])!=as.integer(season[2])+1L)stop("Fulton season years not recognized")
  rows<-html_elements(listing,".show-tile");if(!length(rows))stop("Fulton show tiles missing")
  lapply(rows,function(row) {
    title<-html_text2(html_element(row,".ft-screen-reader"));date<-html_text2(html_element(row,".show-tile__dates"))
    urls<-html_attr(html_elements(row,"a[href]"),"href");url<-urls[grepl("/shows/",urls)][1]
    if(is.na(url)||is.na(date))stop("Fulton show has no detail link/date")
    # July-June season is explicit in the page's 2026/2027 heading. January-June
    # dates belong to its second year, not the current system year.
    r<-direct_date_range(date,as.integer(season[2]));month<-as.integer(substr(r$date,6,7))
    if(month<7L)r<-direct_date_range(date,as.integer(season[3]))
    url<-xml2::url_absolute(url,cfg$url)
    detail<-get_page(url);desc<-html_text2(html_element(detail,".basic-text__text_rich"))
    if(is.na(desc)||!nzchar(desc))stop("Fulton description missing: ",url)
    e<-direct_event(cfg$name,url,title,url,desc,r$start,r$end,cfg$venue)
    e$date_source<-paste("Advertised range in",paste(season[2:3],collapse="/"),"season");e
  })
}

parse_direct_epac <- function(page,cfg,url,year=NULL) {
  blocks<-html_elements(page,".et_pb_text_inner");result<-list()
  for(block in blocks) {
    headings<-html_elements(block,"h3");dates<-html_elements(block,"h5")
    if(!length(headings))next
    title<-html_text2(headings[1]);date<-if(length(dates))html_text2(dates[1]) else if(length(headings)>1L)html_text2(headings[2]) else ""
    if(!grepl("[0-9]",date))next
    r<-direct_date_range(date,year)
    # The nearest Divi row contains this production's More Info link.
    row<-xml_find_first(block,"ancestor::*[contains(concat(' ',normalize-space(@class),' '),' et_pb_row ')][1]")
    # Use a title fragment when a unique detail URL isn't supplied. This also
    # prevents all shows on a season page becoming one series.
    link<-paste0(url,"#",gsub(" +","-",discovery_key(title)))
    e<-direct_event(cfg$name,link,title,link,html_text2(block),r$start,r$end,cfg$venue)
    result[[length(result)+1L]]<-e
  }
  result
}

fetch_direct_epac <- function(cfg,get_page) {
  home<-get_page(cfg$url);urls<-html_attr(html_elements(home,"a[href]"),"href")
  urls<-unique(xml2::url_absolute(urls[grepl("/[0-9]{4}season/?$|/events/?$",urls)],cfg$url))
  if(!length(urls))stop("EPAC current season/special event links missing")
  result<-list()
  for(url in urls)result<-c(result,parse_direct_epac(get_page(url),cfg,url))
  if(!length(result))stop("EPAC advertised dates were not recognized")
  result
}

# Preserve line breaks while selecting the performance date, which may appear
# either before '(Performances)' or after 'Performances:'.
direct_performance_excerpt <- function(excerpt) {
  lines<-trimws(strsplit(excerpt,"\n",fixed=TRUE)[[1]])
  performance<-lines[grepl("Performances?",lines,ignore.case=TRUE)]
  if(length(performance)) {
    line<-tail(performance,1)
    if(grepl("^Performances? *:",line,ignore.case=TRUE))return(sub("^Performances? *: *","",line,ignore.case=TRUE))
    return(sub(" *\\(Performances?\\).*","",line,ignore.case=TRUE))
  }
  excerpt
}

fetch_direct_servant <- function(cfg,get_json) {
  x<-get_json(cfg$url);if(is.null(x$mainContent))stop("Servant Stage upcoming page missing")
  page<-read_html(x$mainContent);cards<-html_elements(page,".summary-item");if(!length(cards))stop("Servant Stage show cards missing")
  result<-list()
  for(card in cards) {
    title<-html_text2(html_element(card,".summary-title-link"));url<-html_attr(html_element(card,"a[href]"),"href")
    if(is.na(title)||is.na(url))stop("Servant Stage title/link missing")
    url<-xml2::url_absolute(url,"https://servantstage.org")
    year<-regmatches(url,regexpr("20[0-9]{2}",url));if(!length(year))stop("No show year in Servant Stage link")
    excerpt_node<-html_element(card,".summary-excerpt")
    xml_set_text(html_elements(excerpt_node,"br"),"\n")
    excerpt<-html_text(excerpt_node)
    # Education/camp notices that do not publish performance dates are not events.
    if(grepl("Ages|Rehearsals",excerpt)&&!grepl("Performances",excerpt,ignore.case=TRUE))next
    excerpt<-direct_performance_excerpt(excerpt)
    r<-direct_date_range(excerpt,as.integer(year))
    item<-get_json(paste0(url,"?format=json"))$item
    if(is.null(item$body))stop("Servant Stage detail body missing")
    price<-if(grepl("pay.what.you.will",discovery_text(item$body),ignore.case=TRUE))"Pay-What-You-Will; check performance details" else "Not supplied by this source; check performance details"
    e<-direct_event(cfg$name,item$id,title,url,item$body,r$start,r$end,cfg$venue,price)
    e$description<-paste(e$description,"Touring locations and individual times are listed above or at the source. The advertised range is not a continuous performance.")
    result[[length(result)+1L]]<-e
  }
  result
}

# Cache public responses separately from event identity/history. It also reduces
# repeated detail-page requests when testing or rerunning a queued workflow.
direct_http <- function(root,now) {
  directory<-file.path(root,".cache","direct-http");dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  function(url,xml=FALSE,json=FALSE) {
    f<-tempfile();writeBin(charToRaw(enc2utf8(url)),f);key<-unname(tools::md5sum(f));unlink(f)
    path<-file.path(directory,paste0(key,".json"))
    cache<-if(file.exists(path))read_json(path,simplifyVector=FALSE) else NULL
    if(is.null(cache)||now-cache$checked_at>18*3600) {
      response<-fetch_page(url);cache<-list(url=url,checked_at=now,body=resp_body_string(response))
      atomic_json(cache,path)
    }
    if(json)return(fromJSON(cache$body,simplifyVector=FALSE))
    if(xml)return(read_xml(cache$body))
    read_html(cache$body)
  }
}

# Upgrade an existing installation without losing its known source records if
# a source happens to fail on the first run of the new per-source cache format.
seed_source_cache <- function(id,name,root,history,checked_at) {
  path<-file.path(root,"state","sources",paste0(id,".json"))
  if(file.exists(path))return(invisible(NULL))
  records<-unlist(lapply(history,function(e)Filter(function(o)identical(o$source,name),e$occurrences)),recursive=FALSE)
  if(!length(records))return(invisible(NULL))
  keys<-vapply(records,function(e)paste(e$source_id,e$url,e$start,e$end,sep="|"),character(1))
  records<-records[!duplicated(keys)]
  atomic_json(list(checked_at=checked_at,events=records),path)
  invisible(NULL)
}

collect_source_safely <- function(id,name,root,now,collect) {
  path<-file.path(root,"state","sources",paste0(id,".json"))
  old<-if(file.exists(path))read_json(path,simplifyVector=FALSE) else NULL
  error<-NULL
  events<-tryCatch(collect(),error=function(e){error<<-conditionMessage(e);NULL})
  if(is.null(error)) {
    atomic_json(list(checked_at=now,events=events),path);checked<-now;status<-"ok"
  } else {
    events<-if(is.null(old))list() else old$events
    checked<-if(is.null(old))NA_real_ else old$checked_at;status<-if(is.null(old))"failed" else "stale"
    message(name,": ",error)
  }
  list(events=events,health=data.frame(source_id=id,source=name,status=status,
    last_success_utc=if(is.na(checked))"" else format(as.POSIXct(checked,origin="1970-01-01",tz="UTC"),"%Y-%m-%d %H:%M UTC"),
    records=length(events),error=if(is.null(error))"" else error,stringsAsFactors=FALSE))
}

collect_direct_sources <- function(root,today,through,now) {
  get<-direct_http(root,now)
  json<-function(url)get(url,json=TRUE)
  results<-list()
  registry<-direct_source_registry()
  enabled<-discovery_preferences$direct_sources
  for(id in names(registry)) {
    if(!id %in% enabled)next
    cfg<-registry[[id]]
    results[[id]]<-collect_source_safely(id,cfg$name,root,now,function()switch(cfg$type,
      tec=fetch_direct_tec(cfg,today,through,json),squarespace=parse_direct_squarespace(json(cfg$url),cfg),
      ware=fetch_direct_ware(cfg,get),fulton=parse_direct_fulton(get(cfg$url),cfg,get),
      epac=fetch_direct_epac(cfg,get),servant=fetch_direct_servant(cfg,json),stop("Unknown adapter")))
    message(cfg$name,": ",results[[id]]$health$records," source records (",results[[id]]$health$status,")")
  }
  results
}

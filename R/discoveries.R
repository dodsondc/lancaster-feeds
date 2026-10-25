# Requires news.R (HTTP/XML helpers), conservancy.R (individual calendar writer),
# discovery-preferences.R, and libraries.R. No credentials or paid services.
discovery_city_url <- "https://tockify.com/api/feeds/rss/visitlancastercity"
discovery_pa_api <- "https://lancasterpa.com/wp-json/tribe/events/v1/events"

# Plain text only: never embed scripts, tracking images or source HTML in RSS.
discovery_text <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1])) return("")
  # Some source fields are double-encoded (e.g. Root&amp;#8217;s).
  x <- x[1]
  for (i in seq_len(3L)) {
    decoded <- html_text2(read_html(paste0("<div>", x, "</div>")))
    if (identical(decoded,x)) break
    x <- decoded
    if (!grepl("&(#?[a-zA-Z0-9]+);",x)) break
  }
  trimws(gsub("[[:space:]]+", " ", x))
}
discovery_key <- function(x) {
  x <- gsub("['’‘]", "", discovery_text(x))
  x <- tolower(iconv(x, to="ASCII//TRANSLIT", sub=""))
  trimws(gsub("[^a-z0-9]+", " ", x))
}
discovery_url <- function(x) sub("[?#].*$", "", trimws(x))
discovery_venue <- function(x, prefs) {
  x <- discovery_key(sub(",.*$", "", x))
  aliases <- prefs$venue_aliases
  if (x %in% names(aliases)) unname(aliases[[x]]) else x
}
discovery_match <- function(e, prefs) {
  if(identical(e$source,"Lancaster Libraries")) {
    policy<-library_policy(e,prefs)
    if(!is.null(policy))return(policy)
  }
  matches <- function(pattern, text) nzchar(pattern) && grepl(pattern, text, ignore.case=TRUE, perl=TRUE)
  if (matches(prefs$always_exclude_title,e$title)) return(list(keep=FALSE,reason="Explicit exclusion"))
  if (matches(prefs$always_include_title,e$title)) return(list(keep=TRUE,reason="Explicit inclusion"))
  venue_text <- paste(discovery_key(e$location), discovery_key(e$title))
  venue_hits <- function(patterns) names(patterns)[vapply(patterns,matches,logical(1),text=venue_text)]
  excluded <- venue_hits(prefs$excluded_venues)
  if(length(excluded))return(list(keep=FALSE,reason=paste("Excluded venue:",paste(excluded,collapse=", "))))
  preferred <- venue_hits(prefs$preferred_venues)
  if(length(preferred))return(list(keep=TRUE,reason=paste("Preferred venue:",paste(preferred,collapse=", "))))
  if (matches(prefs$exclude_title,e$title)) return(list(keep=FALSE,reason="Excluded title: dance"))
  if(identical(e$source,"Lancaster Libraries")) {
    types<-intersect(unlist(e$program_types),prefs$libraries$preferred_program_types)
    if(length(types))return(list(keep=TRUE,reason=paste("Library program:",paste(types,collapse=", "))))
    topics<-prefs$libraries$interests
    text<-paste(e$title,e$categories,e$description)
    found<-names(topics)[vapply(topics,matches,logical(1),text=text)]
    if(length(found))return(list(keep=TRUE,reason=paste("Library topic:",paste(found,collapse=", "))))
  }
  primary <- paste(e$title, e$categories)
  hits <- names(prefs$interests)[vapply(prefs$interests,matches,logical(1),text=primary)]
  description_hits <- names(prefs$description_interests)[vapply(prefs$description_interests,matches,logical(1),text=e$description)]
  reasons <- c(if(length(hits))paste("Title/category:",paste(hits,collapse=", ")),
    if(length(description_hits))paste("Description:",paste(description_hits,collapse=", ")))
  if (!length(reasons)) return(list(keep=FALSE,reason="No interest or preferred-venue match"))
  list(keep=TRUE,reason=paste(reasons,collapse="; "))
}

parse_discovery_city <- function(doc) {
  items <- xml_find_all(doc,"/rss/channel/item")
  if (!length(items)) stop("City calendar contains no RSS items; retaining previous feed")
  lapply(items,function(n) {
    val <- function(name) trimws(xml_text(xml_find_first(n,paste0("./*[local-name()='",name,"']"))))
    start <- publication_time(val("startdate")); end <- publication_time(val("enddate"))
    if (!is.finite(start) || !is.finite(end) || end < start) stop("Invalid city event dates: ",val("title"))
    url <- discovery_url(val("link"))
    # Tockify detail ID identifies the series; trailing number identifies a date.
    series <- sub("/detail/([0-9]+)/.*$","/detail/\\1",url)
    list(source="Visit Lancaster City",source_id=val("guid"),series=series,
      title=sub("^: *","",gsub("\\,",",",discovery_text(val("title")),fixed=TRUE)),
      description=discovery_text(val("description")),categories="",url=url,
      location=discovery_text(sub(", *https?://.*$","",val("location"))),
      price="Not supplied by this calendar; check event details",start=start,end=end,
      # City RSS does not identify all-day items reliably. Times are as supplied.
      all_day=FALSE,status="CONFIRMED")
  })
}

parse_discovery_pa <- function(events) {
  lapply(events,function(e) {
    # Use UTC fields rather than guessing the venue's timezone or DST offset.
    start <- publication_time(paste0(e$utc_start_date,"Z"))
    end <- publication_time(paste0(e$utc_end_date,"Z"))
    if (!is.finite(start)||!is.finite(end)||end<start) stop("Invalid LancasterPA dates: ",e$title)
    venue <- e$venue
    location <- if (is.list(venue)) paste(unique(Filter(nzchar,vapply(
      c("venue","address","city","state","zip"),function(k)text_value(venue[[k]]),character(1)))),collapse=", ") else ""
    url <- discovery_url(e$url)
    list(source="LancasterPA",source_id=as.character(e$id),
      series=sub("/[0-9]{4}-[0-9]{2}-[0-9]{2}/?$","/",url),
      title=discovery_text(e$title),description=discovery_text(e$description),
      categories=paste(vapply(e$categories,function(c)discovery_text(c$name),character(1)),collapse=", "),
      url=url,location=discovery_text(location),price=if(nzchar(text_value(e$cost)))discovery_text(e$cost) else "Not supplied by this calendar; check event details",
      start=start,end=end,all_day=isTRUE(e$all_day),status="CONFIRMED")
  })
}

fetch_discovery_pa <- function(today, through, get_json=function(url) resp_body_json(fetch_page(url),simplifyVector=FALSE),
  get_rss=function() resp_body_xml(fetch_page("https://lancasterpa.com/events-calendar/feed/"))) {
  build <- function(page, include=NULL) {
    r <- request(discovery_pa_api) |> req_url_query(per_page=50,page=page)
    if (is.null(include)) r <- req_url_query(r,start_date=as.character(today),end_date=as.character(through)) else r <- req_url_query(r,include=paste(include,collapse=","))
    r$url
  }
  fetch_set <- function(include=NULL) {
    first <- get_json(build(1L,include))
    if (is.null(first$events)||is.null(first$total_pages)) stop("Unrecognized LancasterPA API response")
    pages <- as.integer(first$total_pages)
    if (is.na(pages)||pages<0L||pages>40L) stop("Unexpected LancasterPA pagination; refusing a partial feed")
    events <- first$events
    if(pages>1L)for(i in seq.int(2L,pages)) {
      next_page <- get_json(build(i,include))
      if(is.null(next_page$events)||!length(next_page$events))stop("Missing LancasterPA page ",i)
      events <- c(events,next_page$events)
      Sys.sleep(0.15)
    }
    if(length(events)!=as.integer(first$total))stop("LancasterPA pagination count changed; retry collection")
    events
  }
  events <- fetch_set()
  # Date-window queries can omit long-running events that began in the past.
  # Supplement with the latest 100 RSS post IDs; include= disables API date filters.
  rss <- get_rss()
  guids <- xml_text(xml_find_all(rss,"/rss/channel/item/guid"))
  ids <- unique(sub(".*[?&]p=([0-9]+).*$","\\1",guids))
  ids <- ids[grepl("^[0-9]+$",ids)]
  ids <- setdiff(ids,vapply(events,function(e)as.character(e$id),character(1)))
  if(length(ids))for(batch in split(ids,ceiling(seq_along(ids)/50L))) events <- c(events,fetch_set(batch))
  if(!length(events))stop("No LancasterPA records returned; retaining previous feed")
  events[!duplicated(vapply(events,function(e)as.character(e$id),character(1)))]
}

# Merge only exact normalized titles at a known matching venue. A single-source
# series ID also joins occurrences. No fuzzy title or geographic inference.
group_discoveries <- function(events,prefs) {
  groups <- list()
  for(e in events) {
    e$venue_key <- discovery_venue(e$location,prefs)
    candidates <- which(vapply(groups,function(g) {
      same_series <- any(vapply(g,function(x) identical(x$source,e$source)&&identical(x$series,e$series),logical(1)))
      same_named <- nzchar(e$venue_key)&&identical(discovery_key(g[[1]]$title),discovery_key(e$title))&&identical(g[[1]]$venue_key,e$venue_key)
      # Different sources must have overlapping advertised dates to merge.
      overlap <- any(vapply(g,function(x)x$start<=e$end&&e$start<=x$end,logical(1)))
      same_series || (same_named && (overlap || all(vapply(g,function(x)identical(x$source,e$source),logical(1)))))
    },logical(1)))
    if(length(candidates)) {
      k<-candidates[1];groups[[k]]<-c(groups[[k]],list(e))
      # Bridge records can join two previously separate groups.
      if(length(candidates)>1L)for(j in rev(candidates[-1])) {groups[[k]]<-c(groups[[k]],groups[[j]]);groups[[j]]<-NULL}
    } else groups[[length(groups)+1L]]<-list(e)
  }
  groups
}

build_discoveries <- function(events,prefs,now,old=list()) {
  today <- as.Date(format(as.POSIXct(now,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d"))
  lower <- publication_time(as.character(today)); upper <- publication_time(as.character(today+prefs$horizon_days+1L))
  audit <- list(); selected<-list()
  for(e in events) {
    m<-discovery_match(e,prefs)
    if(e$end<lower||e$start>=upper)m<-list(keep=FALSE,reason="Outside date window")
    audit[[length(audit)+1L]]<-data.frame(source=e$source,title=e$title,start=format(as.POSIXct(e$start,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d %H:%M"),location=e$location,age_groups=paste(unlist(e$age_groups),collapse=", "),program_types=paste(unlist(e$program_types),collapse=", "),kept=m$keep,reason=m$reason,url=e$url,stringsAsFactors=FALSE)
    if(m$keep) {e$interest<-m$reason;selected[[length(selected)+1L]]<-e}
  }
  groups<-group_discoveries(selected,prefs); result<-list()
  for(g in groups) {
    # Stable key for a source series. Preserve prior identity when an additional
    # source appears or a source's current occurrence rolls forward.
    member_keys<-unique(vapply(g,function(e)paste(e$source,e$series,sep="|"),character(1)))
    previous<-Filter(function(e)length(intersect(unlist(e$member_keys),member_keys))>0L,old)
    id<-if(length(previous))previous[[1]]$id else {
      key<-sort(member_keys)[1]
      # Base R's MD5 utility avoids another dependency and has stable UTF-8 input.
      f<-tempfile();writeBin(charToRaw(enc2utf8(key)),f);h<-unname(tools::md5sum(f));unlink(f);paste0("discovery-",h)
    }
    starts<-vapply(g,`[[`,numeric(1),"start");ends<-vapply(g,`[[`,numeric(1),"end")
    # Prefer the richer source for description, but retain all original links.
    rank<-order(vapply(g,function(e)nchar(e$description),integer(1)),decreasing=TRUE);e<-g[[rank[1]]]
    # Deduplicate identical performances reported by two sources.
    ordered_g<-g[order(vapply(g,`[[`,logical(1),"all_day"),-vapply(g,function(x)nchar(x$description),integer(1)))]
    occurrences<-list()
    for(o in ordered_g) {
      duplicate<-any(vapply(occurrences,function(x) {
        if(identical(x$source,o$source))return(x$start==o$start && x$end==o$end)
        same_day<-format(as.POSIXct(x$start,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d")==format(as.POSIXct(o$start,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d")
        x$start==o$start || (same_day && (x$all_day||o$all_day) && x$end-x$start<=86400 && o$end-o$start<=86400)
      },logical(1)))
      if(!duplicate)occurrences[[length(occurrences)+1L]]<-o
    }
    occurrences<-occurrences[order(vapply(occurrences,`[[`,numeric(1),"start"))]
    prices<-unique(vapply(g,`[[`,character(1),"price"));known<-prices[!startsWith(prices,"Not supplied")]
    if(length(known))e$price<-paste(known,collapse="; ")
    span<-any(ends-starts>24*3600)
    kind<-if(span||length(occurrences)>=3L)"Engagements" else "Events"
    # Two performances stay separate selectable dates within one short listing.
    e$id<-id;e$member_keys<-as.list(unique(c(member_keys,unlist(lapply(previous,`[[`,"member_keys")))))
    e$kind<-kind;e$start<-min(starts);e$end<-max(ends)
    e$occurrences<-occurrences;e$sources<-lapply(g,function(x)list(name=x$source,url=x$url))
    e$sources<-e$sources[!duplicated(vapply(e$sources,`[[`,character(1),"url"))]
    e$first_seen<-if(length(previous))min(vapply(previous,`[[`,numeric(1),"first_seen")) else now
    e$interest<-paste(unique(vapply(g,`[[`,character(1),"interest")),collapse="; ")
    # Keep exact dates separately from synthetic RSS ordering dates.
    signature<-as.character(toJSON(list(title=e$title,description=e$description,occurrences=e$occurrences,sources=e$sources,price=e$price),auto_unbox=TRUE,null="null",digits=NA))
    prev<-if(length(previous))previous[[1]] else NULL
    changed<-is.null(prev)||!identical(prev$signature,signature)
    e$updated_at<-if(changed)now else prev$updated_at;e$sequence<-if(is.null(prev))0L else prev$sequence+as.integer(changed);e$signature<-signature
    result[[id]]<-e
  }
  list(events=result,audit=if(length(audit))do.call(rbind,audit) else data.frame())
}

discovery_when <- function(e) {
  local<-function(t)format(as.POSIXct(t,origin="1970-01-01",tz="America/New_York"),"%a, %b %d, %Y %I:%M %p %Z")
  if(e$all_day) paste(format(as.POSIXct(e$start,origin="1970-01-01",tz="America/New_York"),"%a, %b %d, %Y"),"— date listed; check opening/performance times") else paste(local(e$start),"–",local(e$end))
}

write_discovery_rss <- function(events,path,root) {
  old_locale<-Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME",old_locale),add=TRUE)
  Sys.setlocale("LC_TIME","C")
  headline_day<-function(t)sub(" 0([1-9])$"," \\1",format(as.POSIXct(t,origin="1970-01-01",tz="America/New_York"),"%a %b %d"))
  rss<-xml_new_root("rss",version="2.0");channel<-xml_add_child(rss,"channel")
  xml_add_child(channel,"title","Lancaster Discoveries — Events & Engagements")
  xml_add_child(channel,"link","https://visitlancastercity.com/calendar/")
  xml_add_child(channel,"description","Personal keyword-filtered discoveries from Visit Lancaster City, LancasterPA, and Lancaster County libraries. Sources overlap and coverage is incomplete. Dates sort by detection day, then event start; RSS seconds are ordering values.")
  xml_add_child(channel,"language","en-us")
  ordered<-order_feed_records(events,function(e)e$start)
  for(e in ordered) {
    item<-xml_add_child(channel,"item");body<-xml_new_root("div")
    day<-function(t)format(as.POSIXct(t,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d")
    range<-if(day(e$start)==day(e$end))day(e$start) else paste(day(e$start),day(e$end),sep=" to ")
    headline_range<-if(day(e$start)==day(e$end))headline_day(e$start) else paste(headline_day(e$start),headline_day(e$end),sep=" to ")
    xml_add_child(item,"title",paste(headline_range,e$title,sep=" — "));xml_add_child(item,"link",e$url)
    xml_add_child(item,"guid",paste0("lancaster:",e$id),isPermaLink="false")
    xml_add_child(item,"pubDate",rss_date(e$feed_sort_time));xml_add_child(item,"category",e$kind)
    for(t in c(paste(e$kind,range,sep=": "),paste("Location:",if(nzchar(e$location))e$location else "Check source"),paste("Price:",e$price),e$description))xml_add_child(body,"p",t)
    if(all(vapply(e$occurrences,function(o)identical(o$status,"CANCELLED"),logical(1))))xml_add_child(body,"p","CANCELLED — the source lists these dates as cancelled.")
    if(length(e$age_groups))xml_add_child(body,"p",paste("Library age groups:",paste(unlist(e$age_groups),collapse=", ")))
    if(e$kind=="Engagements")xml_add_child(body,"p","This is an advertised run or a group of repeated dates, not a continuous activity. The range may be clipped to the discovery window. Check the source for the complete performance schedule.")
    for(i in seq_along(e$occurrences)) {
      o<-e$occurrences[[i]];p<-xml_add_child(body,"p",discovery_when(o))
      # Never offer a multi-week calendar block, nor interpret 'all day' as a
      # verified performance time. Only explicit, <=24h timings get calendar links.
      if(!o$all_day && o$end>o$start && o$end-o$start<=24*3600) {
        stamp<-format(as.POSIXct(o$start,origin="1970-01-01",tz="UTC"),"%Y%m%dT%H%M%S")
        o$id<-paste0(e$id,"-",stamp);o$first_seen<-e$first_seen;o$updated_at<-e$updated_at;o$sequence<-e$sequence
        o$event_date<-day(o$start);o$raw_date<-discovery_when(o);o$date_source<-paste("Schedule supplied by",o$source)
        write_event_ics(o,file.path(root,"docs","calendar",paste0(o$id,".ics")))
        xml_add_child(p,"a"," Add to Calendar (.ics)",href=paste0(calendar_base_url,o$id,".ics"))
      }
    }
    for(s in e$sources) {p<-xml_add_child(body,"p");xml_add_child(p,"a",paste("Details:",s$name),href=s$url)}
    xml_add_child(body,"p",paste("Matched interests:",e$interest))
    xml_add_child(body,"p",paste("First detected:",day(e$first_seen),"· Adding a calendar item does not book a ticket. Imported calendar items do not automatically update."))
    xml_add_child(xml_add_child(item,"description"),xml_cdata(as.character(body)))
  }
  tmp<-tempfile(tmpdir=dirname(path));on.exit(unlink(tmp),add=TRUE);write_xml(rss,tmp,options="format");read_xml(tmp)
  if(!file.rename(tmp,path))stop("Cannot replace discovery RSS")
}

collect_discoveries <- function(root,now=as.numeric(Sys.time())) {
  prefs<-discovery_preferences
  today<-as.Date(format(as.POSIXct(now,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d"))
  path<-file.path(root,"state","discoveries.json")
  old<-if(file.exists(path))read_json(path,simplifyVector=FALSE)$items else list()
  # Fetch all sources successfully before replacing anything. An empty filtered
  # result is legitimate; an empty/broken source is a visible collection failure.
  city<-parse_discovery_city(resp_body_xml(fetch_page(discovery_city_url)))
  pa<-parse_discovery_pa(fetch_discovery_pa(today,today+prefs$horizon_days))
  libraries<-fetch_library_events(root,today,today+prefs$horizon_days,prefs,now=now)
  built<-build_discoveries(c(city,pa,libraries),prefs,now,old)
  write_discovery_rss(built$events,file.path(root,"lancaster_discoveries.xml"),root)
  write.csv(built$audit,file.path(root,"discovery-review.csv"),row.names=FALSE,na="")
  # Retain identity history for a year, including temporarily missing listings.
  cache<-old;for(id in names(built$events))cache[[id]]<-built$events[[id]]
  cache<-Filter(function(e)e$end>=publication_time(as.character(today-365L)),cache)
  atomic_json(list(version=1L,last_checked=now,items=cache),path)
  message("Discoveries: ",length(city)," city records + ",length(pa)," LancasterPA records + ",length(libraries)," library records → ",length(built$events)," filtered listings")
  invisible(built)
}

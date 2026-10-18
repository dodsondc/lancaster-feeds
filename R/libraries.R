# Lancaster County's shared Library Calendar. Use their age filters on the
# request, then enforce the age policy again locally (multi-tag listings exist).
library_calendar_base <- "https://calendar.lancasterlibraries.org"

library_policy <- function(e,prefs) {
  rules<-prefs$libraries
  ages<-unlist(e$age_groups,use.names=FALSE)
  if(!length(ages))return(list(keep=FALSE,reason="Library: age group missing"))
  if(any(ages %in% rules$excluded_age_groups))return(list(keep=FALSE,reason="Library: excluded youth age group"))
  allowed<-c("Adults","Seniors",if(isTRUE(rules$include_everyone))"Everyone")
  if(!any(ages %in% allowed))return(list(keep=FALSE,reason="Library: age group not selected"))
  if(isTRUE(rules$on_site_only)&&grepl("\\boff ?site\\b",discovery_key(e$location),perl=TRUE))return(list(keep=FALSE,reason="Library: off-site location"))
  programs<-unlist(e$program_types,use.names=FALSE)
  if(any(programs %in% rules$excluded_program_types))return(list(keep=FALSE,reason="Library: excluded program type"))
  if(length(rules$included_libraries)&&!e$branch %in% rules$included_libraries)return(list(keep=FALSE,reason="Library: branch not selected"))
  if(!any(ages %in% c("Adults","Seniors"))&&nzchar(rules$exclude_youth_title)&&grepl(rules$exclude_youth_title,e$title,ignore.case=TRUE,perl=TRUE))return(list(keep=FALSE,reason="Library: youth title"))
  NULL # Eligible for ordinary interest matching, not an automatic inclusion.
}

library_listing_date <- function(card) {
  text<-function(cls)html_text2(html_element(card,paste0(".lc-date-icon__item--",cls)))
  old<-Sys.getlocale("LC_TIME");on.exit(Sys.setlocale("LC_TIME",old));Sys.setlocale("LC_TIME","C")
  # Cross-month/year icons sometimes omit the day entirely. The link's
  # accessibility label supplies the complete first date for these ranges.
  label<-html_attr(html_element(card,".lc-event__title a"),"aria-label")
  date<-as.Date(NA)
  if(!is.na(label)) {
    m<-regexec(" on [A-Za-z]+, ([A-Za-z]+ [0-9]{1,2}, [0-9]{4}) @",label)
    found<-regmatches(label,m)[[1]]
    if(length(found)>1L)date<-as.Date(strptime(found[2],"%B %d, %Y",tz="UTC"))
  }
  if(is.na(date))date<-as.Date(strptime(paste(substr(text("month"),1,3),sub("[^0-9].*$","",text("day")),text("year")),"%b %d %Y",tz="UTC"))
  if(is.na(date))stop("Unrecognized library listing date; previous discovery feed retained")
  as.character(date)
}

parse_library_listing <- function(page) {
  form<-html_element(page,"#lc-calendar-filter-form")
  if(inherits(form,"xml_missing"))stop("Unrecognized library listing page")
  cards<-html_elements(page,".lc-event--upcoming > article.node--type-lc-event")
  records<-lapply(cards,function(card) {
    link<-html_element(card,".lc-event__title a[href]");url<-xml2::url_absolute(html_attr(link,"href"),library_calendar_base)
    id<-sub(".*-([0-9]+)$","\\1",url)
    if(!grepl("^[0-9]+$",id)||!startsWith(url,paste0(library_calendar_base,"/event/")))stop("Unrecognized library event link")
    date<-library_listing_date(card)
    spans<-function(cls)as.list(html_text2(html_elements(card,paste0(cls," span"))))
    branch<-html_text2(html_element(card,".lc-event__branch"));branch<-if(is.na(branch))"" else trimws(sub("^Library: *","",branch))
    programs<-spans(".lc-event__program-types")
    list(source="Lancaster Libraries",source_id=id,series=url,title=html_text2(link),
      description=discovery_text(html_text2(html_element(card,".lc-event__body"))),
      categories=paste(unlist(programs),collapse=", "),url=url,location=paste(branch,"Library"),
      price="Not supplied by this calendar; check event details",start=publication_time(date),end=publication_time(date)+86399,
      all_day=TRUE,status="CONFIRMED",age_groups=spans(".lc-event__age-groups"),program_types=programs,branch=branch)
  })
  next_url<-html_attr(html_element(page,".pager__item--next a[rel='next']"),"href")
  list(events=records,next_url=if(is.na(next_url))NULL else xml2::url_absolute(next_url,paste0(library_calendar_base,"/events/upcoming")))
}

add_library_details <- function(e,page) {
  x<-find_schema_event(page)
  e$title<-discovery_text(x$name);e$description<-discovery_text(x$description)
  e$start<-publication_time(x$startDate);e$end<-publication_time(x$endDate)
  if(!nzchar(e$title)||!is.finite(e$start)||!is.finite(e$end)||e$end<e$start)stop("Invalid library event details: ",e$url)
  # Require age tags from the detail page too; do not trust only the query filter.
  e$age_groups<-as.list(html_text2(html_elements(page,".lc-event__age-groups span a")))
  e$program_types<-as.list(html_text2(html_elements(page,".lc-event__program-types span a")))
  e$categories<-paste(unlist(e$program_types),collapse=", ")
  room<-html_text2(html_element(page,".lc-event-room"));if(is.na(room))room<-""
  loc<-x$location
  if(is.list(loc)&&!is.null(names(loc))) {
    a<-loc$address
    address<-if(is.list(a))vapply(c("streetAddress","addressLocality","addressRegion","postalCode"),function(k)text_value(a[[k]]),character(1)) else text_value(a)
    e$location<-paste(unique(Filter(nzchar,c(paste(e$branch,"Library"),room,address))),collapse=", ")
  }
  # Date-only schema values are not verified performance times.
  e$all_day<-grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",x$startDate)
  e$status<-if(grepl("Cancelled$",text_value(x$eventStatus)))"CANCELLED" else "CONFIRMED"
  if(grepl("Postponed$",text_value(x$eventStatus)))e$status<-"TENTATIVE"
  offers<-x$offers
  if(is.list(offers)&&!is.null(offers$price)) {
    price<-as.character(offers$price)
    e$price<-if(identical(price,"0"))"Free" else paste(text_value(offers$priceCurrency),price)
  }
  # Group repeated title/branch occurrences using a stable, venue-specific series.
  e$series<-paste0(library_calendar_base,"/series/",gsub(" +","-",discovery_key(paste(e$title,e$branch))))
  e
}

fetch_library_events <- function(root,today,through,prefs=discovery_preferences,
    get_page=function(url)resp_body_html(fetch_page(url)),now=as.numeric(Sys.time())) {
  path<-file.path(root,"state","library-details.json")
  cache<-if(file.exists(path))read_json(path,simplifyVector=FALSE)$items else list()
  # Use the calendar's actual selected checkbox IDs; no account or cookies needed.
  ages<-c(54L,231L,if(isTRUE(prefs$libraries$include_everyone))55L)
  query<-paste(vapply(ages,function(id)paste0("age_groups%5B",id,"%5D=",id),character(1)),collapse="&")
  branch_ids<-c("Adamstown"=89L,"Bookmobile"=237L,"Columbia"=96L,
    "Eastern Lancaster County"=97L,"Ephrata"=99L,"Lancaster"=101L,"Lititz"=103L,
    "Manheim Community"=315L,"Manheim Township"=104L,"Milanof-Schock"=105L,
    "Moores Memorial"=95L,"Mountville Branch"=102L,"Quarryville"=106L,"Strasburg-Heisler"=108L)
  wanted<-prefs$libraries$included_libraries
  if(length(wanted)) {
    ids<-unname(branch_ids[wanted])
    # Unknown names are not silently treated as 'all branches'. Update this map
    # from the site's checkbox IDs when adding another branch.
    if(anyNA(ids))stop("No verified shared-calendar branch ID for: ",paste(wanted[is.na(ids)],collapse=", "))
    query<-paste(query,paste(vapply(ids,function(id)paste0("branches%5B",id,"%5D=",id),character(1)),collapse="&"),sep="&")
  }
  url<-paste0(library_calendar_base,"/events/upcoming?",query)
  events<-list();visited<-character();pages<-0L
  repeat {
    if(url %in% visited||pages>=150L)stop("Library pagination loop/limit; refusing partial feed")
    if(!startsWith(url,paste0(library_calendar_base,"/events/upcoming?")))stop("Unexpected library next-page URL")
    visited<-c(visited,url);pages<-pages+1L
    parsed<-parse_library_listing(get_page(url));batch<-parsed$events
    if(!length(batch)&&pages==1L) {
      # A valid filter form with no cards is a legitimate empty result.
      if(!is.null(parsed$next_url))stop("Empty library page with more pages")
      break
    }
    if(length(batch)) {
      dates<-vapply(batch,function(e)as.Date(format(as.POSIXct(e$start,origin="1970-01-01",tz="America/New_York"),"%Y-%m-%d"))<=through,logical(1))
      # Verify chronological pagination before using an out-of-window page as the boundary.
      starts<-vapply(batch,`[[`,numeric(1),"start")
      if(is.unsorted(starts))stop("Library listing is no longer chronological")
      if(length(events)&&length(starts)&&starts[1]<events[[length(events)]]$start)stop("Library page ordering changed")
      events<-c(events,batch[dates])
      if(!all(dates))break
    }
    if(is.null(parsed$next_url))break
    url<-parsed$next_url
    if(pages%%10L==0L)message("Library calendar: read ",pages," listing pages")
  }
  events<-events[!duplicated(vapply(events,`[[`,character(1),"source_id"))]
  details<-0L
  for(i in seq_along(events)) {
    e<-events[[i]]
    if(!is.null(library_policy(e,prefs)))next
    old<-cache[[e$source_id]]
    if(!is.null(old)&&now-old$checked_at<prefs$libraries$detail_cache_hours*3600) {
      e<-old$event
    } else {
      e<-add_library_details(e,get_page(e$url));details<-details+1L
      cache[[e$source_id]]<-list(checked_at=now,event=e)
      if(details%%50L==0L) {
        # A network interruption should not discard hundreds of fetched details.
        # This cache checkpoint never changes the published feed or reader history.
        atomic_json(list(version=1L,items=cache),path)
        message("Library calendar: read ",details," event detail pages")
      }
    }
    events[[i]]<-e
  }
  # Cache separately from feed history; a source failure never replaces the RSS.
  cache<-Filter(function(x)x$event$end>=publication_time(as.character(today-7L)),cache)
  atomic_json(list(version=1L,items=cache),path)
  message("Library calendar: ",pages," listing pages, ",details," refreshed details, ",length(events)," records")
  events
}

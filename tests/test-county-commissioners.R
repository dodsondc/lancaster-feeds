source("R/news.R")
source("R/meetings.R")
source("R/county-commissioners.R")

fixture <- function(category, year, meeting, id, notice = "", minutes = FALSE) {
  suffix <- paste0("_", meeting, "-", id)
  agenda <- paste0("/AgendaCenter/ViewFile/Agenda/", suffix)
  minutes <- if (minutes) paste0('<a href="/AgendaCenter/ViewFile/Minutes/', suffix, '"><img alt="Minutes"></a>') else ""
  read_html(paste0('<div id="section', category, '"><ul>',
    '<li class="current"><a>', year, '</a></li><li><a href="javascript:changeYear(2026,', category, ')">2026</a></li>',
    '</ul><table id="table', category, '"><tr class="catAgendaRow">',
    '<td><h3><strong>Meeting date</strong> &mdash; ', notice, '</h3><p><a href="', agenda, '">Agenda</a></p></td>',
    '<td class="minutes">', minutes, '</td><td><a href="https://vimeo.com/example">Video</a>',
    '<a href="', agenda, '">Agenda</a><a href="/AgendaCenter/PreviousVersions/123">Previous Versions</a></td>',
    '</tr></table></div>'))
}
root <- tempfile("county-commissioners-")
dir.create(root)
feed <- file.path(root, "lancaster_county_commissioners.xml")
state <- file.path(root, "state", "county-commissioners.json")
at <- publication_time("2027-01-04T12:00:00-05:00")
amended <- FALSE
fetch_category <- function(cat) fixture(cat, 2027, "01062027", cat,
  if (amended) "Amended Jan 5, 2027 1:02 PM" else "Posted Jan 4, 2027 11:13 AM", amended)
requests <- character()
fetch_year <- function(cat, year) {
  requests <<- c(requests, paste(cat, year))
  fixture(cat, year, if (cat == "3") "12302026" else "07032026", paste0(cat, 10), minutes = TRUE)
}
first <- collect_county_commissioner_documents(root, at, fetch_category, fetch_year)
stopifnot(length(first) == 4L, identical(requests, c("3 2026", "4 2026")),
  all(vapply(first, function(r) startsWith(r$url, county_agenda_host), logical(1))))
rss <- read_xml(feed)
stopifnot(length(xml_find_all(rss, "/rss/channel/item")) == 4L,
  any(xml_text(xml_find_all(rss, "//item/title")) == "Minutes — Public Meeting — Dec 30, 2026"),
  any(grepl("Posted by county: Jan 04, 2027", xml_text(xml_find_all(rss, "//item/description")), fixed = TRUE)))
amended <- TRUE
second <- collect_county_commissioner_documents(root, at + 86400, fetch_category, fetch_year)
id <- "lancaster-county-commissioners:3:3:agenda"
stopifnot(length(second) == 6L, all(names(first) %in% names(second)),
  second[[id]]$first_seen == at, second[[id]]$activity_at == at + 86400,
  startsWith(second[[id]]$notice, "Amended by county: Jan 05, 2027"),
  any(xml_text(xml_find_all(read_xml(feed), "//item/link")) ==
      paste0(county_agenda_host, "/AgendaCenter/ViewFile/Minutes/_01062027-3")))
old_feed <- readLines(feed)
old_state <- readLines(state)
broken <- function(cat) if (cat == "4") read_html("<p>Invalid response</p>") else fetch_category(cat)
stopifnot(inherits(try(collect_county_commissioner_documents(root, at, broken, fetch_year), silent = TRUE), "try-error"),
  identical(old_feed, readLines(feed)), identical(old_state, readLines(state)))
unlink(root, recursive = TRUE)
cat("County Commissioners checks passed: direct PDFs, duplicate menu links, six-month cutoff across New Year, late minutes, amended agenda identity, and failure retention.\n")

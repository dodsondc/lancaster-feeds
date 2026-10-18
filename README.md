# Lancaster community feeds

An R collector for Lancaster Township news, intended for personal reading in NetNewsWire.

## Local use

Install the four dependencies once:

```r
install.packages(c("httr2", "rvest", "xml2", "jsonlite"))
```

Run from anywhere:

```sh
Rscript '/Users/David/Documents/Codex/RSS/Feed Maker.R'
```

Or source `Feed Maker.R` in RStudio. Do not run the helper `R/news.R` by itself.

## What the feed contains

The collector reads the township's public news index and retrieves the `#post` article content. It removes scripts and administration controls, resolves links/images to absolute URLs, decodes title entities, and keeps a stable URL-based GUID for each story.

The collector follows only the first 10 entries of the JSON index: the same stories shown on page 1 of newslist.php. It follows their Read More links for article text.

Explicit publication metadata takes priority for newly collected articles. Otherwise, month/day dates from the newest-first listing receive an estimated year: start with the current Eastern calendar year, step back if the result is in the future, and step back at a December/January rollover while walking down the list. Existing verified/estimated dates anchor the sequence and remain stable across runs. These are estimates: pinned/reordered stories or a listing stale for more than a year can violate the assumptions. Image timestamps and dates in article prose are not used.

Estimated dates appear as “Publication date estimated from listing” with no invented time of day; RSS uses local midnight. If the listing date is missing or invalid, the item uses its first collection time and is labeled accordingly. Every record retains first_seen in state. The update migrates existing first-collected records on page 1 to estimated dates without resetting first_seen or changing URL-based GUIDs. Do not delete or replace GitHub's state file when updating the code. Old imported items may remain in NetNewsWire's own history even after they leave the active feed.

Existing article bodies are cached for 24 hours; newly discovered stories are fetched immediately. A failed index or article request leaves the previous feed and state intact and fails the run visibly. Empty article bodies link back to the original announcement. Stories absent from the current index leave the active RSS feed, while their collection history remains in state. Updated bodies retain the same GUID; whether an update is shown as unread depends on the reader.

Keep the state file when moving to GitHub. Removing it resets fallback collection dates.

## Publish through GitHub, using your browser

1. Create a **public** repository named `lancaster-feeds`, using `main` as the default branch. Initialize it with a README. The code, feed and state will be publicly readable.
2. Upload `Feed Maker.R`, `R/`, `tests/`, `state/`, `lancaster_township_news.xml`, this README and `.gitignore` to the repository root. Upload their contents at the root, not inside an extra RSS folder. Do not upload `.Rhistory` or `.RData`.
3. Use Add file > Create new file. Name it `.github/workflows/refresh-news.yml`. Paste the supplied workflow YAML and commit it to `main`. (Uploading the `.github` directory with Git also works.)
4. In Settings > Actions > General, allow GitHub Actions and the actions used by the workflow. The workflow requests Contents write permission so it can commit the generated XML and state. If repository policy prevents that, enable Read and write permissions under Workflow permissions. No personal access token is needed.
5. In Actions, choose **Refresh township news**, then **Run workflow** on `main`. Check that its run is green, including the Publish step. The first run may spend several minutes installing R and dependencies.
6. Open `lancaster_township_news.xml` in the repository and click **Raw**. Copy the resulting URL. It normally looks like:

   `https://raw.githubusercontent.com/YOUR-USERNAME/lancaster-feeds/main/lancaster_township_news.xml`

   This is the subscription URL. GitHub Pages is not needed.
7. On your iPhone, open NetNewsWire, choose Add Feed, and paste the raw URL. Verify that it shows Lancaster Township News and opens article/document links correctly.

The workflow checks at 11:17 and 23:17 UTC daily: 7:17 AM/PM Eastern daylight time, or 6:17 AM/PM Eastern standard time. GitHub scheduling and reader refreshes can be delayed. It can also be run manually. Each successful run commits collection state, recording activity even when there is no new story. GitHub can disable schedules in public repositories after 60 days without repository activity; re-enable under Actions if needed.

Keep GitHub failure notifications enabled. No failure emails are sent by this code itself. A green local test does not prove that the GitHub workflow has run; verify the first cloud run and the public URL.

## Checks

From the repository root:

```sh
Rscript tests/test-news.R
```

These offline checks cover publication dates, Eastern midnight conversion, HTML cleanup, relative URLs including spaces, first-collected dates across runs, year rollovers and persistent estimates, migration of old state, the 10-item limit, and retention of the last good feed on failure.

## Future source types

- News: title, publication date (or labeled first-collection date), text, source URL.
- Event: title, description, date/time, location, price.
- Engagement: title, description, start/end dates, location, price; one item per run of a production.

Only news is implemented so far.

## Meeting-document feed

`R/meetings.R` creates a second feed, `lancaster_township_meetings.xml`, with independent state in `state/township-meetings.json`. It covers Board of Supervisors, Planning Commission, Zoning Hearing Board, and Traffic Committee pages. It includes one item per Agenda, Minutes, or Packet PDF for meetings within the past 366 days, plus future meetings listed by the township. There is a one-time backlog on first subscription (58 PDFs in the initial local run). It ignores video links and canceled-meeting rows without PDFs.

Each title includes document type, body and the explicit meeting date. The item date is first detection, because meeting dates are not document publication dates. Links open the township's PDFs; files are not downloaded, archived, or text-extracted. URLs honor each page's HTML base element and encode spaces. Query timestamps do not change document identity; an altered link updates the existing item's target. Revisions to PDF bytes at an unchanged URL are not detected. NetNewsWire controls whether changes to an existing item appear unread.

The existing workflow runs and tests both collectors. If a source fails, its collector preserves its last good output and reports a failed run; successful output from the other collector can still be committed. GitHub failure notifications remain useful.

### Update an existing GitHub repository

Upload the new `Feed Maker.R`, `R` and `tests` folders, and README to the root. Keep the folder hierarchy (including `tests/fixtures`). Do not replace your existing news state with a local copy. Replace `.github/workflows/refresh-news.yml` with the supplied updated workflow, using GitHub's editor; this adds the meeting test and commits the new XML and state automatically. Run the workflow manually after all files are uploaded and confirm a green result.

Then add this second subscription to NetNewsWire:

`https://raw.githubusercontent.com/dodsondc/lancaster-feeds/main/lancaster_township_meetings.xml`

No change to the existing news subscription is needed. Local test command: `Rscript tests/test-meetings.R`.

## Feed ordering

Both feeds sort by Eastern calendar day of first detection, descending, then by publication date (news) or meeting date (documents), descending. Stable document IDs/URLs break remaining ties deterministically. Since NetNewsWire sorts using RSS pubDate and cannot be told a custom secondary key, the generated pubDate encodes that order using seconds after midnight on the detection day. These seconds are sorting values, not actual publication/detection times. Actual publication estimates, meeting dates, and full first-detection timestamps remain in the item descriptions and saved state. No identifiers or saved detection dates are reset. Existing items' RSS dates will change on the first refresh with this version.

Upload R/news.R, R/meetings.R, tests/test-news.R, tests/test-meetings.R and this README to update sorting. Keep existing state and workflow. NetNewsWire should be set to show newest first; existing cached history may not change until the feed is refreshed and updated entries are accepted by the reader.

## Lancaster Conservancy events and calendar links

`R/conservancy.R` creates `lancaster_conservancy_events.xml`, `state/conservancy-events.json`, and individual iCalendar files under `docs/calendar/`. Each RSS item links to the event's registration/details page and its own `.ics` file. GitHub Pages serves those files at `https://dodsondc.github.io/lancaster-feeds/calendar/`.

Collection starts from the Conservancy's own event listing. Public Eventbrite JSON-LD supplies complete start/end dates, locations and prices for linked events without signing into an account. The workflow setting `CONSERVANCY_EVENTBRITE: 'true'` controls this: set it to `'false'` to make no requests to Eventbrite. The source-only mode still includes registration links, titles, descriptions, month/day dates and listed times; years are inferred from nearby matching month/day/weekday values, and unavailable addresses/prices are labeled. Clicking registration links still visits the linked service.

The collector lists events on or after today's Eastern date. Registration detail failures leave the previous feed/calendar files in place rather than silently inventing details. Structured dates override estimated years; mismatched month/day dates stop publication. Calendar UIDs and RSS identifiers stay stable for Eventbrite events; meaningful detail changes increment the calendar SEQUENCE and update LAST-MODIFIED. Imported calendar entries are individual copies: they do not automatically stay synchronized or register you. Missing-time events are explicitly labeled tentative date reminders, with a transparent all-day placeholder and an exclusive next-day end date. Listing-only times/years remain tentative and labeled. There are no default alarms or attendee invitations.

Calendar files follow RFC 5545: CRLF line endings, UTF-8 octet-aware folding, text escaping, stable UIDs, UTC start/end times converted from Eastern time, and separate date-only reminders. A review/confirmation step in Calendar is expected. iPhone/Safari handling of a hosted `.ics` link still needs an end-to-end test after deployment; valid calendar files alone do not guarantee every browser opens the same import UI.

### GitHub update for Conservancy

1. Upload the new `Feed Maker.R`, `R`, `tests` (including fixtures), `docs`, and README to the repository root. Do not replace existing GitHub state or township XML files with local copies.
2. In repository Settings > Pages, choose **GitHub Actions** as Source and save if prompted. This is one-time setup for the calendar-file URLs; the RSS feeds keep using their existing raw URLs.
3. Replace `.github/workflows/refresh-news.yml` with the supplied updated workflow. It adds the Conservancy tests/collector, commits generated output, and deploys `docs` using official Pages actions. The workflow requests Pages write and OIDC permissions for deployment; no personal token or account credential is needed.
4. Commit, let the automatic run finish, and check the Deploy calendar files step is green. If the Pages environment prompts for deployment approval, approve it in GitHub. If a run starts before all files/settings are ready, run it again once setup is complete.
5. Add `https://raw.githubusercontent.com/dodsondc/lancaster-feeds/main/lancaster_conservancy_events.xml` to NetNewsWire.
6. Test one Add to Calendar link. Verify date, time and location in the import preview before saving. On Mac, a browser may download the `.ics` file; open that file in Calendar. On iPhone, report whether the link opens Calendar or shows/downloads a file so we can verify the delivery behavior.

Offline checks: `Rscript tests/test-conservancy.R`. All scraping and calendar generation remains R; Python's icalendar package was used only as an independent development-time file-format check and is not required by the workflow.

## Lancaster Discoveries (Visit Lancaster City + LancasterPA)

`lancaster_discoveries.xml` is a separate, personal-interest feed. `R/discoveries.R`
reads Visit Lancaster City's Tockify RSS schedule and LancasterPA's public Events
Calendar API. The latter is paginated completely for the next 90 days. Latest RSS
post IDs supplement that window to find ongoing runs that started earlier. This
is a discovery aid, not a complete Lancaster calendar: sources omit events, the
ongoing-event supplement covers only their latest 100 RSS posts, and dates and
prices can be incomplete or incorrect.

Edit **`R/discovery-preferences.R`** to change the horizon, interest patterns,
exclusions, preferred/omitted venues, description patterns, title overrides, or venue aliases. Patterns are case-insensitive R
regular expressions. Titles/categories use `interests`; descriptions use the more
specific `description_interests`. Both can qualify a listing. Descriptions never
trigger venue rules or dance exclusions. Preferred venues (Fulton Theatre, Ware
Center, Demuth Museum, LancasterHistory) bypass ordinary keyword and dance-title
rules. Omitted venues are listed in `excluded_venues`, including the recurring
markets/attractions and restaurants David chose to omit. Venue patterns match
normalized location names or titles, so recognizable venue names can still work
when the location field is missing. Source misspellings such as "Demuth Musuem"
and encoded apostrophes in Root's are handled.

Precedence: explicit title exclusion, explicit title inclusion, omitted venue,
preferred venue, dance-title exclusion, then title/category or description interests.
Thus a title inclusion can allow a one-off exception at an otherwise omitted venue.
Use empty strings to disable title overrides. `discovery-review.csv` records every
source listing's decision, including those outside the date window. Review this
file before broadening terms. No location/radius or age restrictions are inferred.

Deduplication is conservative: the same source series, or the same normalized title
at a known matching venue. Cross-source merges also require overlapping dates.
Verified venue aliases are editable. Different titles and unknown/differently named
venues can still leave duplicates; fuzzy guesses are deliberately avoided. All
matched source URLs survive a merge. Identical performance times are collapsed;
a precise timed occurrence is preferred over a same-day all-day duplicate.

Three or more occurrences, or an explicitly advertised range longer than 24 hours,
become one **Engagement**. The original known dates and gaps remain visible. The
range may reflect only dates returned within the 90-day window; a continuous
multi-day listing does not establish daily performance times. One or two dates
are listed as **Events**. Each verified timed occurrence lasting at most 24 hours
has an individual Add to Calendar link; date-only and multi-day spans have source
links instead of misleading continuous calendar reservations. Imported calendar
items do not update automatically, and calendar imports do not reserve tickets.

State is kept in `state/discoveries.json`: series membership, stable GUIDs,
first-detection dates, and revisions. History lasts one year. Existing story GUIDs
survive rolling occurrences and additional matching sources. RSS ordering follows
our existing detection-day / event-date convention; displayed schedules are real
source dates, while RSS seconds after midnight are synthetic ordering values.
A broken source retains the previous feed and fails visibly; a successful collection
with zero keyword matches legitimately creates an empty feed.

The scheduled workflow builds this feed and its calendar files alongside the three
existing feeds. Subscribe after the first successful run:

`https://raw.githubusercontent.com/dodsondc/lancaster-feeds/main/lancaster_discoveries.xml`

Run just this collector locally with:

```r
source('R/news.R')
source('R/conservancy.R')
source('R/discovery-preferences.R')
source('R/discoveries.R')
collect_discoveries(getwd())  # working directory must be this project
```

Run `Rscript tests/test-discoveries.R` from the project root for offline checks.
Lancaster Libraries is the next source to evaluate; it is not included yet.

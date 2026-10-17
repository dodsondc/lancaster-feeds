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

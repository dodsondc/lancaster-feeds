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

Only explicit publication metadata is treated as a publication date. The township's index provides month and day but no year. Image timestamps and dates mentioned in article text are not publication dates. If no verified publication date is available, the RSS date is the first successful collection time and the description says so. That date is preserved in `state/lancaster-township.json`, not regenerated on each run. The first import includes the existing archive; mark that backlog read if desired.

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

These offline checks cover publication dates, Eastern midnight conversion, HTML cleanup, relative URLs including spaces, first-collected dates across runs, and retention of the last good feed on failure.

## Future source types

- News: title, publication date (or labeled first-collection date), text, source URL.
- Event: title, description, date/time, location, price.
- Engagement: title, description, start/end dates, location, price; one item per run of a production.

Only news is implemented so far.

# Run with Rscript '/path/to/Feed Maker.R', or source this file in RStudio.
# No setwd(): paths are relative to this script, not the current working directory.
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
script <- if (!is.null(source_file)) source_file else if (length(script_arg)) sub("^--file=", "", script_arg[1]) else "Feed Maker.R"
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- dirname(normalizePath(script, mustWork = TRUE))
source(file.path(root, "R", "news.R"))
source(file.path(root, "R", "meetings.R"))
source(file.path(root, "R", "conservancy.R"))
# Independent collectors: a document-page failure should not prevent news updates.
failures <- character()
for (collector in c("collect_township_news", "collect_meeting_documents", "collect_conservancy_events")) {
  tryCatch(get(collector)(root), error = function(e) {
    failures <<- c(failures, paste(collector, conditionMessage(e), sep = ": "))
  })
}
if (length(failures)) stop(paste(failures, collapse = "\n"))

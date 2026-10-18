# Edit these regular expressions to tune the Discoveries feed. Case-insensitive.
# Titles/categories and descriptions have separate patterns; venue rules take priority.
discovery_preferences <- list(
  horizon_days = 90L,
  interests = list(
    Concerts = "\\b(concerts?|orchestra|symphony|chamber music|jazz|recital|choir|choral|marimba|live music|music festival)\\b",
    Arts = "\\b(art walk|artwalk|art exhibits?|art exhibitions?|gallery|galleries|museum|theat(er|re)|plays?|musicals?|shakespeare|improv|poetry|film screening)\\b",
    Tours = "\\b(walking tours?|guided tours?)\\b",
    History = "\\b(history|historical|heritage|archaeology|historic tour|historic rock ford|lancasterhistory)\\b",
    Environment = "\\b(conservancy|conservation|ecology|ecological|watershed|wildlife|birding|birdwatching|native plants?|nature walk|nature hike|naturalist|environmental)\\b"
  ),
  # More specific phrases for descriptions: avoid broad words such as 'historic'
  # that frequently appear in venue biographies and promotional boilerplate.
  description_interests = list(
    Concerts = "\\b(live music|chamber music|jazz concert|choral concert|symphony orchestra)\\b",
    Arts = "\\b(musicals?|theatrical production|art exhibition|art exhibit|film screening)\\b",
    Tours = "\\b(walking tours?|guided tours?)\\b",
    History = "\\b(history lecture|historical lecture|history talk|historical tour)\\b",
    Environment = "\\b(native plants?|birdwatching|birding walk|nature walk|nature hike|watershed restoration)\\b"
  ),
  # These patterns match normalized venue names OR titles, never descriptions.
  # Preferred venues bypass ordinary keyword and dance-title rules.
  preferred_venues = c(
    "Fulton Theatre" = "\\bfulton theat(re|er)\\b",
    "Ware Center" = "\\b(ware center|ware centre)\\b",
    "Demuth Museum" = "\\bdemuth\\b",
    "LancasterHistory" = "\\blancaster ?history\\b"
  ),
  excluded_venues = c(
    "Green Dragon" = "\\bgreen dragon\\b",
    "Eden Resort and Suites" = "\\beden resort\\b",
    "Lancaster Central Market" = "\\blancaster central market\\b",
    "Old Windmill Farm" = "\\bold windmill farm\\b",
    "Plough Restaurant" = "\\bplough restaurant\\b",
    "Roots Country Market" = "\\broots? country market\\b",
    "Strasburg Railroad" = "\\bstrasburg (rail ?road|railway)\\b",
    "The Barn at Paradise Station" = "\\b(the )?barn at paradise station\\b"
  ),
  # Exclude on TITLE only: a concert description mentioning dancing is still OK.
  exclude_title = "\\b(dance|dancing|ballet|ballroom|salsa|contra dance)\\b",
  # Explicit title overrides. Empty strings mean no override.
  always_include_title = "",
  always_exclude_title = "",
  # Verified aliases only, for matching differently named venues across sources.
  venue_aliases = c("ware center for the arts" = "ware center", "the ware center" = "ware center",
    "historic rock ford plantation" = "historic rock ford", "rock ford plantation" = "historic rock ford")
)

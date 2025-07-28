tryCatch({
  source('app.R')
  cat('SUCCESS: App loaded correctly\n')
}, error = function(e) {
  cat('ERROR:', e$message, '\n')
})

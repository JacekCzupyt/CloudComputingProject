app <- ShinyDriver$new("../../")
app$snapshotInit("load_files_filter_dates_move_arrow")

app$uploadFile(files = "../../../StreamingHistory0.json") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(lato = "click")
app$snapshot()

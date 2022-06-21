app <- ShinyDriver$new("../../")
app$snapshotInit("app_full_scenario")

app$uploadFile(files = "~/Studia/repo/Studia/R/TechnikiWizualizacjiDanych/Projekt2/StreamingHistory0.json")
app$snapshot()

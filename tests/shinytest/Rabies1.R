app <- ShinyDriver$new("../../")
app$snapshotInit("Rabies1")

app$uploadFile(tree_file_w_transitions = "../../input/batRABV.MCC.trees") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(distances_file = "../../input/predictors/bodySize.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(sampling_locations = "../../input/hostnames.txt") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(start = "click")
app$snapshot()
app$setInputs(exclude_toggle = "click")
app$setInputs(exclude_reset = "click")
app$setInputs(exclude_toggle = "click")
app$setInputs(exclude_reset = "click")

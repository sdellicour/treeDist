app <- ShinyDriver$new("../../")
app$snapshotInit("Influenza1")

app$uploadFile(tree_file_w_transitions = "../../input/h3_small_sample.MCC.tre") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(distances_file = "../../input/subsetDeff.txt") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(sampling_locations = "../../input/sampling_locations.txt") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(Annotation_State = "states")
app$setInputs(delimiter = "	")
app$setInputs(start = "click")
app$setInputs(exclude_toggle = "click")
app$setInputs(exclude_reset = "click")
app$snapshot()

# Build the WorldView static-app data assets from WVS Wave 7.
# Run from the repository root with: Rscript build_worldview.R

source(file.path("R", "worldview_build.R"))
build_worldview()

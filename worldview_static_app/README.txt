WorldView static application: Step 6

Contents
- index.html
- assets/styles.css
- assets/app.js
- data/* public WorldView assets

Run locally from RStudio

Option 1: using the servr package

  install.packages("servr")  # only if needed
  servr::httd("worldview_static_app", browser = TRUE)

Option 2: using the httpuv package

  install.packages("httpuv") # only if needed
  httpuv::runStaticServer("worldview_static_app")

Do not open index.html directly from the file system. Browser security rules can
block fetch() from loading the JSON files when the page uses a file URL.

Checks
1. Home page shows participant, country, and variable counts.
2. All navigation links change pages.
3. Codebook reports 29 of 29 variables shown.
4. Searching for Q167 shows the hell-belief variable.
5. Filtering Topic to Demographics shows six variables.
6. Download buttons start CSV downloads.


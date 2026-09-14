# Renders each lecture .Rmd a second time as a plain scrollable
# html_document ("lesson notes"), in addition to its ioslides deck.
# Run this after `rmarkdown::render_site()` (so the navbar in _site.yml is
# current) any time a lecture .Rmd's content changes.
#
# Usage: Rscript render-notes.R                # renders all 7 lectures
#        Rscript render-notes.R <lecture-stem> # renders just one (used
#                                               # internally, below)
#
# Each lecture is rendered in its own fresh Rscript subprocess, not in a
# shared loop within one R session -- rendering several html_document
# pages back-to-back in one session (particularly lecture 6, which uses
# leaflet/mapview widgets) has intermittently hit a
# "path for html_dependency not found" error from stale widget dependency
# state carried over between renders.

lectures <- c(
  "01-github",
  "02-file_management_and_workflow",
  "03-metadata",
  "04-R-functional",
  "05-super-functional",
  "06-data-visualization",
  "07-reports-depends"
)

render_one <- function(lec) {
  input <- paste0(lec, ".Rmd")
  output_file <- paste0(lec, "-doc.html")
  message("Rendering ", input, " -> docs/", output_file)
  rmarkdown::render(
    input,
    output_format = rmarkdown::html_document(
      toc = TRUE,
      toc_float = TRUE,
      theme = "united",
      css = c("classes-styles.css", "docs-notes.css")
    ),
    output_file = output_file,
    output_dir = "docs",
    params = list(is_slides = FALSE),
    quiet = TRUE,
    envir = new.env()
  )
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 1) {

  render_one(args[1])

} else {

  for (lec in lectures) {
    status <- system2("Rscript", c("render-notes.R", lec))
    if (status != 0) stop("Rendering ", lec, " failed (exit status ", status, ")")
  }

}

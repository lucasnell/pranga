# Renders each lecture .Rmd a second time as a plain scrollable
# html_document ("lesson notes"), in addition to its ioslides deck.
# Run this after `rmarkdown::render_site()` (so the navbar in _site.yml is
# current) any time a lecture .Rmd's content changes.
#
# Usage: Rscript render-notes.R

lectures <- c(
  "01-github",
  "02-file_management_and_workflow",
  "03-metadata",
  "04-R-functional",
  "05-super-functional",
  "06-data-visualization",
  "07-reports-depends"
)

for (lec in lectures) {
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

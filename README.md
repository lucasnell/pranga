# pranga
Named for *Parambassis ranga* - a transparent fish with a high reproductive rate

## Building the site

Each lecture (`01-github.Rmd` ... `07-reports-depends.Rmd`) renders to two
pages: the ioslides deck (click-through slides) and a plain scrollable
"lesson notes" page (`<lecture>-doc.html`) from the same source, controlled
by the `params$is_slides` flag in each file's `source-html` chunk (that
chunk loads jQuery + footnote-positioning JS that only does anything inside
ioslides' `<slide>` markup; left on for the doc version it silently breaks
the doc page's own table-of-contents JS).

To rebuild after editing content:

```r
rmarkdown::render_site()   # slide decks + index/syllabus/lessons, per _site.yml
Rscript render-notes.R     # the *-doc.html "lesson notes" versions
```

Always run both, in that order, even for a change that seems unrelated to
the lesson-notes pages (e.g. editing `index.Rmd`). `rmarkdown::render_site()`
deletes any file already in `docs/` that it doesn't recognize as one of its
own outputs -- which includes every `*-doc.html` page -- so it silently
wipes all seven lesson-notes pages on every run, not just when the navbar
changes. `Rscript render-notes.R` regenerates them again afterward.

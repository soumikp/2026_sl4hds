# Handoff notes — folder moved from Research/ to Teaching/ (2026-08-17)

This folder was moved from `Research/2026_sl4hds` to `Teaching/2026_sl4hds`,
replacing a stale checkout of the same repo that was 16 commits behind and
had no unique uncommitted work. The prior Claude Code chat that did most of
this work is not resumable at the new path — the transcript is preserved
on disk keyed to the old path, but a fresh session here starts without it.

## What the prior session did, roughly chronologically

1. Synced local scaffold with the real course content from GitHub
   (`soumikp/2026_sl4hds`).
2. Converted early Shiny-app-based labs (02-training, 03-regression-1) to
   the same `.Rmd`/`.qmd` lab format used elsewhere.
3. Standardized all labs to a `cosmo` theme, then converted **all labs to
   live, in-browser R via webR** (`quarto-live` extension,
   `_extensions/r-wasm/live/`), format: `live-html`. 80vw width, 16pt/28pt/14pt
   font sizing for classroom projection. Custom "Download my code" button
   (`_extensions/r-wasm/live/resources/download-code.js`).
   - Lecture 07 (k-means/GMM lab) kept static (`.Rmd`) since it depends on
     `mclust`, which isn't in webR's package repo.
   - Fixed a data-loading bug: local CSVs for webR labs must live in a
     `data/` folder next to the `.qmd`, and need **both** a top-level
     `resources:` key (Quarto's site-asset copy) **and** a `webr: resources:`
     key (webR's virtual filesystem fetch) — missing the top-level one silently
     drops the file from the built site.
4. Removed a broken GitHub Actions render workflow (never had R installed,
   was failing on every push). Site deploys from `docs/` on `main` via
   GitHub Pages' legacy branch-based source, independent of any CI.
5. Hid all unreleased lectures (02 through the newly added 12) from the live
   site: excluded from `_quarto.yml`'s `render:` list (so URLs 404, matching
   the existing `!assignments/hw*.qmd` pattern) and scoped the top-level
   `resources:` copy rules down to `lectures/01-intro/` only, so no stray
   PDFs/code/data leak out for hidden lectures either. `schedule.qmd` shows
   plain text (no links) for hidden lecture rows, mirroring how the
   Assignments page already hides unreleased homework.
6. Restructured grading per instructor request:
   - Added a 12th lecture, "Multiple Hypothesis Testing" (Nov 13,
     `lectures/12-hyptest/`, currently a placeholder — also hidden).
   - Nov 20 marked "No class — prepare for project."
   - Grading table: Quizzes 10% (12 quizzes, 5 questions each, administered
     on Canvas — no on-site quiz content needed — lowest score dropped),
     Homework 60% (expanded from 3 assignments to 6, ~10% each, roughly one
     per 2 lecture topics, addressing feedback that HW was too infrequent),
     Final project 30% (up from 25%).
   - See `syllabus.qmd` (`## Grading`, `### Quiz policy`) and
     `assignments/index.qmd` for the authoritative current text.
7. Simplified the class 1 (`lectures/01-intro/`) landing page: going forward
   every lecture will link only **Clean slides** (up before class) and
   **Annotated slides** (up after class) — no more 2-up print PDF. Removed
   the Code/Data/Extras subsections and their build-output copying for
   lecture 01; this is meant to be the template for all future lectures.

## Known pre-existing quirks (not from this session, don't re-attempt fixes)

- Two "Zhang and Castelló (2017)" reading PDFs show up as untracked files
  in `git status` under `lectures/09-pca/readings/` (and its nested
  `readings/readings/` duplicate). This is a macOS APFS Unicode
  normalization collision (composed vs. decomposed "é") — the tracked and
  untracked paths look identical but aren't, byte-for-byte. `rm`/`git
  checkout` by filename will target the wrong one. Leave alone; needs
  manual Finder-based cleanup if ever addressed.
- `figures/figures`, `readings/readings`, `data/data` nested duplicate
  directories exist in most lecture folders (leftover from an earlier
  `code/code` duplication that was cleaned up) — deferred by explicit
  instructor choice ("leave for later"), not yet cleaned up.
- `_quarto.yml` `site-url` and some homework release/due dates are still
  placeholders ("TBD") — deferred early in the original session, never
  circled back to.

## Repo basics

- GitHub: `soumikp/2026_sl4hds`, single branch `main`.
- Quarto website project, `output-dir: docs`, GitHub Pages serves `docs/`
  on `main` directly (no CI needed — just `quarto render` + commit + push).
- Rendering the whole site: `quarto render` from repo root.

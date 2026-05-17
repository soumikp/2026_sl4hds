# BIOST 2155 lecture scaffold

This bundle gives you the 11 missing lecture pages plus a corrected `schedule.qmd`. Drop it into your repo, add your PDFs, push.

## What's in here

```
lectures/
  01-intro/
    index.qmd
    code/         (empty)
    data/         (empty)
    figures/      (empty)
    readings/     (empty)
    extras/       (empty)
  02-training/    ... same structure
  03-regression-1/
  04-regression-2/
  05-classification-1/
  06-classification-2/
  07-clustering-kmeans/
  08-clustering-gmm/
  09-pca/
  10-trees/
  11-ensemble/
schedule.qmd      (replacement for current file)
```

Each `index.qmd` has YAML front matter, a short intro paragraph, four sections (Materials / Code & lab / Readings / Extras), and HTML-commented placeholders you uncomment as you add files.

## How to drop in

From the repo root:

```bash
# Back up first
cp schedule.qmd schedule.qmd.bak

# Unzip on top of the repo
unzip -o sl4hds_scaffold.zip
```

The zip's structure (`lectures/...` and `schedule.qmd` at top level) matches the repo, so `unzip -o` overlays cleanly.

## Then add your slides

For each lecture you have a PDF for:

```bash
cp ~/path/to/your/lec01.pdf lectures/01-intro/slides.pdf
cp ~/path/to/your/lec02.pdf lectures/02-training/slides.pdf
# ...etc
```

The `index.qmd` files already link to `slides.pdf` in the same folder, so once the file exists the link works.

## Rebuild and check locally

```bash
quarto render
```

This populates `_site/`. Open `_site/index.html` in a browser and click through every schedule link — verify each lecture page loads and each `slides.pdf` opens.

## Push

```bash
git add lectures/ schedule.qmd
git commit -m "Add lecture scaffolds and slide PDFs"
git push
```

If `git push` fails with a size error or LFS message, see the diagnostic step in the chat — we'll fix it before re-pushing.

## Things I changed vs. your live site

1. **Dec 5 row in the schedule** now reads "In-class final project presentations" instead of "No class," matching the syllabus and project page.
2. **Nov 7** stays "No class" — the syllabus listed ICA there but you skipped that topic this cycle.

If either of those is wrong, just edit `schedule.qmd` before rendering.

## Things I left alone

- `_quarto.yml` — flag from the chat: your `site-url` placeholder is `your-username.github.io/biost2155/` but the real URL is `soumikp.github.io/2026_sl4hds/`. Fix when convenient; it affects RSS, sitemap, and OG tags.
- HW page release/due dates ("TBD") — also worth filling in now that they're known.
- `set.seed(2155)` vs. `set.seed(42)` mismatch between HW pages and the PDFs — pick one.

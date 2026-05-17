"""
Generate per-lecture index.qmd files from the contents of each lecture folder.

For each lectures/<slug>/:
  - reads slides.pdf, code/, data/, figures/, readings/, extras/
  - writes index.qmd with appropriate sections, omitting empty ones.

Per-lecture metadata (title, date, ISLR reading) is hard-coded here, since it
isn't recoverable from filenames alone.
"""

from pathlib import Path
import re

# Resolve relative to this script so it works wherever the project lives
ROOT = Path(__file__).resolve().parent / "lectures"

# Per-lecture metadata. Order matches syllabus.
LECTURES = [
    dict(slug="01-intro",
         title="Introduction",
         subtitle="Supervised vs. unsupervised learning",
         date="Aug 29, 2025",
         reading="ISLR ch. 2",
         summary=("Course overview and the landscape of statistical learning. "
                  "We distinguish supervised problems (regression, classification) "
                  "from unsupervised ones (clustering, dimensionality reduction), "
                  "and motivate the rest of the semester through health-science examples.")),
    dict(slug="02-training",
         title="Training statistical learning models",
         subtitle="Bias-variance, validation, resampling",
         date="Sep 5, 2025",
         reading="ISLR §2.2, ch. 5",
         summary=("Under- and over-fitting; the training/validation/test split; "
                  "k-fold cross-validation and the bootstrap. Foundations for every "
                  "method we'll see later in the course.")),
    dict(slug="03-regression-1",
         title="Regression I",
         subtitle="Subset selection and shrinkage",
         date="Sep 12, 2025",
         reading="ISLR ch. 3, 6",
         summary=("Linear regression as a baseline; best-subset and stepwise selection; "
                  "ridge regression and the lasso as principled alternatives when p is "
                  "large or predictors are correlated.")),
    dict(slug="04-regression-2",
         title="Regression II",
         subtitle="Non-linear regression, splines, and the kernel trick",
         date="Sep 19, 2025",
         reading="ISLR ch. 7",
         summary=("Polynomials, step functions, regression splines, smoothing splines, "
                  "and an introduction to the kernel trick — moving beyond the linear "
                  "model while keeping the regression framework.")),
    dict(slug="05-classification-1",
         title="Classification I",
         subtitle="Logistic regression and generative models",
         date="Sep 26, 2025",
         reading="ISLR ch. 4",
         summary=("Logistic regression as a discriminative classifier; LDA, QDA, "
                  "and naive Bayes as generative classifiers. Decision boundaries "
                  "and what each approach assumes about the data.")),
    dict(slug="06-classification-2",
         title="Classification II",
         subtitle="Support vector machines",
         date="Oct 3, 2025",
         reading="ISLR ch. 9",
         summary=("Maximum margin classifiers; the soft-margin extension; kernel "
                  "SVMs for non-linear decision boundaries.")),
    dict(slug="07-clustering-kmeans",
         title="Clustering I",
         subtitle="k-means and cluster evaluation",
         date="Oct 17, 2025",
         reading="ISLR §10.3",
         summary=("k-means as an EM-style algorithm; choosing k; silhouette scores "
                  "and gap statistics; what cluster evaluation actually tells you.")),
    dict(slug="08-clustering-gmm",
         title="Clustering II",
         subtitle="Gaussian mixture models",
         date="Oct 24, 2025",
         reading="PRML §9.1–9.2",
         summary=("Gaussian mixture models as soft clustering; the EM algorithm; "
                  "model selection via BIC; comparing GMMs to k-means.")),
    dict(slug="09-pca",
         title="Dimensionality reduction",
         subtitle="Principal component analysis",
         date="Oct 31, 2025",
         reading="ISLR §6.3",
         summary=("PCA as variance maximization and as a reconstruction problem; "
                  "the SVD; choosing the number of components; PCR as a regression "
                  "follow-up.")),
    dict(slug="10-trees",
         title="Decision trees",
         subtitle="CART for regression and classification",
         date="Nov 14, 2025",
         reading="ISLR §8.1",
         summary=("Recursive binary splitting; impurity measures (Gini, entropy); "
                  "tree pruning via cost-complexity; trees as interpretable but "
                  "high-variance learners.")),
    dict(slug="11-ensemble",
         title="Ensemble learning",
         subtitle="Bagging, random forests, boosting",
         date="Nov 21, 2025",
         reading="ISLR §8.2",
         summary=("Why averaging helps; bagging and the bootstrap; random forests "
                  "and feature subsampling; boosting (AdaBoost, gradient boosting) "
                  "as a different ensemble philosophy.")),
]


def list_files(folder: Path, exts=None):
    """Return sorted list of files in folder matching extensions (or all)."""
    if not folder.exists():
        return []
    files = [f for f in folder.iterdir() if f.is_file() and not f.name.startswith(".")]
    if exts:
        files = [f for f in files if f.suffix.lower() in exts]
    return sorted(files, key=lambda f: f.name.lower())


def pretty_reading_name(filename: str) -> str:
    """Turn ugly reading filenames into clean labels.

    Examples:
      'reading1_breimann2001.pdf'    -> 'Breimann (2001)'
      'islr_class1.pdf'               -> 'ISLR (selected pages)'
      'Heinze et al. (2017).pdf'      -> 'Heinze et al. (2017)'   [unchanged]
      'Van Calster et al. (2025).pdf' -> 'Van Calster et al. (2025)' [unchanged]
    """
    name = Path(filename).stem

    # Special-case: ISLR chapter excerpts
    if re.match(r"^islr[_\s]?class\d+$", name, re.I):
        return "ISLR (selected pages)"

    # Drop reading-number prefixes like "reading1_"
    name = re.sub(r"^reading\d+_", "", name)
    # Replace underscores with spaces
    name = name.replace("_", " ")
    # If we end up with "WordYYYY" with no space between, split it
    name = re.sub(r"^([A-Za-z]+)(\d{4})$", r"\1 (\2)", name)
    # Title-case if all-lowercase
    if name == name.lower():
        name = name.title()
    return name


def build_lecture_qmd(meta: dict) -> str:
    slug = meta["slug"]
    folder = ROOT / slug

    parts = []

    # ---- frontmatter ----
    parts.append("---")
    parts.append(f'title: "{meta["title"]}"')
    parts.append(f'subtitle: "{meta["subtitle"]}"')
    parts.append(f'date: "{meta["date"]}"')
    parts.append("date-format: long")
    parts.append("---")
    parts.append("")
    parts.append(meta["summary"])
    parts.append("")
    parts.append(f"**Assigned reading:** {meta['reading']}")
    parts.append("")

    # ---- slides ----
    slides_pdf = folder / "slides.pdf"
    if slides_pdf.exists():
        parts.append("## Slides")
        parts.append("")
        parts.append(f'<iframe src="slides.pdf" class="slide-embed" title="{meta["title"]} slides"></iframe>')
        parts.append("")
        parts.append("[📥 Download slides (PDF)](slides.pdf)")
        parts.append("")

    # ---- readings ----
    readings = list_files(folder / "readings", exts={".pdf"})
    if readings:
        parts.append("## Readings")
        parts.append("")
        parts.append('<ul class="resource-list">')
        for r in readings:
            label = pretty_reading_name(r.name)
            parts.append(f'<li>📄 <a href="readings/{r.name}">{label}</a></li>')
        parts.append("</ul>")
        parts.append("")

    # ---- lab ----
    code_dir = folder / "code"
    lab_html = list_files(code_dir, exts={".html"})
    lab_rmd = [f for f in list_files(code_dir, exts={".rmd"}) if "lab" in f.name.lower()]
    lab_r = [f for f in list_files(code_dir, exts={".r"})
             if re.match(r"^lab\d*\.r$", f.name, re.I)]
    if lab_html or lab_rmd or lab_r:
        parts.append("## Lab")
        parts.append("")
        parts.append('::: {.lab-header}')
        parts.append("In-class walkthrough — open the rendered lab in a new tab. The R Markdown source is provided so you can run the code yourself in RStudio.")
        parts.append(":::")
        parts.append("")
        parts.append('<ul class="resource-list">')
        for h in lab_html:
            parts.append(f'<li>🧪 <a href="code/{h.name}" target="_blank">Open lab (HTML)</a></li>')
        for r in lab_rmd:
            parts.append(f'<li>📝 <a href="code/{r.name}">Lab source ({r.name})</a></li>')
        for r in lab_r:
            parts.append(f'<li>📝 <a href="code/{r.name}">Lab source ({r.name})</a></li>')
        parts.append("</ul>")
        parts.append("")

    # ---- code (figure scripts, shiny apps, helpers) ----
    if code_dir.exists():
        all_code = list_files(code_dir, exts={".r", ".rmd"})
        # Exclude lab Rmds and lab .R scripts (already covered above) but keep
        # figure/app/helper R scripts
        non_lab = [f for f in all_code
                   if not (f.suffix.lower() == ".rmd" and "lab" in f.name.lower())
                   and not re.match(r"^lab\d*\.r$", f.name, re.I)]
        if non_lab:
            parts.append("## Code")
            parts.append("")
            # Group by type
            figs = [f for f in non_lab if f.name.lower().startswith("fig")]
            apps = [f for f in non_lab if f.name.lower().startswith("app")]
            others = [f for f in non_lab if f not in figs and f not in apps]

            if figs:
                parts.append("**Figure scripts** — reproduce each figure from the slides:")
                parts.append("")
                parts.append('<ul class="resource-list">')
                for f in figs:
                    parts.append(f'<li>📈 <a href="code/{f.name}">{f.name}</a></li>')
                parts.append("</ul>")
                parts.append("")
            if apps:
                parts.append("**Interactive demos** — Shiny apps (run locally with `shiny::runApp()`):")
                parts.append("")
                parts.append('<ul class="resource-list">')
                for f in apps:
                    parts.append(f'<li>🎛️ <a href="code/{f.name}">{f.name}</a></li>')
                parts.append("</ul>")
                parts.append("")
            if others:
                parts.append("**Other scripts**:")
                parts.append("")
                parts.append('<ul class="resource-list">')
                for f in others:
                    parts.append(f'<li>📜 <a href="code/{f.name}">{f.name}</a></li>')
                parts.append("</ul>")
                parts.append("")

    # ---- data ----
    data_files = list_files(folder / "data", exts={".csv", ".tsv", ".rds", ".rdata"})
    if data_files:
        parts.append("## Data")
        parts.append("")
        parts.append('<ul class="resource-list">')
        for d in data_files:
            parts.append(f'<li>📊 <a href="data/{d.name}">{d.name}</a></li>')
        parts.append("</ul>")
        parts.append("")

    # ---- extras ----
    extras_dir = folder / "extras"
    extras = list_files(extras_dir)
    if extras:
        parts.append("## Extras")
        parts.append("")
        parts.append("Optional supplementary material:")
        parts.append("")
        parts.append('<ul class="resource-list">')
        for e in extras:
            icon = "📄" if e.suffix.lower() == ".pdf" else "📜"
            parts.append(f'<li>{icon} <a href="extras/{e.name}">{e.name}</a></li>')
        parts.append("</ul>")
        parts.append("")

    return "\n".join(parts)


def main():
    project_root = Path(__file__).resolve().parent
    for meta in LECTURES:
        qmd = build_lecture_qmd(meta)
        target = ROOT / meta["slug"] / "index.qmd"
        target.write_text(qmd)
        print(f"  wrote {target.relative_to(project_root)}  ({len(qmd):,} chars)")


if __name__ == "__main__":
    main()

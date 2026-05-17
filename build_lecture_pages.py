"""
Generate per-lecture index.qmd files from the contents of each lecture folder.

For each lectures/<slug>/:
  - reads slides_clean/annotated/split PDFs, code/, data/, figures/, readings/, extras/
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
         date="Aug 28, 2026",
         reading="ISLR ch. 2",
         summary=("Course overview and the landscape of statistical learning. "
                  "We distinguish supervised problems (regression, classification) "
                  "from unsupervised ones (clustering, dimensionality reduction), "
                  "and motivate the rest of the semester through health-science examples.")),
    dict(slug="02-training",
         title="Training statistical learning models",
         subtitle="Bias-variance, validation, resampling",
         date="Sep 4, 2026",
         reading="ISLR §2.2, ch. 5",
         summary=("Under- and over-fitting; the training/validation/test split; "
                  "k-fold cross-validation and the bootstrap. Foundations for every "
                  "method we'll see later in the course.")),
    dict(slug="03-regression-1",
         title="Regression I",
         subtitle="Subset selection and shrinkage",
         date="Sep 11, 2026",
         reading="ISLR ch. 3, 6",
         summary=("Linear regression as a baseline; best-subset and stepwise selection; "
                  "ridge regression and the lasso as principled alternatives when p is "
                  "large or predictors are correlated.")),
    dict(slug="04-regression-2",
         title="Regression II",
         subtitle="Non-linear regression, splines, and the kernel trick",
         date="Sep 18, 2026",
         reading="ISLR ch. 7",
         summary=("Polynomials, step functions, regression splines, smoothing splines, "
                  "and an introduction to the kernel trick — moving beyond the linear "
                  "model while keeping the regression framework.")),
    dict(slug="05-classification-1",
         title="Classification I",
         subtitle="Logistic regression and generative models",
         date="Sep 25, 2026",
         reading="ISLR ch. 4",
         summary=("Logistic regression as a discriminative classifier; LDA, QDA, "
                  "and naive Bayes as generative classifiers. Decision boundaries "
                  "and what each approach assumes about the data.")),
    dict(slug="06-classification-2",
         title="Classification II",
         subtitle="Support vector machines",
         date="Oct 2, 2026",
         reading="ISLR ch. 9",
         summary=("Maximum margin classifiers; the soft-margin extension; kernel "
                  "SVMs for non-linear decision boundaries.")),
    dict(slug="07-clustering-kmeans",
         title="Clustering I",
         subtitle="k-means and cluster evaluation",
         date="Oct 9, 2026",
         reading="ISLR §10.3",
         summary=("k-means as an EM-style algorithm; choosing k; silhouette scores "
                  "and gap statistics; what cluster evaluation actually tells you.")),
    dict(slug="08-clustering-gmm",
         title="Clustering II",
         subtitle="Gaussian mixture models",
         date="Oct 16, 2026",
         reading="PRML §9.1–9.2",
         summary=("Gaussian mixture models as soft clustering; the EM algorithm; "
                  "model selection via BIC; comparing GMMs to k-means.")),
    dict(slug="09-pca",
         title="Dimensionality reduction",
         subtitle="Principal component analysis",
         date="Oct 23, 2026",
         reading="ISLR §6.3",
         summary=("PCA as variance maximization and as a reconstruction problem; "
                  "the SVD; choosing the number of components; PCR as a regression "
                  "follow-up.")),
    dict(slug="10-trees",
         title="Decision trees",
         subtitle="CART for regression and classification",
         date="Oct 30, 2026",
         reading="ISLR §8.1",
         summary=("Recursive binary splitting; impurity measures (Gini, entropy); "
                  "tree pruning via cost-complexity; trees as interpretable but "
                  "high-variance learners.")),
    dict(slug="11-ensemble",
         title="Ensemble learning",
         subtitle="Bagging, random forests, boosting",
         date="Nov 6, 2026",
         reading="ISLR §8.2",
         summary=("Why averaging helps; bagging and the bootstrap; random forests "
                  "and feature subsampling; boosting (AdaBoost, gradient boosting) "
                  "as a different ensemble philosophy.")),
]

# Ordered slide variants with (filename, emoji, display label, note)
SLIDE_VARIANTS = [
    ("slides_clean.pdf",      "🖥️",  "Clean slides",     ""),
    ("slides_annotated.pdf",  "✏️",  "Annotated slides", "*(in-class version)*"),
    ("slides_split.pdf",      "🖨️",  "Slides (2-up)",    "*(two per page, for printing)*"),
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
    """Turn ugly reading filenames into clean citation-style labels.

    Examples:
      'reading1_breimann2001.pdf'    -> 'Breimann (2001)'
      'islr_class1.pdf'               -> 'ISLR (selected pages)'
      'Heinze et al. (2017).pdf'      -> 'Heinze et al. (2017)'   [unchanged]
    """
    name = Path(filename).stem

    if re.match(r"^islr[_\s]?class\d+$", name, re.I):
        return "ISLR (selected pages)"

    name = re.sub(r"^reading\d+_", "", name)
    name = name.replace("_", " ")
    name = re.sub(r"^([A-Za-z]+)(\d{4})$", r"\1 (\2)", name)
    if name == name.lower():
        name = name.title()
    return name


def pretty_extras_name(filename: str) -> str:
    """Turn extras filenames into readable display labels."""
    name = Path(filename).stem
    name = re.sub(r"^extra[_\s]+", "", name, flags=re.I)
    name = name.replace("_", " ").replace("-", " ")
    name = re.sub(r"([a-z])([A-Z])", r"\1 \2", name)
    name = name.strip()
    words = [w.capitalize() if w.islower() else w for w in name.split()]
    return " ".join(words)


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
    parts.append(f"📖 **Assigned reading:** {meta['reading']}")
    parts.append("")
    parts.append("---")
    parts.append("")

    # ---- slides ----
    present_slides = [(fname, emoji, label, note)
                      for fname, emoji, label, note in SLIDE_VARIANTS
                      if (folder / fname).exists()]
    if present_slides:
        parts.append("## 📊 Slides")
        parts.append("")
        for fname, emoji, label, note in present_slides:
            suffix = f"  {note}" if note else ""
            parts.append(f"- {emoji} [{label}]({fname}){suffix}")
        parts.append("")

    # ---- readings ----
    readings = list_files(folder / "readings", exts={".pdf"})
    if readings:
        parts.append("## 📚 Readings")
        parts.append("")
        for r in readings:
            label = pretty_reading_name(r.name)
            parts.append(f"- 📄 [{label}](readings/{r.name})")
        parts.append("")

    # ---- lab ----
    code_dir = folder / "code"
    lab_html = [f for f in list_files(code_dir, exts={".html"}) if "lab" in f.name.lower()]
    lab_rmd  = [f for f in list_files(code_dir, exts={".rmd"})  if "lab" in f.name.lower()]
    lab_r    = [f for f in list_files(code_dir, exts={".r"})
                if re.match(r"^lab\d*\.r$", f.name, re.I)]
    if lab_html or lab_rmd or lab_r:
        parts.append("## 🧪 Lab")
        parts.append("")
        parts.append("In-class walkthrough — open the rendered lab in a new tab. "
                     "The R Markdown source is provided so you can run the code yourself in RStudio.")
        parts.append("")
        for h in lab_html:
            label = h.stem.replace("_", " ")
            parts.append(f'- 🧪 [{label} (HTML)](code/{h.name}){{target="_blank"}}')
        for r in lab_rmd:
            parts.append(f"- 📝 [Lab source ({r.name})](code/{r.name})")
        for r in lab_r:
            parts.append(f"- 📝 [Lab source ({r.name})](code/{r.name})")
        parts.append("")

    # ---- code (figure scripts, shiny apps, helpers) ----
    if code_dir.exists():
        all_code = list_files(code_dir, exts={".r", ".rmd"})
        non_lab = [f for f in all_code
                   if not (f.suffix.lower() == ".rmd" and "lab" in f.name.lower())
                   and not re.match(r"^lab\d*\.r$", f.name, re.I)]
        if non_lab:
            parts.append("## 💻 Code")
            parts.append("")
            figs   = [f for f in non_lab if f.name.lower().startswith("fig")]
            apps   = [f for f in non_lab if f.name.lower().startswith("app")]
            others = [f for f in non_lab if f not in figs and f not in apps]

            if figs:
                parts.append("**Figure scripts** — reproduce each figure from the slides:")
                parts.append("")
                for f in figs:
                    parts.append(f"- 📈 [{f.name}](code/{f.name})")
                parts.append("")
            if apps:
                parts.append("**Interactive demos** — Shiny apps (run locally with `shiny::runApp()`):")
                parts.append("")
                for f in apps:
                    parts.append(f"- 🎛️ [{f.name}](code/{f.name})")
                parts.append("")
            if others:
                parts.append("**Other scripts:**")
                parts.append("")
                for f in others:
                    parts.append(f"- 📜 [{f.name}](code/{f.name})")
                parts.append("")

    # ---- data ----
    data_files = list_files(folder / "data", exts={".csv", ".tsv", ".rds", ".rdata"})
    if data_files:
        parts.append("## 🗂️ Data")
        parts.append("")
        for d in data_files:
            parts.append(f"- 📊 [{d.name}](data/{d.name})")
        parts.append("")

    # ---- extras ----
    extras_dir = folder / "extras"
    extras = list_files(extras_dir)
    if extras:
        parts.append("## ⭐ Extras")
        parts.append("")
        parts.append("Optional supplementary material:")
        parts.append("")
        for e in extras:
            icon  = "📄" if e.suffix.lower() == ".pdf" else "📜"
            label = pretty_extras_name(e.name)
            parts.append(f"- {icon} [{label}](extras/{e.name})")
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

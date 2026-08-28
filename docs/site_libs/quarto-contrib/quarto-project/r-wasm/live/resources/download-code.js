// Adds a floating "Download my code" button to webR-live pages.
// Reads the LIVE CodeMirror editor content for every {webr} cell on the
// page (including any edits the student made) and bundles it into one
// downloadable .R script, labeled by the section heading each cell sits
// under.
(function () {
  function currentSectionTitle(cellEl) {
    let node = cellEl;
    while (node) {
      let prev = node.previousElementSibling;
      while (prev) {
        if (/^H[1-6]$/.test(prev.tagName)) {
          return prev.textContent.trim();
        }
        prev = prev.previousElementSibling;
      }
      node = node.parentElement;
      if (node && /^H[1-6]$/.test(node.tagName)) {
        return node.textContent.trim();
      }
    }
    return "Untitled section";
  }

  function extractEditorText(exerciseCellEl) {
    const cmContent = exerciseCellEl.querySelector(".cm-content");
    if (!cmContent) return null;
    const lines = Array.from(cmContent.querySelectorAll(".cm-line"));
    if (lines.length === 0) return cmContent.textContent;
    return lines.map((l) => l.textContent).join("\n");
  }

  function collectCode() {
    const exerciseCells = Array.from(document.querySelectorAll(".exercise-cell"));
    if (exerciseCells.length === 0) return null;

    const bySection = new Map();
    exerciseCells.forEach((cellEl, i) => {
      const code = extractEditorText(cellEl);
      if (code === null) return;
      const wrapper = cellEl.closest(".cell") || cellEl;
      const title = currentSectionTitle(wrapper);
      if (!bySection.has(title)) bySection.set(title, []);
      bySection.get(title).push(code);
    });

    const parts = [
      "# " + document.title,
      "# Downloaded " + new Date().toLocaleString(),
      "# Code as it appeared in each editable cell at download time",
      "# (includes any edits made in class -- this is not the original lab file)",
      "",
    ];
    for (const [title, snippets] of bySection.entries()) {
      parts.push("# ---- " + title + " ----");
      snippets.forEach((code, i) => {
        if (snippets.length > 1) parts.push("# cell " + (i + 1));
        parts.push(code);
        parts.push("");
      });
    }
    return parts.join("\n");
  }

  function downloadText(filename, text) {
    const blob = new Blob([text], { type: "text/plain;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  function makeFilename() {
    const base = (document.title || "lab").replace(/[^\w.-]+/g, "_");
    const stamp = new Date().toISOString().slice(0, 10);
    return base + "_" + stamp + ".R";
  }

  function addButton() {
    if (document.getElementById("webr-download-code-btn")) return;
    const btn = document.createElement("button");
    btn.id = "webr-download-code-btn";
    btn.textContent = "⬇ Download my code";
    btn.setAttribute("aria-label", "Download the current contents of every code cell as an R script");
    btn.style.cssText = [
      "position:fixed",
      "bottom:1.25rem",
      "right:1.25rem",
      "z-index:9999",
      "padding:0.6rem 1rem",
      "font-size:0.95rem",
      "font-weight:600",
      "border-radius:0.5rem",
      "border:1px solid rgba(0,0,0,.2)",
      "background-color:#0d6efd",
      "color:#fff",
      "box-shadow:0 2px 8px rgba(0,0,0,.25)",
      "cursor:pointer",
    ].join(";");
    btn.addEventListener("mouseenter", () => (btn.style.backgroundColor = "#0b5ed7"));
    btn.addEventListener("mouseleave", () => (btn.style.backgroundColor = "#0d6efd"));
    btn.addEventListener("click", () => {
      const code = collectCode();
      if (!code) {
        alert("No code cells found on this page yet -- try again once the page has finished loading.");
        return;
      }
      downloadText(makeFilename(), code);
    });
    document.body.appendChild(btn);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", addButton);
  } else {
    addButton();
  }
})();

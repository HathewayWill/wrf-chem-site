(async function () {
  const input = document.querySelector("#q");
  const out = document.querySelector("#results");
  const count = document.querySelector("#count");
  if (!input || !out) return;

  const res = await fetch("./search-index.json", { cache: "no-store" });
  const data = await res.json();

  function esc(s){ return (s||"").replace(/[&<>"]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c])); }

  function render(list){
    out.innerHTML = "";
    if (!list.length){
      out.innerHTML = '<div class="result"><div><strong>No matches.</strong></div><div class="meta">Try a shorter query.</div></div>';
      count.textContent = "0";
      return;
    }
    count.textContent = String(list.length);
    for (const it of list.slice(0, 40)){
      const p = esc(it.path);
      const t = esc(it.title);
      const s = esc(it.summary || "");
      out.insertAdjacentHTML("beforeend", `
        <div class="result">
          <div><a href="../${p}">${t}</a></div>
          <div class="meta">/${p}${s ? " · " + s : ""}</div>
        </div>
      `);
    }
  }

  function search(q){
    q = (q||"").trim().toLowerCase();
    if (!q){
      out.innerHTML = "";
      count.textContent = "0";
      return;
    }
    const terms = q.split(/\s+/).filter(Boolean);
    const hits = data.filter(it => {
      const hay = (it.title + " " + it.path + " " + (it.summary||"")).toLowerCase();
      return terms.every(t => hay.includes(t));
    });
    render(hits);
  }

  input.addEventListener("input", () => search(input.value));
})();

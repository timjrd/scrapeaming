function main() {
  if (window.location.href === "https://www.bing.com/") {
    document.querySelector(".b_searchbox").value = QUERY;
    document.querySelector("input[type=submit]").click();
  }
  else {
    for (let e of document.querySelectorAll(".b_algo h2 > a"))
      outputUrl(e.getAttribute("href"));
    const next = document.querySelector(".sb_pagN");
    if (next !== null)
      next.click();
  }
}

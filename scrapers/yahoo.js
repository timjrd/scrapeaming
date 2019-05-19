function main() {
  if (window.location.hostname === "guce.oath.com") {
    document.querySelector("button.primary").click();
  }
  else if (window.location.href === "https://search.yahoo.com/?guccounter=1") {
    document.querySelector("input[type=text]").value = QUERY;
    document.querySelector("button[type=submit]").click();
  }
  else {
    for (let e of document.querySelectorAll("h3 > a.ac-algo")) {
      const s = e.href.split("/RU=")[1];
      if (s !== undefined)
	outputUrl(decodeURIComponent(s.split("/")[0]));
    }
    const next = document.querySelector(".next");
    if (next !== null)
      next.click();
  }
}

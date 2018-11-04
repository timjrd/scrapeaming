// 60 document_idle
function main() {
    if (window.location.href === "https://duckduckgo.com/") {
	document.querySelector("input[type=text]").value = QUERY;
	document.querySelector("input[type=submit]").click();
    }
    else {
	repeat(() => {
	    for (let e of document.querySelectorAll(".result__title")) {
		if (e.querySelector(".badge--ad") === null)
		    outputUrl(e.querySelector("a.result__a:first-of-type").getAttribute("href"));
	    }
	    const next = document.querySelector(".result--more__btn");
	    if (next !== null)
		next.click();
	});
    }
}

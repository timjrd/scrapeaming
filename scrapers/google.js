// 60 document_idle
function main() {
    if (window.location.href === "https://www.google.com/") {
	document.querySelector("input[type=text]").value = QUERY;
	document.querySelector("input[type=submit]").click();
    }
    else {
	for (let e of document.querySelectorAll(".srg .r > a:first-of-type"))
	    outputUrl(e.getAttribute("href"));
	const next = document.querySelector("#pnnext");
	if (next !== null)
	    next.click();
    }
}

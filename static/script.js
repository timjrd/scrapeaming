"use strict";

setInterval(refresh, 500);
refresh();

function refresh() {
    get("/status/" + getToken(), (response) => {
	if (response === null) {
	    setDone(true);
	}
	else {
	    const status = JSON.parse(response);

	    const logs = document.getElementById("logs");
	    logs.innerText = "";
	    
	    let n = 5;
	    for (let log of status.logs) {
		const li = document.createElement("li");
		li.innerText = log;
		logs.appendChild(li);
		n--; if (n <= 0) break;
	    }
	    
	    const table = document.getElementById("results");
	    table.innerText = "";
	    
	    for (let result of status.results)
		table.appendChild( resultNode(result) );
	    
	    setDone(status.done);
	}
    });
}

function onSearch(e) {
    if (isClickOrEnter(e)) {

	document.getElementById("video").innerText = "";
	const query = document.getElementById("query").value;
	
	get("/search/" + query, (token) => {
	    if (token !== null) {
		setToken(token);
		refresh();
	    }
	});
    }
}
function onCancel(e) {
    if (isClickOrEnter(e))
	get("/cancel/" + getToken(), () => refresh());
}

function setDone(done) {
    if (done) {
	document.body.className = "done";
	document.getElementById("query").disabled = false;
    }
    else {
	document.body.className = "ongoing";
	document.getElementById("query").disabled = true;
    }
}

function play(result) {
    const video = document.createElement("video");
    video.controls = true;
    video.autoplay = true;
    video.src = result.sources[0];

    const container = document.getElementById("video");
    container.innerText = "";
    container.appendChild(video);
    window.scrollTo(0,0);
}

function resultNode(result) {
    const tr  = document.createElement("tr");
    
    const td1 = document.createElement("td");
    const td2 = document.createElement("td");

    const time   = document.createElement("time");
    const em     = document.createElement("em");
    const strong = document.createElement("strong");
    
    const h4  = document.createElement("h4");
    const ul  = document.createElement("ul");

    const space = () => document.createTextNode(" ");
    
    time.dateTime    = result.duration + "s";
    time.innerText   = duration(result.duration);
    em.innerText     = result.width + "x" + result.height;
    strong.innerText = result.quality + "%";

    td1.appendChild(time);
    td1.appendChild(space());
    td1.appendChild(em);
    td1.appendChild(space());
    td1.appendChild(strong);
    
    h4.innerText = result.title;
    h4.onclick = () => play(result);
    
    for (let source of result.sources) {
	const li = document.createElement("li");
	const a  = document.createElement("a");
	a.href = source;
	a.innerText = source;
	li.appendChild(a);
	ul.appendChild(li);
    }

    td2.appendChild(h4);
    td2.appendChild(ul);

    tr.appendChild(td1);
    tr.appendChild(td2);

    return tr;
}

function duration(x) {
    if      (x < 60     ) return seconds(x);
    else if (x < 60 * 60) return minutes(x);
    else return hours(x) + " " + minutes(x);
}
function seconds(x) {
    return (x % 60) + "s";
}
function minutes(x) {
    return Math.floor(x % (60*60) / 60) + "min";
}
function hours (x) {
    return Math.floor(x / (60*60)) + "h";
}

function getToken() {
    return window.location.hash.slice(1);
}
function setToken(token) {
    window.location.hash = token;
}

function isClickOrEnter(e) {
    return e.key === undefined || e.key === "Enter";
}

function get(path, f) {
    const req = new XMLHttpRequest();
    req.onreadystatechange = function(event) {
	if (this.readyState === XMLHttpRequest.DONE) {
            if (this.status === 200)
		f(this.responseText);
            else
		f(null);
	}
    };
    req.open("GET", path, true);
    req.send(null);
}

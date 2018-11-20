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

	    if (!status.done)
		sync( zipWithIndex( take(5, status.logs) ),
		      document.getElementById("logs"),
		      (x) => status.logCount - x[0],
		      logNode );
	    
	    sync( status.results,
		  document.getElementById("results"),
		  (x) => x.sources[0],
		  resultNode );
	    
	    setDone(status.done);
	}
    });
}

function onSearch(e) {
    if (isClickOrEnter(e)) {
	
	unplay();
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
	hide("#cancel, .dot, #logs");
	show("h1, #query, #search");
	document.getElementById("query").disabled = false;
    }
    else {
	hide("#search");
	show("h1, #query, #cancel, .dot, #logs");
	document.getElementById("query").disabled = true;
    }
}

function play(result, node) {
    if (play.node !== undefined)
	play.node.classList.remove("play");
    play.node = node;
    node.classList.add("play");
    
    const video = document.getElementById("video");

    const width  = video.getBoundingClientRect().width;
    const height = result.height * (width / result.width);
    
    video.className = "show";
    video.src       = result.sources[0];
    video.height    = Math.min(height, width);
    
    window.scrollTo(0,0);
}
function unplay() {
    if (play.node !== undefined) {
	play.node.classList.remove("play");
	delete play.node;
    }
    
    const video = document.getElementById("video");
    video.className = "hide";
    video.src = "";
}

function logNode(log) {
    const li = document.createElement("li");
    li.innerText = log[1];
    return li;
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
    
    for (let source of result.sources) {
	const li = document.createElement("li");
	const a  = document.createElement("a");
	a.href = source;
	a.innerText = source;
	a.onclick = () => false;
	li.appendChild(a);
	ul.appendChild(li);
    }

    td2.appendChild(h4);
    td2.appendChild(ul);

    tr.appendChild(td1);
    tr.appendChild(td2);

    tr.onclick = () => play(result, tr);
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

function zipWithIndex(xs) {
    const ys = [];
    let i = 0;
    for (let x of xs) {
	ys.push([i,x]);
	i++;
    }
    return ys;
}

function take(n, xs) {
    const ys = [];
    for (let x of xs) {
	if (n > 0) {
	    ys.push(x);
	    n--;
	}
	else break;
    }
    return ys;
}

function sync(xs, ys, id, f) {
    function remove(y) {
	for (let x of xs)
	    if (y.dataset.syncId === id(x).toString())
		return false;
	return true;
    }
    function add(y1, y2) {
	let found = y1 === null;
	const res = [];
	for (let x of xs) {
	    if (!found && y1.dataset.syncId === id(x).toString())
		found = true;
	    else if (y2 !== undefined && y2.dataset.syncId === id(x).toString())
		return res;
	    else if (found) {
		const y = f(x);
		y.dataset.syncId = id(x).toString();
		res.push(y);
	    }
	}
	return res;	
    }
    sync_(ys, remove, add);
}
function sync_(container, remove, add) {
    const children1 = [];
    for (let child of container.children)
	if (child.dataset.syncRemoved === undefined)
	    children1.push(child);

    const children2 = [];
    for (let child of children1) {
	if (remove(child)) {
	    child.dataset.syncRemoved = true;
	    fadeOut(child);
	}
	else
	    children2.push(child);
    }
    
    let prev = null;
    for (let child of children2) {
	for (let e of add(prev, child))
	    fadeIn(container, e, child);
	prev = child;
    }
    for (let e of add(prev))
	fadeIn(container, e);
}

const fadeTimeout   = 100;
const removeTimeout = 1000;
function fadeIn(container, e, ref) {
    e.className = "hide";
    setTimeout(() => e.className = "show", fadeTimeout);

    if (ref === undefined)
	container.appendChild(e);
    else
	container.insertBefore(e, ref)
}
function fadeOut(e) {
    setTimeout(() => e.className = "hide", fadeTimeout);
    setTimeout(() => e.remove(), removeTimeout);
}

function hide(s) {
    for (let e of document.querySelectorAll(s)) {
	e.classList.remove("show");
	e.classList.add("hide");
    }
}
function show(s) {
    for (let e of document.querySelectorAll(s)) {
	e.classList.remove("hide");
	e.classList.add("show");
    }
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

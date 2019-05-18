function output(msg) {
  const k = " " + TOKEN + " ";
  if (msg !== null)
    console.log(k + msg + k);
}
function outputUrl(url) {
  if (url !== null)
    output( new URL(url, window.location.href).href );
}
function repeat(f) {
  // See https://stackoverflow.com/a/19201292/3776835 .
  // We don't use setInterval because it could be canceled by the page.
  
  function workerFun() {
    setInterval(() => postMessage(null), 1000);
  };
  
  const blobUrl = URL.createObjectURL(
    new Blob( ["(", workerFun.toString(), ")()"],
	      {type:"application/javascript"} ));
  
  worker = new Worker(blobUrl);
  worker.onmessage = f;
  URL.revokeObjectURL(blobUrl);
}
main();

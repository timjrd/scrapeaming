function main() {
  repeat(() => {
    for (let e of document.querySelectorAll("video, video > source"))
      outputUrl(e.getAttribute("src"));
  });
}

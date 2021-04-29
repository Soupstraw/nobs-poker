import Main from "./output/Main";

// see https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function() {
    console.log("running main again");
    document.body.innerHTML = "";
    Main.main();
  });
}

console.log("starting");
Main.main();

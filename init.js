// import { createApp } from "vue";
// import App from "./App.vue";

// console.log(App);

// const app = createApp(App, {});

// app.mount("#app");

import { Elm } from "./Main.elm";

Elm.OfferingGame.Main.init({
  node: document.getElementById("app"),
  flags: {},
});

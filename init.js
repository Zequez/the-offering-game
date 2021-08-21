// import { createApp } from "vue";
// import App from "./App.vue";

// console.log(App);

// const app = createApp(App, {});

// app.mount("#app");
import "./style.css";
import { Elm } from "./Main.elm";

Elm.OfferingGame.Main.init({
  node: document.getElementById("app"),
  flags: {},
});

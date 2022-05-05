import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

app.ports.runForeign.subscribe(([cmd, args]) => {
  const candidate = cmd.reduce((cur, key) => cur != null ? cur[key] : undefined, window || global)
  if (typeof candidate === "function") {
    candidate.apply(null, args)
  } else {
    console.warn(cmd, "was not resolved to a function")
  }
})
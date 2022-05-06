import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

const getPath = (path, start) =>
  path.reduce((cur, seg) => cur != null ? cur[seg] : undefined, start)

app.ports.runForeign.subscribe(({ id, cmd, args }) => {
  const candidate = getPath(cmd, window ?? global)
  if (typeof candidate === "function") {
    try {
      const result = candidate.apply(null, args)
      if (result != null && typeof result["then"] === "function") {
        result.then(res => app.ports.foreignResult_.send([id, {__type: "Ok", value: res}]))
          .catch(err =>
            app.ports.foreignResult_.send([id, {__type: "Exception", exception: err.toString(), cmd, args}])
          )
      } else {
        app.ports.foreignResult_.send([id, {__type: "Ok", value: result}])
      }
    } catch (err) {
      app.ports.foreignResult_.send([id, {__type: "Exception", exception: err.toString(), cmd, args}])
    }
  } else {
    app.ports.foreignResult_.send([id, {__type: "NotFound", cmd: cmd}])
  }
})
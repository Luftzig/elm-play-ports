import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

const getPath = (path, start) =>
  path.reduce((cur, seg) => cur != null ? cur[seg] : undefined, start)

function reportException(id, err, cmd, args) {
  return app.ports.foreignResult_.send([id, {__type: "Exception", exception: err.toString(), cmd, args}])
}

function reportSuccess(id, res) {
  return app.ports.foreignResult_.send([id, {__type: "Ok", value: res}])
}

function reportNotFound(id, cmd) {
  app.ports.foreignResult_.send([id, {__type: "NotFound", cmd: cmd}])
}

function runGlobalCall(cmd, args, id) {
  const candidate = getPath(cmd, window ?? global)
  const parent = getPath(cmd.slice(0, -1), window ?? global)
  if (typeof candidate === "function") {
    try {
      const result = candidate.apply(parent, args)
      if (result != null && typeof result["then"] === "function") {
        result.then(res => reportSuccess(id, res))
          .catch(err =>
            reportException(id, err, cmd, args)
          )
      } else {
        reportSuccess(id, result)
      }
    } catch (err) {
      reportException(id, err, cmd, args)
    }
  } else {
    reportNotFound(id, cmd)
  }
}

function runListenOn(id, cmd, args, target) {
  if (target !== "") {
    // TODO: Handle target
  }
  const cb = (event) => reportSuccess(id, event)
  const candidate = getPath(cmd, window ?? global)
  const parent = getPath(cmd.slice(0, -1), window ?? global)
  if (typeof candidate === "function") {
    candidate.call(parent, cb, ...args)
  } else if (candidate != null && typeof candidate["addEventListener"] === "function") {
    candidate.addEventListener(args[0], cb, args[1])
  } else {
    reportNotFound(id, cmd)
  }
}

app.ports.runForeign_.subscribe(({ __type, id, cmd, args, target }) => {
  switch (__type) {
    case "Call":
      return runGlobalCall(cmd, args, id)
    case "New":
      return console.warn("New not implemented")
    case "Create":
      return console.warn("Create not implemented")
    case "InvokeOn":
      return console.warn("InvokeON not implemented")
    case "ListenOn":
      return runListenOn(id, cmd, args, target)
  }
})
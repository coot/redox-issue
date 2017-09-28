import resolve from "rollup-plugin-node-resolve";
import commonjs from "rollup-plugin-commonjs";
import replace from "rollup-plugin-replace";
import uglify from "rollup-plugin-uglify";
import gzip from "rollup-plugin-gzip";

const isPrd = process.env.NODE_ENV === "production";

function ignore(list) {
  const emptyFile = "export default {}";
  const emptyFileName = "\0empty_module";
  return {
    name: "ignore",
    resolveId(importee) {
      return list.some(l => importee.includes(l)) ? emptyFileName : null;
    },
    load(id) {
      return id === emptyFileName ? emptyFile : null;
    }
  };
}

export default {
  input: isPrd ? "dce-output/Main/index.js" : "output/Main/index.js",
  name: "App",
  output: {
    file: "dist/bundle.js",
    format: "iife",
    globals: {
      react: "preactCompat",
      "react-dom": "preactCompat",
      "create-react-class": "preactCompat.createClass",
      "prop-types": "preactCompat"
    }
  },
  sourcemap: true,
  watch: {
    include: "output/**"
  },
  external: ["react", "react-dom", "create-react-class", "prop-types"],
  plugins: [
    ignore(["stream", "react-dom/server"]),
    resolve(),
    commonjs(),
    replace({ "process.env.NODE_ENV": JSON.stringify(process.env.NODE_ENV) }),
    isPrd && uglify(),
    isPrd && gzip()
  ]
};

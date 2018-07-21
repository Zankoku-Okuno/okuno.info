var DEBUG = process.env.JSC_MODE !== "production" // JSC === JavaScript Compiler
var path = require("path")


var es6 =
    { test: /\.js/
    , include: [ path.resolve(__dirname, "src") ]
    , use: { loader: "babel-loader" }
    }


module.exports =
    { mode: DEBUG ? "development" : "production"
    , devtool : DEBUG ? "source-map" : null
    , entry:
        { app: "./src/main.js"
        }
    , output:
        { filename: "[name].js"
        , path: path.resolve(__dirname, "dist")
        }
    , module: {
        rules: [
            { test: /\.html$/
            , use: { loader: "html-loader" }
            }
        ] }
    }
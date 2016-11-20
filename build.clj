(require 'cljs.build.api)

(cljs.build.api/build "src"
 {:main 'catparty.core
  :output-to "out/main.js"})

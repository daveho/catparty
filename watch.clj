(require 'cljs.build.api)

(cljs.build.api/watch "src"
  {:main 'catparty.core
   :output-to "out/main.js"})

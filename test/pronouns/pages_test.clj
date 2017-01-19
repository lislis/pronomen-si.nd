(ns pronouns.pages-test
  (:require [pronouns.pages :as pages]
            [midje.sweet :refer :all]))

(fact "prose-comma-list turns a list of strings into a prose list with commas"
      (pages/prose-comma-list ["foo"]) => "foo"
      (pages/prose-comma-list ["foo" "bar"]) => "foo und bar"
      (pages/prose-comma-list ["foo" "bar" "baz"]) => "foo, bar, und baz"
      (pages/prose-comma-list ["foo" "bar" "baz" "bobble"]) => "foo, bar, baz, und bobble"
      (pages/prose-comma-list []) => "")

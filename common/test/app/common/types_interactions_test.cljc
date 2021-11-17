;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.types-interactions-test
  (:require
   [clojure.test :as t]
   [clojure.pprint :refer [pprint]]
   [app.common.exceptions :as ex]
   [app.common.pages.init :as cpi]
   [app.common.types.interactions :as cti]
   [app.common.uuid :as uuid]
   [app.common.geom.point :as gpt]))

(t/deftest set-event-type
  (let [interaction cti/default-interaction
        shape       (cpi/make-minimal-shape :rect)
        frame       (cpi/make-minimal-shape :frame)]

    (t/testing "Set event type unchanged"
      (let [new-interaction
            (cti/set-event-type interaction :click shape)]
        (t/is (= :click (:event-type new-interaction)))))

    (t/testing "Set event type changed"
      (let [new-interaction
            (cti/set-event-type interaction :mouse-press shape)]
          (t/is (= :mouse-press (:event-type new-interaction)))))

    (t/testing "Set after delay on non-frame"
      (let [result (ex/try
                     (cti/set-event-type interaction :after-delay shape))]
        (t/is (ex/exception? result))))

    (t/testing "Set after delay on frame"
      (let [new-interaction
            (cti/set-event-type interaction :after-delay frame)]
        (t/is (= :after-delay (:event-type new-interaction)))
        (t/is (= 600 (:delay new-interaction)))))

    (t/testing "Set after delay with previous data"
      (let [interaction (assoc interaction :delay 300)
            new-interaction
            (cti/set-event-type interaction :after-delay frame)]
        (t/is (= :after-delay (:event-type new-interaction)))
        (t/is (= 300 (:delay new-interaction)))))))


(t/deftest set-action-type
  (let [interaction cti/default-interaction]

    (t/testing "Set action type unchanged"
      (let [new-interaction
            (cti/set-action-type interaction :navigate)]
        (t/is (= :navigate (:action-type new-interaction)))))

    (t/testing "Set action type changed"
      (let [new-interaction
            (cti/set-action-type interaction :prev-screen)]
        (t/is (= :prev-screen (:action-type new-interaction)))))

    (t/testing "Set action type navigate"
      (let [interaction {:event-type :click
                         :action-type :prev-screen}
            new-interaction
            (cti/set-action-type interaction :navigate)]
        (t/is (= :navigate (:action-type new-interaction)))
        (t/is (nil? (:destination new-interaction)))
        (t/is (= false (:preserve-scroll new-interaction)))))

    (t/testing "Set action type navigate with previous data"
      (let [destination (uuid/next)
            interaction {:event-type      :click
                         :action-type     :prev-screen
                         :destination     destination
                         :preserve-scroll true}
            new-interaction
            (cti/set-action-type interaction :navigate)]
        (t/is (= :navigate (:action-type new-interaction)))
        (t/is (= destination (:destination new-interaction)))
        (t/is (= true (:preserve-scroll new-interaction)))))

    (t/testing "Set action type open-overlay"
      (let [new-interaction
            (cti/set-action-type interaction :open-overlay)]
        (t/is (= :open-overlay (:action-type new-interaction)))
        (t/is (= :center (:overlay-pos-type new-interaction)))
        (t/is (= (gpt/point 0 0) (:overlay-position new-interaction)))))

    (t/testing "Set action type open-overlay with previous data"
      (let [interaction (assoc interaction :overlay-pos-type :top-left
                                           :overlay-position (gpt/point 100 200))
            new-interaction
            (cti/set-action-type interaction :open-overlay)]
        (t/is (= :open-overlay (:action-type new-interaction)))
        (t/is (= :top-left (:overlay-pos-type new-interaction)))
        (t/is (= (gpt/point 100 200) (:overlay-position new-interaction)))))

    (t/testing "Set action type toggle-overlay"
      (let [new-interaction
            (cti/set-action-type interaction :toggle-overlay)]
        (t/is (= :toggle-overlay (:action-type new-interaction)))
        (t/is (= :center (:overlay-pos-type new-interaction)))
        (t/is (= (gpt/point 0 0) (:overlay-position new-interaction)))))

    (t/testing "Set action type toggle-overlay with previous data"
      (let [interaction (assoc interaction :overlay-pos-type :top-left
                                           :overlay-position (gpt/point 100 200))
            new-interaction
            (cti/set-action-type interaction :toggle-overlay)]
        (t/is (= :toggle-overlay (:action-type new-interaction)))
        (t/is (= :top-left (:overlay-pos-type new-interaction)))
        (t/is (= (gpt/point 100 200) (:overlay-position new-interaction)))))

    (t/testing "Set action type close-overlay"
      (let [new-interaction
            (cti/set-action-type interaction :close-overlay)]
        (t/is (= :close-overlay (:action-type new-interaction)))
        (t/is (nil? (:destination new-interaction)))))

    (t/testing "Set action type close-overlay with previous data"
      (let [destination (uuid/next)
            interaction (assoc interaction :destination destination)
            new-interaction
            (cti/set-action-type interaction :close-overlay)]
        (t/is (= :close-overlay (:action-type new-interaction)))
        (t/is (= destination (:destination new-interaction)))))

    (t/testing "Set action type prev-screen"
      (let [new-interaction
            (cti/set-action-type interaction :prev-screen)]
        (t/is (= :prev-screen (:action-type new-interaction)))))

    (t/testing "Set action type open-url"
      (let [new-interaction
            (cti/set-action-type interaction :open-url)]
        (t/is (= :open-url (:action-type new-interaction)))
        (t/is (= "" (:url new-interaction)))))

    (t/testing "Set action type open-url with previous data"
      (let [interaction (assoc interaction :url "https://example.com")
            new-interaction
            (cti/set-action-type interaction :open-url)]
        (t/is (= :open-url (:action-type new-interaction)))
        (t/is (= "https://example.com" (:url new-interaction)))))))


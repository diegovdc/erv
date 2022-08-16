# erv

A library for generating microtonal scales inspired by work and theories of Erv Wilson.

## Usage
This library has several different namespaces each corresponding to a different type of scale.

### CPS (Combination product sets)
For an introduction to CPS scales:
http://www.anaphoria.com/hexany.pdf
https://en.xen.wiki/w/Combination_product_set

``` clojure
(require '[erv.cps.core :as cps])

(def my-hexany (cps/make 2 [1 3 5 7]))

(-> my-hexany
  +all-subcps ;; add all subsets under the `:subcps` key
  )
```


### MOS (Moment of Symmetry)
For an introduction to MOS scales:
https://www.anaphoria.com/mos.pdf
https://anaphoria.com/wilsonintroMOS.html
https://en.xen.wiki/w/MOS_scale
``` clojure
(require '[erv.mos.core :as mos]
         '[erv.mos.submos :as submos]')

(def period 31)
(def generator 13)
(def my-mos (mos/make period generator))
(submos/make-all-submos (nth my-mos 3) generator) ;; this will generate all secondary MOS and all possible "traverse" MOS



```


## Note
This library is a work in progress and mostly a workshop for myself, so the code is not polished as it should. If you are using this library, feel free to let make me aware of so that I can take more care of the code and the documentation.


## License

Copyright © 2020 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.

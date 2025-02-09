# Ftraqtyl Keyboard

The Ftraqtyl is a family member of the Dactyl-Manuform family of keyboards with
added trackball support for finger-operated trackballs.

The genealogy goes roughly like this, with no claim to be accurate:

- [Dactyl](https://github.com/adereth/dactyl-keyboard): Clojure!
- [ManuForm](https://github.com/jeffgran/ManuForm): thumb cluster
- [Dactyl-ManuForm](https://github.com/tshort/dactyl-keyboard): combination
- [Dactyl-Manuform Mini](https://github.com/l4u/dactyl-manuform-mini-keyboard): Mini thumb cluster
- [carbonfet](https://github.com/carbonfet/dactyl-manuform): new cluster
- [crystalhand](https://github.com/crystalhand/dactyl-keyboard): palm rests
- /u/qqurn, /u/drashna, [/u/noahjoseph](https://github.com/noahprince22/tractyl-manuform-keyboard): trackball mad lads.

## Other variants you should check out

- https://github.com/okke-formsma/dactyl-manuform-tight/
- https://github.com/geoffder/dometyl-keyboard

Shoutout also to the folks from OhKeycaps who inadvertently got me started on this journey.

## Notable Changes

- Trackball holder for the fingers (!) of the right hand.
- Trackballs don't use dowels or ball bearings, but press-fit steel balls. I prefer the feel of this, but might try BTUs also.
- Tighter vertical spacing, works for MT3 keycaps.
- Tighter horizontal spacing for first and last column. First column also slanted inwards. Last column splayed a bit.
- Three button cluster based off the "mini" cluster.
- Palm rests without silicone pads.
- Rolling rotary encoders (like a proper mouse wheel).
- Support for 34mm and 44mm trackballs.

## Generate OpenSCAD and STL models

This requires the unreleased (yet) version of scad-clj, you should be able to clone scad-clj and run `lein jar && lein install`.
(This will be obsolete once https://github.com/farrellm/scad-clj/issues/49 has been resolved).

To create everything, just run `lein generate && make -j4 all`, otherwise:

* Run `lein repl`, enter: `(load-file "src/dactyl_keyboard/dactyl.clj")`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to render and export STL files, either from UI or using `openscad --export-format binstl -o right.stl things/right.scad`

## License

Copyright © 2015-2024 Matthew Adereth, Tom Short, Leo Lou, carbonfet, crystalhands, and Ulrich Spörlein.

The source code for generating the models is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).

The generated models are distributed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](LICENSE-models).

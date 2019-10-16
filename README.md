purescript-halogen-modal
===

Very simple Modal to use with [purescript-halogen](https://github.com/slamdata/purescript-halogen)

This modal takes a type HH.HTML as input and renders it inside the modal. The
modal includes very basic interaction (only a close option for now)

For styling [purescript-css](https://github.com/slamdata/purescript-css)
/ [purescript-halogen-css](https://github.com/slamdata/purescript-halogen-css)
is used. However the following css classes have been added for custom styling.

* .modal-layer     - Overlay which covers the whole screen
* .modal-container - The modal container itself
* .modal-header    - The header of the modal for close interaction
* .modal-body      - The modal body where your content is rendered

Check out the example in ./example or run it like so

    make install
    make example

*http-server is required to run the example and is included as a dependency in
package.json. Note that you can also change the package manager in use by
editing PCK_MANAGER variable inside Makefile*

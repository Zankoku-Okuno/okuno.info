/******

Collapsible sections of page.

Classes
-------
* `collapse-head`: Contains at least the input elements for the state of the collapse.
* `collapse-selector`: When clicked, toggles the collapse state.
* `collapse`: An element controlled by collapsing by adding the classes `collapse-{open,close}`.
* `collapse-open`: A collapsible element which is open.
* `collapse-close`: A collapsible element which is closed.

Dom
---
* `.collapse-head input[type-checkbox]`: By having the state of the collapse be stored
        by an HTML control element, browsers can make decisions about maintaining that
        state beyond the lifetime of the page view. The associated control with a
        collapse is the first checkbox in `collapse-head`.
* `data-collapse` attribute: This links up elements that are part of the same collapse.
        A `collapse` element will be affected by the checkbox in a `collapse-head` with
        the same value of this attribute, which in turn is affected by any
        `collapse-selectors` with the same value of this attribute.

Interaction
-----------
* `change` on checkbox: Whenever the controlling checkbox changes state,
        the `collapse-{open,closed}` classes are disbursed as appropriate.
* `click` on collapse-selector: Whenever a `collapse-selector` is clicked, the corresponding
        checkbox is toggled.

Functions
---------

* `init_collapse`: Dom -> ()
    Call this on a Dom object (an element, the document, a document fragment; basically
    anything with `querySelector` and `querySelectorAll`) to initialize all collapsables
    within that element.

******/


function init_collapse(container) {
    function update_collapse(dom) {
        var add = dom.checkbox.checked ? 'collapse-open' : 'collapse-closed'
        var rem = dom.checkbox.checked ? 'collapse-closed' : 'collapse-open'
        dom.blocks.forEach(function (block) {
            block.classList.add(add)
            block.classList.remove(rem)
        })
    }

    var items = container.querySelectorAll('.collapse-head')
    items.forEach(function (item) {
        var collapse_id = item.getAttribute('data-collapse')
        var dom = {
            head: item,
            selector: container.querySelector(".collapse-selector[data-collapse='"+collapse_id+"']"),
            checkbox: item.querySelector("input[type=checkbox]"),
            blocks: container.querySelectorAll(".collapse[data-collapse='"+collapse_id+"']"),
        }
        update_collapse(dom)
        dom.checkbox.addEventListener('change', function (e) { update_collapse(dom) })
        dom.selector.addEventListener('click', function (e) {
            dom.checkbox.checked = !dom.checkbox.checked
            dom.checkbox.dispatchEvent(new Event('change'))
        })
    })

}

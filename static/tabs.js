/******

Tabs through semantic Html.

Classes
-------
* `tabs-head`: Contains at least the input elements for the state of the tab set.
* `tab-selector`: When clicked, changes the active tab.
* `tabpane`: An element which belongs to a tab pane.
* `tab-open`: An element belonging to a tab pane which is active.
* `tab-close`: An element belonging to a tab pane which is not active.

Dom
---
* `.tabs-head input[type='radio']`: By having the state of tab sets be stored
        by an HTML control element, browsers can make decisions about maintaining that
        state beyond the lifetime of the page view. The associated control for a
        tab set is the radiogroup in `tabs-head` named like the head's `data-tabs`.
* `data-tabs` attribute: This links up elements that are part of the same tab set.
        On a tab pane, it specifies which tab set the pane belongs to.
        On a `tabs-head`, it specifies thich tab set its radio buttons control.
        On a tab selector, it specifies which radioset to control on click.
* `data-tabpane` attribute: This links up elements within a tab set that are part of the same tab pane.
        On a tab pane, it specifies which tab pane within a tab set the element belongs to.
        On a tab selector, it specifies which tab pane should be made active.
        It is not set on the radio buttons in `tab-head`; rather, use `value`.
* `value` attribute on `input[type='radio']`: For the tab set's radio controls, use this
        instead of `data-tabpane`.
* `name` attribute on `input[type='radio']`: This is not necessarily the same as `data-tabs`,
        since `data-tabs` is already implicitly namespaces by the name of the attribute,
        but `name` is not namespaced, so the user may need to distinguish between sets of
        radio buttons using a different identifier.

Interaction
-----------
* `change` on radioset: Whenever the controlling radioset changes state,
        the `tab-{open,closed}` classes are disbursed as appropriate.
* `click` on tab-selector: Whenever a `tab-selector` is clicked, the corresponding
        radio button is set.

Functions
---------

* `init_tabs`: Dom -> ()
    Call this on a Dom object (an element, the document, a document fragment; basically
    anything with `querySelector` and `querySelectorAll`) to initialize all tab sets
    within that element.

******/

function init_tabs(container) {
    function update_tabs(dom) {
        dom.radiogroup.forEach((radio) => {
            if (radio.checked) {
                var pane_id = radio.getAttribute('value')
                dom.panes.forEach((pane) => {
                    if (pane.getAttribute('data-tabpane') === pane_id) {
                        pane.classList.add('tab-open')
                        pane.classList.remove('tab-closed')
                    }
                    else {
                        pane.classList.add('tab-closed')
                        pane.classList.remove('tab-open')
                    }
                })
            }
        })
    }

    var items = container.querySelectorAll('.tabs-head')
    items.forEach((item) => {
        var tab_id = item.getAttribute('data-tabs')
        var dom = {
            head: item,
            selectors: container.querySelectorAll(".tab-selector[data-tabs='"+tab_id+"'][data-tabpane]"),
            radiogroup: item.querySelectorAll("input[type='radio'][data-tabs='"+tab_id+"']"),
            panes: container.querySelectorAll(".tabpane[data-tabs='"+tab_id+"']")
        }
        update_tabs(dom)

        dom.radiogroup.forEach((radio) => {
            radio.addEventListener('change', function (e) { update_tabs(dom) })
        })
        dom.selectors.forEach((selector) => {
            var pane_id = selector.getAttribute('data-tabpane')
            selector.addEventListener('click', function (e) {
                dom.radiogroup.forEach((radio) => {
                    if (radio.getAttribute('value') === pane_id) {
                        radio.checked = true
                        radio.dispatchEvent(new Event('change'))
                    }
                })
                e.preventDefault()
                e.stopPropagation()
            })
        })
    })
}

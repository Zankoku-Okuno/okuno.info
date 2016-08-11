function init_collapse(container) {
    function update_collapse(dom) {
        var add = dom.checkbox.checked ? 'collapse-open' : 'collapse-closed'
        var rem = dom.checkbox.checked ? 'collapse-closed' : 'collapse-open'
        dom.blocks.forEach((block) => {
            block.classList.add(add)
            block.classList.remove(rem)
        })
    }

    var items = container.querySelectorAll('.collapse-head')
    items.forEach((item) => {
        var collapse_id = item.getAttribute('data-collapses')
        var dom = {
            head: item,
            selector: container.querySelector(".collapse-selector[data-collapses='"+collapse_id+"']"),
            checkbox: item.querySelector("input[type=checkbox]"),
            blocks: container.querySelectorAll(".collapse[data-collapses='"+collapse_id+"']"),
        }
        update_collapse(dom)
        dom.checkbox.addEventListener('change', function (e) { update_collapse(dom) })
        dom.selector.addEventListener('click', function (e) {
            dom.checkbox.checked = !dom.checkbox.checked
            dom.checkbox.dispatchEvent(new Event('change'))
        })
    })

}

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

    var items = container.querySelectorAll('.tab-head')
    items.forEach((item) => {
        var tab_id = item.getAttribute('data-tabs')
        var dom = {
            head: item,
            selectors: container.querySelectorAll(".tab-selector[data-tabs='"+tab_id+"'][data-tabpane]"),
            radiogroup: item.querySelectorAll("input[data-tabs='"+tab_id+"']"),
            panes: container.querySelectorAll(".tab[data-tabs='"+tab_id+"']")
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
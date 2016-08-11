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
            radiogroup: item.querySelectorAll("input[data-tabs='"+tab_id+"']"),
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

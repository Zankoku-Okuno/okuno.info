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

document.addEventListener("DOMContentLoaded", function () {
    document.querySelectorAll(".action_item[data-pk] select[data-tabs]").forEach(function (controller) {
        controller.value = 'view'
    })
    patch_dom(document)
})

function init_textarea(dom) {
    dom.querySelectorAll("textarea").forEach(autoResize)
}

function init_nav(dom) {
    dom.querySelectorAll(".nav ul > li a").forEach(function (link) {
        if (link.href === document.location.href) {
            link.dataset.old_href = link.href
            link.removeAttribute('href')
        }
    })
}

function init_markdown(dom) {
    dom.querySelectorAll(".markdown").forEach(function (md) {
        md.innerHTML = markdown.toHTML(md.innerHTML)
    })
}

function init_tabs(dom) {
    dom.querySelectorAll("select[data-tabs]").forEach(function (controller) {
        function update_tabs() {
            var tabset = controller.dataset.tabs
            var new_tab = controller.value
            document.querySelectorAll("*[data-tab][data-tabset='"+tabset+"']").forEach(function (tab) {
                tab.classList[tab.dataset.tab === new_tab ? 'remove' : 'add']("hidden")
            })
        }
        update_tabs()
        controller.addEventListener("change", update_tabs)
    })
}

function init_multi_select(dom) {
    // FIXME this structure needs a controller full of the tags that are active
    dom.querySelectorAll("select[data-multi_select]").forEach(function (select) {
        var set = select.dataset.multi_select
        var accum = dom.querySelector(".accum[data-multi_select='"+set+"']")
        
        select.addEventListener('change', function (event) {
            var select = event.target
            var option = select.options[select.selectedIndex]
            select.selectedIndex = 0
            event.preventDefault()
            if (option.value === "new") {
                accum.dispatchEvent(new CustomEvent('new'))
            }
            else {
                var detail = {
                    value: option.value,
                    label: option.label,
                }
                accum.dispatchEvent(new CustomEvent('add', { detail: detail }))
            }
        })
        
        function init_delete_buttons(dom) {
            var toDelete = (function () {
                var children = []
                for (var i = 0, e = dom.children.length; i < e; ++i) {
                    children.push(dom.children.item(i))
                }
                return children
            })()
            dom.querySelectorAll(".multi_select_delete").forEach(function (del) {
                del.addEventListener('click', function () {
                    var detail = {
                        toDelete: toDelete
                    }
                    accum.dispatchEvent(new CustomEvent('delete', { detail: detail }))
                })
            })
        }

        accum.addEventListener('add', function (event) {
            var content = accum.querySelector("template.multi_select_add").content.cloneNode(true)
            content.querySelectorAll(".multi_select_label").forEach(function (x) { x.textContent = event.detail.label })
            content.querySelectorAll(".multi_select_value").forEach(function (x) { x.value = event.detail.value })
            patch_dom(content)
            init_delete_buttons(content)
            accum.append(content)
        })

        accum.addEventListener('new', function () {
            var template = accum.querySelector("template.multi_select_new")
            var content = template.content.cloneNode(true)
            patch_dom(content)
            init_delete_buttons(content)
            var input = content.querySelector("input") // must be above `.append` b/c that drains elements from `n`
            accum.append(content)
            input.focus() // must be below `.append` b/c it works only on elements that are part of the dom
        })

        accum.addEventListener('delete', function (event) {
            event.detail.toDelete.forEach(function (child) { child.remove() })
        })
    })
}

function init_put_forms(dom) {
    dom.querySelectorAll("form[method='PUT']").forEach(function (form) {
        form.addEventListener("submit", function (e) {
            e.preventDefault()

            var params = {}
            form.querySelectorAll("*[name]").forEach(function (data) {
                if (data.value === "") { return } // FIXME is this right?
                if (params[data.name] === undefined) {
                    params[data.name] = [data.value]
                }
                else {
                    params[data.name].push(data.value)
                }
            })

            Http.put({
                url: form.action,
                query: params,
                headers: {
                    'Accept': "application/htmlfrag+json"
                },
            })
            .then(function (response) {
                var event_name = form.querySelector("input[name='id']") === null
                               ? 'create'
                               : 'update'
                var detail = {
                    id: response.body.id,
                    fragment: (function () {
                            var tmp = document.createElement('template')
                            tmp.innerHTML = response.body.htmlfrag
                            return tmp.content
                        })()
                }
                form.dispatchEvent(new CustomEvent(event_name, { detail: detail }))
            })
            .catch(console.log) // TODO
        })
    })
}

function init_cancel_button(dom) {
    var controller = dom.querySelector("select[data-tabs]")
    if (controller === null) { return }
    var view = controller.querySelector("option[value='view']")
    if (view === null) { return }
    dom.querySelectorAll("*[data-tab='edit'] form").forEach(function (form) {
        form.addEventListener("reset", function () {
            controller.value = 'view'
            controller.dispatchEvent(new Event("change"), {})
        })
    })
}

function init_action_item_forms(dom) {
    // adjust the dom after successful action_item persistence
    dom.querySelectorAll("form.action_item").forEach(function (form) {
        form.addEventListener('create', function (event) {
            var li = document.createElement('li')
            li.classList.add("action_item")
            li.dataset['pk'] = event.detail.id
            li.append(event.detail.fragment)
            document.querySelector(".action_items[data-project='']").prepend(li)
            patch_dom(li)
            form.reset()
        })

        form.addEventListener('update', function (event) {
            document.querySelectorAll(".action_item[data-pk='"+event.detail.id+"']").forEach(function (item) {
                item.innerHTML = ''
                item.append(event.detail.fragment)
                patch_dom(item)
            })
        })
    })
}

function init_item_forms(dom, options) {
    dom.querySelectorAll("form."+options.item_class).forEach(function (form) {
        form.addEventListener('create', function (event) {
            var li = document.createElement('li')
            li.classList.add(options.item_class)
            li.dataset['pk'] = event.detail.id
            li.append(event.detail.fragment)
            document.querySelector("."+options.group_class).prepend(li)
            patch_dom(li)
            form.reset()
        })

        form.addEventListener('update', function (event) {
            document.querySelectorAll("."+options.item_class+"[data-pk='"+event.detail.id+"']").forEach(function (item) {
                item.innerHTML = ''
                item.append(event.detail.fragment)
                patch_dom(item)
            })
        })
    })
}

function patch_dom(dom) {
    init_textarea(dom)
    init_nav(dom)
    init_markdown(dom)
    init_tabs(dom)
    init_multi_select(dom)
    dom.querySelectorAll(".action_item[data-pk]").forEach(init_cancel_button)
    dom.querySelectorAll(".project[data-pk]").forEach(init_cancel_button)
    dom.querySelectorAll(".tag[data-pk]").forEach(init_cancel_button)
    init_put_forms(dom)
    init_action_item_forms(dom)
    init_item_forms(dom, {
        item_class: "project",
        group_class: "projects",
    })
    init_item_forms(dom, {
        item_class: "tag",
        group_class: "tags",
    })

    document.querySelectorAll(".action_item[data-pk]").forEach(function (item) {
        var tabset = "action_item_"+item.dataset["action_item"]
        var controller = item.querySelector("select[data-tabs='"+tabset+"'")
        var form = item.querySelector("form[method='PUT'].action_item")
        if (controller !== null && form !== null) {
            controller.addEventListener('change', function () {
                form.reset()
            })
        }
    })
}


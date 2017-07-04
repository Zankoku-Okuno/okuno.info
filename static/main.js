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

function init_put_forms(dom) {
    dom.querySelectorAll("form[method='PUT']").forEach(function (form) {
        form.addEventListener("submit", function (e) {
            e.preventDefault()

            var params = {}
            form.querySelectorAll("*[name]").forEach(function (data) {
                if (data.value === "") { return }
                params[data.name] = data.value
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


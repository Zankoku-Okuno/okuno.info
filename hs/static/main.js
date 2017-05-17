document.addEventListener("DOMContentLoaded", function () {
    patch_dom(document)
})

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

function init_action_item_forms(dom) {
    dom.querySelectorAll("form[method='PUT'][action='/action-item']").forEach(function (form) {
        form.addEventListener("submit", function (e) {
            e.preventDefault()

            var params = {}
            form.querySelectorAll("*[name]").forEach(function (data) {
                if (data.value === "") { return }
                params[data.name] = data.value
            })

            Http.put({
                url: "/action-item",
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
                            tmp.innerHTML = response.body.action_item
                            return tmp.content
                        })()
                }
                form.dispatchEvent(new CustomEvent(event_name, { detail: detail }))
            })
            .catch(console.log) // TODO
        })
    })

    // adjust the dom after successful action_item persistence
    dom.querySelectorAll("form[action='/action-item']").forEach(function (form) {
        form.addEventListener('create', function (event) {
            var li = document.createElement('li')
            li.dataset['action_item'] = event.detail.id
            li.append(event.detail.fragment)
            document.querySelector("#action_items").prepend(li)
            patch_dom(li)
            form.reset()
        })

        form.addEventListener('update', function (event) {
            document.querySelectorAll("*[data-action_item='"+event.detail.id+"']").forEach(function (item) {
                item.innerHTML = ''
                item.append(event.detail.fragment)
                patch_dom(item)
            })
        })
    })
}

function patch_dom(dom) {
    init_tabs(dom)
    init_action_item_forms(dom)
}


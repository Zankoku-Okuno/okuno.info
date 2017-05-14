document.addEventListener("DOMContentLoaded", function () {
    document.querySelectorAll("form[method='PUT'][action='/action-item']").forEach(function (form) {
        form.addEventListener("submit", function (e) {
            e.preventDefault()

            var params = {}
            form.querySelectorAll("*[name]").forEach(function (data) {
                if (data.value === "") { return }
                params[data.name] = data.value
            })

            Http.put({ // FIXME grab method from form
                url: "/action-item", // FIXME grab action from form
                query: params,
                headers: {
                    'Accept': "text/html+frag"
                },
                responseType: 'text',
            })
            .then(function (response) {
                // FIXME fire an event from the form
                var fragment = (function () {
                    var tmp = document.createElement('template')
                    tmp.innerHTML = response.body
                    return tmp.content
                })()

                document.querySelector("#action_items").prepend(fragment)
                form.reset()
            })
            .catch(console.log) // TODO
        })
    })
})
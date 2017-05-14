document.addEventListener("DOMContentLoaded", function () {
    var form = document.querySelector("form#create-action-item")
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
                'Accept': "text/html+frag"
            },
            responseType: 'text',
        })
        .then(function (response) {
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
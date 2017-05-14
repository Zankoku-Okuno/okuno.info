document.addEventListener("DOMContentLoaded", function () {
    var dom = document.querySelector("form#create-action-item")
    dom.addEventListener("submit", function (e) {
        e.preventDefault()

        var params = {}
        dom.querySelectorAll("*[name]").forEach(function (data) {
            if (data.value === "") { return }
            params[data.name] = data.value
        })

        Http.put({
            url: "/delme",
            query: params,
            responseType: 'text'
        }).then(console.log)
    })
    
})
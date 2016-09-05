// this just puts static markdown
// TODO make a markdown panel that responds to changes in a text input
function init_markdown(container) {
    var items = container.querySelectorAll(".md")
    items.forEach(function (item) {
        item.innerHTML = markdown.toHTML(item.textContent)
    })
}
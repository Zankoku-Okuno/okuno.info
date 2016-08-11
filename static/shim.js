if (NodeList.prototype.forEach === undefined) {
    NodeList.prototype.forEach = Array.prototype.forEach;
}

function createHTMLFragment(htmlText) {
    var tpl = document.createElement('template') // FIXME why do I need to use a template? can't I use a div for increased protability?
    tpl.innerHTML = htmlText
    return tpl.content
}
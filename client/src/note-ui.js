import templateContent from "./note-ui.html"
const template = document.createElement("template")
template.innerHTML = templateContent


export default class NoteUi extends HTMLElement {
    constructor() {
        super()

        const shadow = this.attachShadow({ mode: "open" })
        shadow.appendChild(template.content.cloneNode(true))

        this.viewBtn = shadow.querySelector("button[name='view']")
        this.editBtn = shadow.querySelector("button[name='edit']")
        this.viewPanel = shadow.querySelector(".viewPanel")
        this.editPanel = shadow.querySelector(".editPanel")

        this.viewText = shadow.querySelector("p")
        this.editText = shadow.querySelector("textarea[name='text']")

        this.form = shadow.querySelector("form")
    }

    connectedCallback() {
        this.viewText.textContent = this.text
        this.editText.defaultValue = this.text

        this.viewPanel.hidden = this.panel !== "view"
        this.editPanel.hidden = this.panel !== "edit"
        this.viewBtn.addEventListener("click", () => {
            this.viewPanel.hidden = false
            this.editPanel.hidden = true
        })
        this.editBtn.addEventListener("click", () => {
            this.viewPanel.hidden = true
            this.editPanel.hidden = false
        })

        this.form.addEventListener("submit", (evt) => {
            evt.preventDefault()
            evt.stopPropagation()
            this.dispatchEvent(new Event("save"))
        })
    }

    get text() { return this.textContent }
    //     if (this._text === undefined) {
    //         const text = this.textContent
    //         this._text = text === null ? "" : text.textContent
    //     }
    //     return this._text
    // }

    get pk() { return this.getAttribute("pk") }

    get panel() { return this.getAttribute("panel") || "view" }

}

customElements.define('note-ui', NoteUi)

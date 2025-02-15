import './ace.js'
import './mode-json.js'

// This implementation is based on http://juicy.github.io/juicy-ace-editor/ 
// and https://github.com/LostInBrittany/ace-widget

let template = document.createElement("template")
template.innerHTML = `    
    <style>
        :host{
            display: block;
            width: 100%;
        }
        #ace-editor-container{
            height: 100%;
            margin-top: -65px;
            border: 1px solid #e0e0e0;
            border-radius: 2px;
        }
    </style>
    <div id="ace-editor-container"></div>
`

// Shim Shadow DOM styles if needed
if (window.ShadowDOMPolyfill) {
    WebComponents.ShadowCSS.shimStyling(template.content, "ace-editor")
}

// Fix focus issues in Safari and Firefox
function editorFocus() { 
    let _self = this
    setTimeout(() => {
        if (!_self.isFocused()) {
            _self.textInput.focus()
        }
    })
    this.textInput.$focusScroll = "browser"
    this.textInput.focus()
}

// Creates an object based in the HTML Element prototype
window.customElements.define("ace-editor", class AceEditor extends HTMLElement {
    get editorValue() {
        return this.textContent
    }

    // List of observed attributes
    static get observedAttributes() {
        return ["theme", "mode", "fontsize", "softtabs", "tabsize", "readonly"
               , "wrapmode", "min-lines", "max-lines", "shadow-style"]
    }

    // Fires when an instance of the element is created
    constructor(self) {
        // Polyfill caveat we need to fetch the right context
        // https://github.com/WebReflection/document-register-element/tree/master#v1-caveat
        self = super(self)
        // Creates the shadow root
        let shadowRoot = null
        if (self.attachShadow && self.getRootNode) {
            shadowRoot = self.attachShadow({mode: "open"})
        } else {
            shadowRoot = self.createShadowRoot()
        }
        // Adds a template clone into shadow root
        let clone = document.importNode(template.content, true)
        // getElementById may not be polyfilled yet
        self.container = clone.querySelector("#ace-editor-container")
        shadowRoot.appendChild(clone)
        return self
    }

    connectedCallback() {
        let text = this.childNodes[0]
        let container = this.container
        let element = this
        let editor = null

        if (this.editor) {
            editor = this.editor
        } else {
            const options = {}
            // Support autoresizing
            if (this.hasAttribute("max-lines")) {
                options.maxLines = Number(this.getAttribute("max-lines"))
            }
            if (this.hasAttribute("min-lines")) {
                options.minLines = Number(this.getAttribute("min-lines"))
            }
            
            editor = ace.edit(container, options)
            this.dispatchEvent(new CustomEvent("editor-ready", {bubbles: true, composed: true, detail: editor}))
            this.editor = editor
            this.editor.focus = editorFocus

            // Inject base editor styles
            this.injectTheme("#ace_editor\\.css")

            editor.getSession().on("change", (event) => {
                this.textContent = this.editor.getSession().getValue()
                if (!this.editor.isFocused()) {
                    this.editor.focus()  // Bizarrely, the editor loses focus after getValue() in Chrome 
                }
                element.dispatchEvent(new CustomEvent("change", {bubbles: true, composed: true, detail: event}))
            })
        }

        // Handle theme changes
        editor.renderer.addEventListener("themeLoaded", this.onThemeLoaded.bind(this))

        // Initial attributes
        editor.setTheme(this.getAttribute("theme"))
        editor.setFontSize(Number(this.getAttribute("fontsize")) || 12)
        editor.setReadOnly(this.hasAttribute("readonly"))
        let session = editor.getSession()
        session.setMode(this.getAttribute("mode"))
        session.setUseSoftTabs(this.getAttribute("softtabs"))
        if (this.getAttribute("tabsize")) {
            session.setTabSize(this.getAttribute("tabsize"))
        }
        session.setUseWrapMode(this.hasAttribute("wrapmode"))
        // non-Ace specific
        if (this.hasAttribute("shadow-style")) {
            this.injectTheme(this.getAttribute("shadow-style"))
        }

        editor.focus()

        // Observe input textNode changes
        // Could be buggy as editor was also added to Light DOM
        let observer = new MutationObserver(function(mutations) {
            mutations.forEach((mutation) => {
            if (mutation.type == "characterData") {
                element.value = text.data
            }
            })
        })
        if (text) {
            observer.observe(text, {characterData: true})
        }
        this._attached = true
    }

    disconnectedCallback() {
        this._attached = false
    }

    attributeChangedCallback(attr, oldVal, newVal) {
        if (!this._attached) {
            return false
        }
        switch (attr) {
            case "theme":
                this.editor.setTheme(newVal)
                break
            case "mode":
                this.editor.getSession().setMode(newVal)
                break
            case "fontsize":
                this.editor.setFontSize(newVal)
                break
            case "softtabs":
                this.editor.getSession().setUseSoftTabs(newVal)
                break
            case "tabsize":
                this.editor.getSession().setTabSize(newVal)
                break
            case "readonly":
                this.editor.setReadOnly(newVal === "" || newVal)
                break
            case "wrapmode":
                this.editor.getSession().setUseWrapMode(newVal !== null)
                break
            case "max-lines":
                this.editor.renderer.$maxLines = Number(newVal)
                break
            case "min-lines":
                this.editor.renderer.$minLines = Number(newVal)
                break
            // non-Ace specific
            case "shadow-style":
                if (oldVal) {
                    this.shadowRoot.querySelector(oldVal).remove()
                }
                this.injectTheme(newVal)
                break
        }
    }

    onThemeLoaded(e) {
        var themeId = "#" + e.theme.cssClass
        this.injectTheme(themeId)
        // Work around Chrome-stable bug, force repaint
        this.container.style.display = "none"
        this.container.offsetHeight
        this.container.style.display = ""
    }

    /**
     * Injects a style element into ace-editor"s shadow root
     * @param {CSSSelector} selector for an element in the same shadow tree or document as `ace-editor`
     */
    injectTheme(selector) {
        const lightStyle = this.getRootNode().querySelector(selector) || document.querySelector(selector)
        if (lightStyle !== null) {
            this.shadowRoot.appendChild(lightStyle.cloneNode(true))
        }
    }
})

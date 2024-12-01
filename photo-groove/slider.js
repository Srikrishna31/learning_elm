/*
Importing Custom elements

To use the third party Javascript slider in Elm, we'll first wrap it in a custom element.

Custom Elements
Custom Elements are a part of the Web Components specification. Here's how they work:
* We write some Javascript code to define a new DOM element type. In this case, the new
element will be called a <range-slider>.(Custom elements must have a dash in their names).
* We run that Javascript code when our page loads, to register this custom element with browser.
* From now on, whenever any code-in Javascript or in Elm-create a DOM element whose tag is "range-slider"
that element will behave according to this custom logic.

Custom elements are implemented in Javascript. They may throw runtime exceptions.
 */
class RangeSlider extends HTMLElement {
    connectedCallback() {
        let input = document.createElement("input");
        this.appendChild(input);

        const jsr = new JSR(input, {
            max: this.max, // Uses the Attr.max value we set in Elm.
            values: [this.val], // Uses the Attr.property "val" we set in Elm.
            sliders: 1,
            grid: false
        });

        const rangeSliderNode = this;

        jsr.addEventListener("update", function(elem, value) {
            const event = new CustomEvent("slide", {
                detail: {usersSlidTo: value}
            });
            rangeSliderNode.dispatchEvent(event);
        })
    }

}
    window.customElements.define("range-slider", RangeSlider);


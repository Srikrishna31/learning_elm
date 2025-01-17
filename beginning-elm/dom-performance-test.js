let someString = "some string";

function updateString() {
    for (let i = 0; i <= 10000; ++i) {
        someString = "updated string";
    }
}

let t1 = performance.now();
updateString();
let t2 = performance.now();

console.log ("It took " + (t2 - t1) + " milliseconds to update a string");


let newDiv = document.createElement("div");
let newText = document.createTextNode("some text");


newDiv.appendChild(newText);
document.body.appendChild(newDiv);

function updateDOM() {
    for (let i = 0; i <= 10000; ++i) {
        newDiv.innerHTML = "updated text";
    }
}

t1 = performance.now();
updateString();
t2 = performance.now();

console.log ("It took " + (t2 - t1) + " milliseconds to update a DOM element");
/*
The object app, lets us subscribe to data that Elm sends to JavaScript via ports like setFilters. When the Elm
Runtime executes the Cmd returned by setFilters, the callback function we've passed to app.ports.setFilters.subscribe
will run. The options argument it accepts is the FilterOptions record Elm set over, but converted from an Elm record
to a JavaScript object.

Because all Elm values must be immutable, mutable values (such as JavaScript objects and arrays) can't be sent
through the port. Instead they automatically get copied into immutable data structures. This process has some
overhead. Passing in values like strings and numbers, which are already immutable, has no such overhead.
 */
const app = Elm.PhotoGroove.init({
    node: document.getElementById("app"),
    flags: Pasta.version
});

app.ports.setFilters.subscribe(function(options) {
    /*
        Synchronizing with the Elm Runtime
        The JavaScript function requestAnimationFrame allows code to run just before the browser's next repaint.
        Bacause this is when the Elm Runtime will schedule its next DOM update, we can use requestAnimationFrame
        to delay our call to document.getElementById("main-canvas")  until after our next view has added the <canvas>
        we need to the DOM.
     */
    requestAnimationFrame(function() {
        Pasta.apply(document.getElementById("main-canvas"), options);
    });
});

// Pasta calls this function whenever its activity changes.
Pasta.addActivityListener(function(activity) {
    console.log("Got some activity to send to Elm: ", activity);
    app.ports.activityChanges.send(activity);
})

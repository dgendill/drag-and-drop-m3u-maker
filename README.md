## Drag and Drop M3U Maker

[Available here](http://htmlpreview.github.io/?http://github.com/dgendill/drag-and-drop-m3u-maker/blob/master/index.html) or checkout the repo.

## Customization

You can inject the m3u maker into any page by including [jsmediatags](https://github.com/aadsm/jsmediatags) and `app.js` on the page, and then running `run` with the following configuration. The `jsmediatags` variable must be exposed on window.

```
PS.Main.run({
  elements : {
    dropzone : ElementIDString,
    errors : ElementIDString,
    textarea : ElementIDString
  },
  ops : PS.Main.defaultFunctionalityConfig
})();
```

For example:

```
PS.Main.run({
  elements : {
    // Set the dropzone to element with id #dropzone
    dropzone : "dropzone",
    // Set where errors get displayed.  Elemenet with id #errors
    errors : "errors",
    // Set where the m3u is displayed. Element with id #m3u
    textarea : "m3u",
  },
  ops : PS.Main.defaultFunctionalityConfig
})();
```

'use strict';

exports.readyImpl = function(fn) {
    document.addEventListener("DOMContentLoaded", function(event) { 
        fn();
    });
}

exports.getElementByIdImpl = function(c, just, nothing) {
  var r = document.getElementById(c);
  if (r) {
    return just(r);
  } else {
    return nothing;
  }
}

exports.getElementsByClassNameImpl = function(c, just, nothing) {
  var r = document.getElementsByClassName(c);
  if (r.length > 0) {
    return just(r);
  } else {
    return nothing;
  }
}

exports.setInnerHTMLImpl = function(content, element) {
  element.innerHTML = content;
}

exports.prependChildImpl = function(parent, child) {
  parent.prepend(child);
}

exports.setDataAttributeImpl = function(element, attr) {
  element.setAttribute('data', attr);
}



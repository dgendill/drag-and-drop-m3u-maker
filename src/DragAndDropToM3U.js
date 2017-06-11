function fileUrl(file) {
  return URL.createObjectURL(file);
}

function injectAudio(url, file, element) {
  var audio = document.createElement('audio');
  audio.src = url;
  audio.setAttribute('controls','');
  audio.setAttribute(file.type);
  element.appendChild(audio);
  return audio;
}

// AudioElement -> Aff (dom :: DOM) Duration
function audioDuration(audio) {
   if (!isNaN(audio.duration)) {
     return function(success, fail) {
       success(audio.duration);
     }
   } else {
     return function(success, fail) {
       var timeoutid = window.setTimeout(function() {
          audio.removeEventListener('durationchange', onchange);
          fail(new Error("It took too long to get the audio's duration."));
       }, 10000);

       var onchange = function() {
         clearTimeout(timeoutid);
         audio.removeEventListener('durationchange', onchange);
         success(audio.duration);
       }

       audio.addEventListener('durationchange', onchange);

     }
   }
}

function m3uStart() {
  return "#EXTM3U\n";
}

// File -> AudioElement -> Aff (dom :: DOM) M3UString
function fileToM3U(file, audio) {
    var str = "";
    return function(success, fail) {
        audioDuration(audio)(function(duration) {
            str += "#EXTINF:" + Math.ceil(duration) + ",";
            jsmediatags.read(file, {
              onSuccess: function(tag) {

                if (tag.tags.artist) {
                    str += tag.tags.artist;
                } else {
                    fail("no artist");
                }

                if (tag.tags.title) {
                    str += " - " + tag.tags.title + "\n";
                } else {
                    fail("no title");
                }

                str += file.name + "\n";

                success(str);

              },
              onError: function(error) {
                console.log(':(', error.type, error.info);
              }
            });
        }, function() {
            fail(new Error('failed to read audio duration.'))
        })

    }
}

function traverse(arr) {
    var r = "";
    var f = false;

    return function(success, fail) {
        function step(arr, i) {
            arr[i](function(str) {
                r += str;
                if (i + 1 == arr.length) {
                    f ? fail(f) : success(r);
                } else {
                    step(arr, i+1)
                }
            }, function(err) {
                f = err;
                if (i + 1 == arr.length) {
                    fail(f);
                } else {
                    step(arr, i+1)
                }
            });
        }

        step(arr, 0);
    }
}

function drop_handler(ev) {
  ev.preventDefault();

  // If dropped items aren't files, reject them
  var dt = ev.dataTransfer;
  if (dt.items) {
    // Use DataTransferItemList interface to access the file(s)
    var m3u = Array.prototype.reduce.apply(
      dt.items, [
      function(acc, item, index, arr) {
          console.log(acc, item, index, arr);
          if (item.kind == "file") {
            var f = item.getAsFile();

            var url = fileUrl(f);
            console.log(url);
            var audio = injectAudio(url);

            var indexC = index;

            // Array (Aff (dom :: DOM) String)
            acc.push(
                function(success, fail) {
                    fileToM3U(f, audio)(function(str) {
                      // arr[indexC] = str;
                      success(str);
                    }, function(err) { console.error(err); });
                }
            );
          }
          return acc;


     }, []
    ]);

    traverse(m3u)(function(str) {
       console.log(m3uStart() + str);
    }, function(err) {
       console.log("fail", err)
    })

  } else {
    // Use DataTransfer interface to access the file(s)
    for (var i=0; i < dt.files.length; i++) {
      console.log("... file[" + i + "].name = " + dt.files[i].name);
    }
  }
}
var t = document;
t.ondrop = drop_handler;
t.addEventListener('dragend', function(e) {
    e.preventDefault();

});
t.addEventListener('dragover', function(e) {
     e.preventDefault();
});

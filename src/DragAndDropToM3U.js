'use strict';

exports.fileUrlImpl = function fileUrl(file) {
  return URL.createObjectURL(file);
}

exports.injectAudioHiddenImpl = function injectAudioHiddenImpl(url, file) {
  var audio = document.createElement('audio');
  audio.src = url;
  audio.setAttribute('controls', '');
  audio.setAttribute('type', file.type);
  audio.style.display = "none";
  document.body.appendChild(audio);
  return audio;
}

// AudioElement -> Aff (dom :: DOM) Duration
exports.audioDurationImpl = function audioDurationImpl(audio) {
  return function(success, fail) {

    if (audio.attributes.type.value.indexOf('audio') !== 0) {
      fail(new Error("The file is not an audio file."));
      return;
    }

    if (!isNaN(audio.duration)) {
      success(audio.duration);
    } else {

      var timeoutid = window.setTimeout(function() {
        audio.removeEventListener('durationchange', onchange);
        fail(new Error("It took too long to get the audio's duration."));
      }, 6000);

      var onchange = function() {
        clearTimeout(timeoutid);
        audio.removeEventListener('durationchange', onchange);
        success(audio.duration);
      }

      audio.addEventListener('durationchange', onchange);
    }
  }
}

// File -> Aff () AudioTags
exports.audioTagsImpl = function audioTagsImpl(file) {
  return function(success, fail) {
    var filename = file.name;
    jsmediatags.read(file, {
      onSuccess: function(tag) {
        var data = {};
        if (tag.tags.artist) {
          data.artist = tag.tags.artist;
        } else {
          fail(new Error(filename + ' is missing artist tag.'));
        }

        if (tag.tags.title) {
          data.title = tag.tags.title;
        } else {
          fail(new Error(filename + ' is missing title tag.'));
        }

        data.filename = filename;

        success(data);

      },
      onError: function(error) {
        fail(error);
      }
    });

  }
}

exports.toFileArrayImpl = function(fileList) {
  var acc = [];
  for (var i = 0; i < fileList.length; i++) {
    acc[i] = fileList.item(i);
  }
  return acc;
}

"use strict";

var fileupload, uploadlist, uploadspeed, filelist;
var placeholder = "<p>Drop files to upload.</p>";
var all_files = [];
var currentupload, uploadbutton;

var defaultUploadButton   = "<i class=\"fa fa-upload\"></i> Upload";
var uploadingUploadButton = "<i class=\"fa fa-circle-o-notch fa-spin\"></i> Uploading…";

/*
 * Helper functions
 *
 */

// Human readable file sizes
var readable = function (size) {
  var prefix = ["B", "KB", "MB", "GB"],
      p_indx = 0;

  while (size > 1024 && p_indx < prefix.length) {
    p_indx++;
    size /= 1024;
  }

  return size.toFixed(1) + " " + prefix[p_indx];
}

var checksum = function (file, callback) {

  // calling this function really only makes sense with a callback
  if (typeof callback !== "function") { return; }

  // set up worker
  var worker = new Worker("static/checksum-worker.js");

  worker.onmessage = function (e) {
    callback(e.data);
  }

  worker.postMessage(file);
}

/*
 * UI helper
 *
 */

// Remove upload list elements
var removeUploadElem = function (elem) {

  // remove partial upload from server
  if (elem.hasClass("partial-upload")) {
    $.ajax("files/" + elem.data("partial-checksum") + ".prt", {
      method: "DELETE",
    });
  }

  // remove HTML element
  elem.remove();

  // put placeholder back when removing last element
  if (uploadlist.find("li").length === 0) {
    uploadlist.replaceWith(placeholder);
    uploadlist = $();
    uploadspeed = $();
  }
}

var setProgress = function (elem, percent) {

  // base percentage value
  var base = (elem.data("partial-size") || 0)
           / (elem.data("size") || 1);

  percent = (base * 100 + (1 - base) * percent).toFixed(1);

  // update progress bar
  var progressbar = elem.find(".progress");
  if (progressbar.length === 0) {
    progressbar = $("<span class=\"progress\"></span>");
    elem.find(".filename").append(progressbar);
  }
  progressbar.css("width", percent + "%");

  // show percent behind file size
  var percentage = elem.find(".percent");
  if (percentage.length === 0) {
    percentage = $("<span class=\"percent noselect\"></span>");
    elem.find(".filesize").append(percentage);
  }
  percentage.html(percent + "% uploaded");
}

var setSpeed = function (speed) {
  var txt = readable(speed) + "/s";

  if (!uploadspeed || uploadspeed.length === 0) {
    uploadspeed = $("<span class=\"uploadspeed noselect\"></span>");
    $(".uploadlist button:last").after(uploadspeed);
  }

  uploadspeed.text(txt);
}

var getUploadStatus = function (elem) {
  var stat = $(elem).find(".upload-status");

  if (stat.length === 0) {
    stat = $( "<div class=\"upload-status noselect\">"
            +   "<i class=\"fa\"></i>"
            +   "<div></div>"
            + "</div>"
            );
    $(".main-container", elem).append(stat);
  }

  return stat;
}

var setUploadState = function (elem, icon, html) {

  var stat = getUploadStatus(elem);

  stat.find(".fa")
    .removeClass(function (i, css) {
      //return (css.match(/(?:^|\s)(fa-(?:(?!spin|pulse)\S+))/g) || []).join(" ");
      return (css.match(/(?:^|\s)(fa-\S+)/g) || []).join(" ");
    })
    .addClass(icon);

  stat.find("> div").html(html);

  return stat;
}

var getUploadlist = function () {

  if (uploadlist.length === 0) {

    // Create upload list
    uploadlist = $("<div class=\"uploadlist\"><ul></ul></div>");

    // replace placeholder
    placeholder = fileupload.find("p");
    placeholder.replaceWith(uploadlist);

    // Create upload button
    uploadbutton = $( "<button class=\"upload-all\">"
                    + defaultUploadButton
                    + "</button>");

    uploadbutton.click(function () {
      startUploads();
    });

    uploadlist.append(uploadbutton);
  }

  return uploadlist;
}

// Create new upload list entry
var newUploadElem = function (file, ix) {

  // create HTML element
  var elem = $("<li class=\"new-upload noselect\" data-fileindex=\"" + ix + "\">"
              + "<i class=\"fa fa-times clickable remove\"></i>"
              + "<div class=\"main-container\">"
              +   "<div class=\"filename select\">" + file.name + "</div>"
              +   "<div class=\"filesize\">" + readable(file.size) + "</div>"
              + "</div>"
              + "</li>");

  elem.find(".remove").click(function () {
    removeUploadElem(elem);
  });

  getUploadlist().find("ul").append(elem);

  // start hashing

  var stat = setUploadState(elem, "fa-circle-o-notch fa-spin",
      "Generating checksum (<span class=\"chksm-state\">0</span>% done)."
      ),
      chksm = stat.find(".chksm-state");

  checksum(file, function (data) {
    if (data.done) {
      setUploadState(elem, "fa-check", "Ready to upload.");
      elem.data("checksum", data.result)
          .addClass("ready");
    }
    else {
      chksm.text((data.percent || 0).toFixed(1));
    }
  });

  return elem;
}

var resetUpload = function () {
  uploadbutton
    .html(defaultUploadButton)
    .click(startUploads);
  uploadspeed.remove();
  uploadspeed = $();
}

// Start uploading all files in the current list
var startUploads = function () {

  // find first upload item
  var first = $(".ready:first", uploadlist);

  if (first.length === 0) {
    resetUpload();
    return;
  }

  // get first file
  var ix    = first.data("fileindex"),
      file  = all_files[ix],
      chksm = first.data("checksum");

  // callbacks
  var callbacks = {
        success: function (data) {
          // create new list entry
          filelist.prepend("<li>" + data + "</li>");

          // continue with next file
          removeUploadElem(first);
          startUploads();
        },

        progress: function (progress) {
          setProgress(first, progress);
        },

        speed: setSpeed,
      };

  if (first.hasClass("partial-upload")) {

    var blob = file.slice(parseInt(first.data("partial-size"))),
        ps   = first.data("partial-size"),
        pcs  = first.data("partial-checksum"),
        tcs  = first.data("checksum");

    // perform partial upload
    resumeUpload(blob, ps, pcs, tcs, callbacks);
  }
  else {
    // perform upload
    upload(file, chksm, callbacks);
  }

  setUploadState(first, "fa-upload fading", "Uploading file…");

  // change upload button click handler
  uploadbutton
    .unbind("click")
    .click(function () {
      // TODO
    });

  // change upload button content
  uploadbutton.html(uploadingUploadButton);
}

/*
 * Partial uploads
 *
 */

var resumePartialUpload = function (partial, file, ix) {

  var stat = setUploadState(partial,
      "fa-circle-o-notch fa-spin",
      "Verifying checksum (<span class=\"chksm-state\">0</span>% done)."
      ),
      chksm = stat.find(".chksm-state");

  checksum(file, function (data) {
    if (data.done) {
      if (data.result === partial.data("checksum")) {
        setUploadState(partial, "fa-check", "Ready to upload.");
        partial.addClass("ready");
        partial.data("fileindex", ix);
      }
      else {
        var s = setUploadState(partial,
          "fa-exclamation",
          "File does not match partial upload.<br/>"
          + "<a class=\"clickable replace\">Replace upload</a>"
          + " or "
          + "<a class=\"clickable new\">create a new file</a>?"
          );
        s.find(".replace").click(function () {
          // TODO
        });
        s.find(".new").click(function () {
          // TODO
        });
      }
    }
    else {
      chksm.text((data.percent || 0).toFixed(1));
    }
  });
}

/*
 * Upload functions
 *
 */

// XHR object with upload progress built in
var uploadXHR = function (speed_cb, progress_cb) {

  // default XHR object
  var xhrobj = $.ajaxSettings.xhr();

  if ((typeof speed_cb === "function" || typeof progress_cb === "function") && xhrobj.upload) {

    // closures for last checked file position, last checked time and last speed
    var lst, lst_t, lst_speed = 0;

    xhrobj.upload.addEventListener("progress", function (e) {

      var pos = e.loaded || e.position,
          tot = e.total;

      // Call progress callback
      if (e.lengthComputable && typeof progress_cb === "function") {
        progress_cb(pos / tot * 100);
      }

      // Call speed callback
      if (typeof speed_cb === "function" && pos != undefined && pos != tot) {

        // difference in time
        var t = new Date(),
            d_t = t - lst_t;

        // calculate speed from difference in position
        var d_pos = pos - (lst != undefined ? lst : 0),
            speed = d_t > 0 ? (d_pos / (d_t / 1000)) : 0;

        // build average over two time steps
        speed_cb( (speed + lst_speed) / 2 );

        // replace old values
        lst       = pos;
        lst_t     = t;
        lst_speed = speed;
      }
    }, false);
  }

  return xhrobj;
}

var performUpload = function (url, formdata, settings) {

  console.log(url, formdata, settings);

  var success_cb  = settings.success,
      speed_cb    = settings.speed,
      progress_cb = settings.progress;

  // start upload
  currentupload = $.ajax({

    xhr: function () { return uploadXHR(speed_cb, progress_cb); },

    // upload url & data
    type: "POST",
    url: url,
    data: formdata,
    contentType: false,
    processData: false,

    success: function (data) {

      // clean up current upload
      currentupload = undefined;

      if (typeof success_cb === "function") {
        success_cb(data);
      }

    },
  });
}

var upload = function (file, checksum, settings) {

  // build form data
  var fd = new FormData();
  fd.append("size", file.size);
  fd.append("checksum", checksum);
  fd.append("file", file);

  performUpload("upload/file", fd, settings);
}

var resumeUpload = function (blob, partial_size, partial_checksum, total_checksum, settings) {

  // build form data
  var fd = new FormData();
  fd.append("size", blob.size);
  fd.append("partial-size", partial_size);
  fd.append("checksum", total_checksum);
  fd.append("blob", blob);

  performUpload("upload/file/" + partial_checksum, fd, settings);
}

/*
 * Initialization
 *
 */

var findPartial = function (file) {
  return uploadlist.find(".partial-upload:not(.ready)").filter(function () {
    var up = $(this);
    return up.data("name") === file.name; // && up.data("size") == file.size;
  });
}

$(document).ready(function () {

  fileupload   = $(".fileupload");
  uploadlist   = $(".uploadlist");
  uploadbutton = $(".upload-all");
  filelist     = $(".filelist");

  // bind upload button (if any)
  uploadbutton.click(function () {
    startUploads();
  });

  // bind remove buttons (if any)
  uploadlist.find(".remove").click(function () {
    removeUploadElem($(this).parent());
  });

  // handle styling
  fileupload.on("dragenter dragover", function (e) {
    e.stopPropagation();
    e.preventDefault();
    $(this).addClass("dragging");
  });
  fileupload.on("dragleave drop", function (e) {
    e.stopPropagation();
    e.preventDefault();
    $(this).removeClass("dragging");
  });

  // Add files to upload on drop
  fileupload.on("drop", function (e) {
    e.stopPropagation();
    e.preventDefault();

    // add all files to upload list
    var total_files = all_files.length;
    var files = e.originalEvent.dataTransfer.files;
    for (var i = 0; i < files.length; i++) {

      var file = files[i];

      // keep track of all files
      all_files.push(file);
      var ix = total_files + i;

      // test if any of the partial files match current file size/name
      var partial = findPartial(file);
      if (partial.length > 0) {
        resumePartialUpload(partial.first(), file, ix);
      }
      else {
        newUploadElem(file, ix);
      }
    }
  });

  // Prevent drops outside of drag area
  $(document).on("dragenter dragover drop", function (e) {
    e.stopPropagation();
    e.preventDefault();
  });

});

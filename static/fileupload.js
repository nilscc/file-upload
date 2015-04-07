var fileupload, uploadlist, uploadspeed, filelist;
var placeholder;
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

/*
 * UI helper
 *
 */

// Remove upload list elements
var removeUploadElem = function (elem) {

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

  if (!uploadspeed || uploadspeed.length == 0) {
    uploadspeed = $("<span class=\"uploadspeed noselect\"></span>");
    $(".uploadlist button:last").after(uploadspeed);
  }

  uploadspeed.text(txt);
}

// Create new upload list entry
var newUploadElem = function (file, ix) {

  console.log(file);

  // "buttons" next to each file
  var filecontrols = "<i class=\"fa fa-times clickable remove\"></i>";

  // create HTML element
  var elem = $("<li class=\"new-upload noselect\" data-fileindex=\"" + ix + "\">"
              + filecontrols
              + "<span class=\"filename select\">" + file.name + "</span> "
              + "<span class=\"filesize\">" + readable(file.size) + "</span>"
              + "</li>");

  elem.find(".remove").click(function () {
    removeUploadElem(elem);
  });

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

  uploadlist.find("ul").append(elem);

  return elem;
}

// Start uploading all files in the current list
var startUploads = function () {

  // find first list entry
  var first = uploadlist.find("li:first");
  if (first.length === 0) {
    return;
  }

  // get first file
  var ix   = first.data("fileindex"),
      file = all_files[ix];

  // perform upload, set all callbacks
  upload(file, {

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

  });

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
 * Upload functions
 *
 */

var checksum = function (file, callback) {

  // calling this function really only makes sense with a callback
  if (typeof callback !== "function") { return; }

  var reader = new FileReader();

  reader.onload = function (e) {
    console.log(e);
    var md5 = CryptoJS.MD5(e.target.result).toString();
    callback(md5);
  };

  reader.readAsBinaryString(file);
}

var upload = function (file, settings) /* success_callback, speed_callback, progress_callback) */ {

  checksum(file, function (md5) {

    // build form data
    var fd = new FormData();
    fd.append("file", file);
    fd.append("size", file.size);
    fd.append("checksum", md5);

    var success_cb  = settings.success,
        speed_cb    = settings.speed,
        progress_cb = settings.progress;

    // start upload
    currentupload = $.ajax({

      // Add progress listener to XHR object
      xhr: function () {

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
              progress_cb(Math.ceil(pos / tot * 100));
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
      },

      // upload url & data
      type: "POST",
      url: "/upload/file",
      data: fd,
      contentType: false,
      processData: false,

      success: function (data) {

        // clean up current upload
        currentupload = undefined;

        if (typeof success_cb === "function") {
          success_cb(data);
        }
      }

    }); /* $.ajax */

  }); /* checksum */
}

/*
 * Initialization
 *
 */

$(document).ready(function () {

  fileupload = $(".fileupload");
  uploadlist = $(".uploadlist");
  filelist   = $(".filelist");

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
      newUploadElem(files[i], total_files+i);
      all_files.push(files[i]);
    }
  });

  // Prevent drops outside of drag area
  $(document).on("dragenter dragover drop", function (e) {
    e.stopPropagation();
    e.preventDefault();
  });

});

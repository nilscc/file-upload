// Perform ajax call to delete file on server
var deleteFile = function (file_id) {
  var url = "files/" + file_id;
  $.ajax(url, {
    method: "DELETE",
    success: function () {
      $("li:has(> .file[data-id='" + file_id + "'])").remove();
    },
  });
}

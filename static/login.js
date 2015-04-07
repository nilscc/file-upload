$(document).ready(function () {
  $(".login input[type='text']").focus();

  var input  = $(".login input[type='text']"),
      submit = $(".login input[type='submit']");

  input.bind("change paste keyup", function () {
    if ($(this).val()) {
      submit
        .css("opacity", "1")
        .css("cursor", "pointer")
        .prop("disabled", false);
    }
    else {
      submit
        .css("opacity", "0")
        .css("cursor", "default")
        .prop("disabled", true);
    }
  });
});

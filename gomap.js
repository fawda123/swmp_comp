// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("Latitude");
  var long = $el.data("Longitude");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: Latitude,
    lng: Longitude,
    nonce: Math.random()
  });
});
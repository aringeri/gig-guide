var tileUrl = 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png';
var mymap = L.map('mapid').setView([-37.813611,144.963056], 13);

L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token={accessToken}', {
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    maxZoom: 18,
    id: 'mapbox.streets',
    accessToken: 'pk.eyJ1IjoiYXJpbmciLCJhIjoiY2p6N3prbmVwMGlvdjNjbWhlMzJtb240dCJ9.UzSWkQCDedp7y9LXp7BY4g'
}).addTo(mymap);

var bounds = mymap.getBounds();
var reloadBtn;

L.Control.Reload = L.Control.extend({
    onAdd: function(map) {
        var btn = L.DomUtil.create('button', 'leaflet-control button');
        btn.innerHTML = '<i class=\"fa fa-search\"></i>Search this area';
        btn.onclick = reload;
        return btn;
    },

    onRemove: function(map) {
        // Nothing to do here
    }
});

var global_geo_layer = new L.geoJson();
global_geo_layer.addTo(mymap);

function reload() {
  var b = mymap.getBounds();
  let d = document.getElementById("date").value;

  $.ajax({
    dataType: "json",
    url: "search?north="  + b.getNorth()
              + "&east="  + b.getEast()
              + "&south=" + b.getSouth()
              + "&west="  + b.getWest()
              + "&date="  + d
    ,
    success: function(data) {
        // remove old points
        function onEachFeature(feature, layer) {
            if (feature.properties && feature.properties.vName) {
              let markerClicked = function() {
                set_sidebar_data(feature);
                w3_open();
              }
              layer.on("click", markerClicked)
            }
        }
        var geoLayer = new L.geoJson(null, {
          onEachFeature : onEachFeature
        });
        geoLayer.addTo(mymap);
        

        $(data.features).each(function(key, data) {
            geoLayer.addData(data);
        });
        global_geo_layer.remove();
        global_geo_layer = geoLayer;
        loading_feedback_off();
    }
  }).error(function() {
    loading_feedback_off();
  });

  if (reloadBtn != null) {
    reloadBtn.remove();
    reloadBtn = null;
  }
}

function set_sidebar_data(feature) {
  venue_div = document.getElementById("venue-holder");
  let html = 
    `<label for"venue">Venue:</label>
    <span id="venue">${feature.properties.vName}</span>`;

  for (var e in feature.properties.events) {
    var ev = feature.properties.events[e]
    html += 
    `<div>
        <h3 id="event_name${e}">${ev.eName}</h3>
        <label for"price${e}">Price:</label>
        <span id="event_price${e}">${ev.price}</span>`;

    if (ev.categories.length > 0) {
      var cats = ""
      for (var c in ev.categories) {
        cats += ev.categories[c] + " ";
      }
      html += 
    `    <br><label>Category:<label>
         <span>${cats}</span>`; 
    }
    
    html += 
    `    <hr>
     </div>`;
  }
  venue_div.innerHTML = html
}

L.control.reload = function(opts) {
    return new L.Control.Reload(opts);
}

function boundsMoved(e) {
  if (reloadBtn == null) {
    reloadBtn = L.control.reload({ position: 'topright' });
    reloadBtn.addTo(mymap);
  }
}
reload();
mymap.on("moveend", boundsMoved)

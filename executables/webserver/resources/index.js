var tileUrl = 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png';
var mymap = L.map('mapid').setView([-37.813611,144.963056], 13);

L.tileLayer('https://api.mapbox.com/styles/v1/mapbox/streets-v10/tiles/{z}/{x}/{y}?access_token={accessToken}', {
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    maxZoom: 18,
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
  if (!d || d === "") {
    d = fmtDate(todaysDate());
  }

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
    var ev = feature.properties.events[e];
    html += 
      `<div>
          <h3 id="event_name${e}">${ev.eName}`
    if (ev.ticketUrl) {
      html += 
        `   <a target="_blank" rel="noopener noreferrer" href="${ev.ticketUrl}">
              <i class="fa fa-ticket" aria-hidden="true"></i>
            </a>`;
    }
    html += `
        </h3>
        
        <label for"event_time${e}" class="gig-label">Time:</label>
        <span id="event_time${e}" >${ev.time}</span><br>
        <label for"event_price${e}" class="gig-label">Price:</label>
        <span id="event_price${e}">${ev.price}</span>`;
    
    if (ev.genre) {
      tagColour = genreColour(ev.genre);
      html += 
        `    <br><label for"event_genre" class="gig-label">Genre:</label>
             <span id="event_genre" class="w3-tag ${tagColour}">${ev.genre}</span>`;
    }

    html += arrToHTMl(ev.supports, "Supports:", ", ");
    
    html += 
      `    <hr>
       </div>`;
  }
  venue_div.innerHTML = html
}

function genreColour(genre) {
  switch (genre) {
    case "Rock":
    case "Indie":
      return "w3-red";
    case "Global":
    case "World Music":
      return "w3-cyan";
    case "Jazz":
    case "Soul/Funk":
    case "Experimental":
      return "w3-purple";
    case "Blues":
      return "w3-indigo";
    case "Electronic":
      return "w3-light-green";
    case "R&B":
    case "Hip Hop":
      return "w3-deep-purple";
    case "Classical":
      return "w3-amber";
    case "Punk":
    case "Metal":
      return "w3-dark-grey";
    case "Pop":
      return "w3-pale-red";
    case "Acoustic":
    case "Country/Folk":
      return "w3-khaki";
    default:
      return "";
  }
}

function arrToHTMl(arr, label, sep=" ") {
  if (arr.length > 0) {
    let h = 
      `    <br><label class="gig-label">${label}</label>
           <span class="multi-line-content">${arr.join(sep)}</span>`; 
    return h;
  }
  return "";
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

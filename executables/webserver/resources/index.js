var tileUrl = 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png';
var mymap = L.map('mapid', {
  zoomControl: false
}).setView([-37.813611,144.963056], 13);

L.tileLayer('https://api.mapbox.com/styles/v1/mapbox/streets-v10/tiles/{z}/{x}/{y}?access_token={accessToken}', {
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    maxZoom: 18,
    accessToken: 'pk.eyJ1IjoiYXJpbmciLCJhIjoiY2p6N3prbmVwMGlvdjNjbWhlMzJtb240dCJ9.UzSWkQCDedp7y9LXp7BY4g'
}).addTo(mymap);

var bounds = mymap.getBounds();
var reloadBtn;

L.control.zoom({
  position: "topright"
}).addTo(mymap);
L.Control.Reload = L.Control.extend({
    onAdd: function(map) {
        var btn = L.DomUtil.create('button', 'leaflet-control button');
        btn.innerHTML = '<i class=\"fa fa-search\"></i>Search this area';
        btn.onclick = reload;
        L.DomEvent.disableClickPropagation(btn);
        return btn;
    },

    onRemove: function(map) {
        // Nothing to do here
    }
});

mymap.on("dblclick", toggleFullscreen);

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
              layer.on("click", function() {
                 set_sidebar_data(feature);
                 w3_open();
              });
              layer.bindPopup(feature.properties.vName);
              layer.on('mouseover', function (e) {
                  this.openPopup();
              });
              layer.on('mouseout', function (e) {
                  this.closePopup();
              });
            }
        }
        var geoLayer = new L.geoJson(null, {
          pointToLayer: function(feature, latlng) {
              var iconPath = "icon/" + genreIcon(feature);
              var smallIcon = new L.Icon({
                   iconSize: [33, 33],
                   iconAnchor: [17, 33],
                   popupAnchor:  [0, -25],
                   shadowSize:   [35, 30],
                   shadowAnchor: [10, 30],
                   iconUrl: iconPath,
                   shadowUrl: 'https://unpkg.com/leaflet@1.4.0/dist/images/marker-shadow.png'
              });
             return L.marker(latlng, {icon: smallIcon});
             },
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

function findFirstGenre(feature) {
  var events = feature.properties.events;
  for (var i in events) {
    var e = events[i];
    if (e.genre) {
      return e.genre;
    }
  }
  return "";
}

function genreIcon(feature) {
  var genre = findFirstGenre(feature);
  switch (genre) {
    case "Rock":
    case "Indie":
      return "icon-red.png";
    case "Global":
    case "World Music":
      return "icon-cyan.png";
    case "Jazz":
    case "Soul/Funk":
    case "Experimental":
    case "R&B":
    case "Hip Hop":
      return "icon-purple.png";
    case "Blues":
      return "icon-blue.png";
    case "Electronic":
      return "icon-lime.png";
    case "Classical":
      return "icon-amber.png";
    case "Punk":
    case "Metal":
      return "icon-black.png";
    case "Pop":
      return "icon-pale-red.png";
    case "Acoustic":
    case "Country/Folk":
      return "icon-orange.png";
    default:
      return "icon-green.png";
  }
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
    reloadBtn = L.control.reload({ position: 'topleft' });
    reloadBtn.addTo(mymap);
  }
}
reload();
mymap.on("moveend", boundsMoved)

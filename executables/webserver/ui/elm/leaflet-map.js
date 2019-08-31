customElements.define('custom-map', class extends HTMLElement {
  constructor() {
    super();
    this._geoJson = { features : [] };
    this._view = { coords : [37.813611,144.963056], zoom: 13 }
    this._tileProperties =
      { url : 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        options : {
          attribution : 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>',
          maxZoom: 18
        }
      }
    this._markerMap = new Map();
  }

  set tileProperties(value) {
    this._tileProperties = value;
  }

  get bounds() {
    var ne = this._bounds.getNorthEast();
    var sw = this._bounds.getSouthWest();
    return {
      northEast : {lat : ne.lat, lon : ne.lng },
      southWest : {lat : sw.lat, lon : sw.lng }
    };
  }

  get geoJson() {
    return this._geoJson;
  }

  set geoJson(value) {
    if (this._geoJson == value) return;
    this._geoJson = value;
    if (!this._map) return;
    this.replaceGeoJson();
  }

  replaceGeoJson() {
    if (this._geoLayer) {
      this._geoLayer.remove();
    }
    this._geoLayer = this.createGeoLayer();
    this._geoLayer.addTo(this._map);

    for (var i in this._geoJson.features) {
      var f = this._geoJson.features[i];
      this._geoLayer.addData(f);
    }
  }

  createGeoLayer() {
    var custom_this = this;
    var geoLayer = new L.geoJson(null, {
      pointToLayer : (feature, latlng) => {
        return custom_this.featureMarker(feature, latlng)
      },
      onEachFeature : (feature, layer) => {
        if (feature.properties && feature.properties.vName) {
          layer.on('click', () => {
            custom_this._selected = feature;
            custom_this.dispatchEvent(new CustomEvent('markerClicked'));
          });

          const popup = L.popup({autoPan : false});
          popup.setContent(feature.properties.vName);

          layer.bindPopup(popup);
          layer.on('mouseover', function (e) {
              this.openPopup();
          });
          layer.on('mouseout', function (e) {
              this.closePopup();
          });
        }
      }
    });
    return geoLayer;
  }

  featureMarker(feature, latlng) {
    const icon = new L.icon(this.getIconProps(feature))
    return L.marker(latlng, {icon: icon});
  }

  getIconProps(feature) {
    const genre = this.findFirstGenre(feature);
    if (genre) {
      const marker = this._markerMap.get(genre);
      if (marker) {
        return marker;
      }
    }
    return this._defaultIconProps;
  }

  set defaultIconProps(value) {
    this._defaultIconProps = value;
  }

  findFirstGenre(feature) {
    const events = feature.properties.events;
    for (var i in events) {
      const e = events[i];
      if (e.genre) {
        return e.genre;
      }
    }
    return null;
  }

  set markerMap(value) {
    this._markerMap = new Map(value);
  }

  set view(value) {
    this._view = value;
  }

  set zoomPosition(value) {
    this._zoomControlPosition = value;
  }

  connectedCallback() {
    if (this._zoomControlPosition) {
      this._map = L.map(this, {zoomControl : false});
      L.control.zoom({
        position: this._zoomControlPosition
      }).addTo(this._map);
    } else {
      this._map = L.map(this);
    }
    this._map.setView(this._view.coords, this._view.zoom);
    this._bounds = this._map.getBounds();

    L.tileLayer(this._tileProperties.url, this._tileProperties.options)
        .addTo(this._map);

    this.replaceGeoJson();

    this._map.on('moveend', e => {
      this._bounds = this._map.getBounds();
      this.dispatchEvent(new CustomEvent('boundsChanged'));
    })
  }
})
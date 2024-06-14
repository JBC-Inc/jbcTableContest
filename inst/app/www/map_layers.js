  function(el, x, data) {

    var m = this;

    var basemaps = {

      CartoDB_Positron:

      L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png', {maxZoom: 19, minZoom: 6}),

      Streets:

      L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {maxZoom: 19, minZoom: 6}),

      NatGeo:

      L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}', { maxZoom: 16, minZoom: 6 })

      // Stadia_AlidadeSmoothDark:

      // L.tileLayer('https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png', { maxZoom: 14, minZoom: 10 })
    };

    var groupedOverlays = {
      "Map Type": {
        "Cartography Database": basemaps.CartoDB_Positron,
        "Open Street Map": basemaps.Streets,
        "National Geographic": basemaps.NatGeo
        //"Stadia Alidade": basemaps.Stadia_AlidadeSmoothDark
      }
    };

    var options = {
      groupCheckboxes: false,
      exclusiveGroups: ["Map Type"],
      collapsed: true,
      position: "bottomleft",
    };

    L.control.groupedLayers(null, groupedOverlays, options).addTo(m);
    basemaps.CartoDB_Positron.addTo(m);  // Default Map Selected
  }

  function(el, x, data) {
    
    var m = this;
    var button = document.getElementById('choices-render');
    var elem = document.getElementById('leaflet-busy');
    
    // initial map render  ==================================
    
    m.whenReady(function() {
      elem.style.display = 'flex';
      setTimeout(function() {
        elem.style.display = 'none';
        
      }, 1000)
    });
    
    // When render button pressed ===========================
          
    button.addEventListener('click', function(event) {
      elem.style.display = 'flex';
      (new Promise(function (resolve, reject) {
        m.addEventListener('layeradd', function (event) {
          console.log(event.type)
          setTimeout(function () {
            resolve('done');
            
          }, 500)
          
        })
      })).then(function (response) {
        console.log('done');
        elem.style.display = 'none';
        
      }).catch(function (error) {
        console.error(error)
        
      })
      
    });
    
  }

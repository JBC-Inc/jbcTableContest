$(document).on('click', 'li', function() {
  window.dispatchEvent(new Event("resize"));
  });

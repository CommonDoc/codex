window.MathJax = {
  chtml: {
    fontURL: 'static/'
  },
};

(function () {
  var script = document.createElement('script');
  script.src = 'static/tex-chtml.js';
  script.async = true;
  document.head.appendChild(script);
})();

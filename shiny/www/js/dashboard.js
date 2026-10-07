// File: shiny/www/js/dashboard.js
 $(function () {
  $('[data-toggle="tooltip"]').tooltip();
  console.log("[rpredictmaint] dashboard loaded " + new Date().toISOString());
  setInterval(function () {
    console.log("[rpredictmaint] heartbeat " + new Date().toISOString());
  }, 60000);
});
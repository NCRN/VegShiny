library(shiny)
library(NPSForVeg)
library(leaflet)
library(shinyjs)
library(DT)
library(bslib)
library(htmltools)


shiny::navbarPage(
  title=htmltools::HTML("National&nbsp;Capital&nbsp;Region&nbsp;Network<wbr> Forest&nbsp;Vegetation"),
  position = "static-top", inverse=TRUE, collapsible = TRUE, fluid=TRUE, windowTitle = base::paste(NETWORK, "Forest Vegetation"),
  theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", id="MainNavBar",
  
  htmltools::tags$head(shiny::includeCSS("./www/mapstyles.css"),
                       shiny::includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"),
                       htmltools::tags$style(
                         htmltools::HTML("
      .action-button {white-space: normal !important; word-wrap: break-word !important;}

      .main-full-width {width: 100% !important;}
      
      .navbar-inverse .navbar-brand {
        font-size: calc(1.5rem + 1.5vw);      
        font-family: 'Times New Roman';
        color: #fff !important;                    
        line-height: 1.0;
        padding-left: 1rem;
        margin: 0;
        pointer-events:none;}

      .navbar {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 2000 !important;
        min-height: auto !important;
        padding-top: 1.5rem;
        padding-bottom: 1.5rem;}
        
      .navbar-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        width: 100%;
        float: none;             
        gap: clamp(0.5rem, 1vw, 1rem);}

      .navbar-header .navbar-brand {
        order: 1;                     
        text-align: left;           
        margin: 0;            
        display: flex;
        align-items: center;}

      .navbar-header .navbar-toggle {
        order: 2;                     
        align-items: center;
        margin-left: auto;            
        margin-right: clamp(0.5rem, 2vw, 2rem);  
        display: inline-flex !important;
        flex-direction: column;           
        justify-content: center;          
        margin-top: 0 !important;         
        margin-bottom: 0 !important;      
        padding: 0 !important;}

      .navbar-collapse {
        position: absolute !important;
        top: 100% !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 1999 !important;
        background-color: #000 !important;
        padding: 0.5rem 1.5rem !important;
        overflow: hidden !important;}

      .navbar-collapse.collapse {display: none !important;}
      .navbar-collapse.in {display: block !important;}
      body {padding-top: 80px;}

      .navbar-nav {
        display: flex;   
        float: none !important; 
        align-items: left;
        gap: 0.75rem;                  
        flex-direction: column;
        margin: 0;
        margin-top: 0 !important;}
        
      .navbar-nav > li:has(> a:empty) {
        display: none !important;}

      .navbar-toggle {border: none !important;}
      .navbar-toggle:hover,
      .navbar-toggle:active,
      .navbar-toggle:focus {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        outline: none !important;}

      .navbar-toggle > span:not(.icon-bar) { display: none !important; }

      .navbar-toggle::before,
      .navbar-toggle::after { content: none !important; display: none !important; }

      .navbar-toggle .icon-bar {
        display: block !important;
        width: 1.6em;
        height: 0.10em;
        background-color: #fff !important;}
      .navbar-toggle .icon-bar + .icon-bar { margin-top: 0.4em; }

      .navbar-nav .dropdown-menu {
        position: static !important;   
        float: none !important;
        margin-top: 0 !important;
        display: none;                 
        border: none;                  
        box-shadow: none;             
        background: #000;              
        margin-left: 1.5rem;}
        
      .navbar-nav .dropdown.open > .dropdown-menu {display: block;}

      .navbar-nav > li > a,
      .navbar-nav .dropdown-menu > li > a{
        font-family: 'Times New Roman' !important;
        font-size: calc(.75em + .75vw) !important;
        color: #fff !important;}

      .navbar-nav > li > a:empty {
        display: none !important;
        visibility: hidden !important;
        height: 0 !important;
        padding: 0 !important;
        margin: 0 !important;}

      .navbar.navbar-inverse .navbar-nav li.dropdown.open a.dropdown-toggle .caret {
        transform: scaleY(-1) translateX(-2px);
        align-self: center !important;
        margin-left: 0.35rem;}
      .navbar.navbar-inverse .navbar-nav > li.dropdown > a.dropdown-toggle {
        display: flex !important;
        align-items: center !important;
        width: 100% !important;
        padding: 0.75rem 1.25rem !important;}

      .navbar-inverse .navbar-nav > li > a {padding: 0.75rem 1.25rem;}
      .navbar-inverse .navbar-nav > li > a:hover,
      .navbar-nav .dropdown-menu > li > a:hover {
        color: #fff !important;
        background-color: #916800 !important;}

      .navbar-nav .dropdown-menu > li > a:focus,
      .navbar-nav .dropdown-menu > li > a:active {
        outline: 0 !important;}
      .navbar-nav .dropdown-menu > .active > a,
      .navbar-nav .dropdown-menu > .active > a:hover,
      .navbar-nav .dropdown-menu > .active > a:focus {
        background-color: transparent;
        color: #fff !important;}

      .sidebar-col-outer {
        position: relative;
        float: left;
        box-sizing: border-box;
        transition: width 0.3s ease;
        overflow: visible;}
      .sidebar-col-outer.collapsed {
        width: 24px !important;
        padding-left: 0 !important;
        padding-right: 0 !important;}
      .sidebar-slide-wrap {
        position: relative;
        width: 100%;
        transition: margin-left 0.3s ease;}
      .sidebar-slide-wrap.collapsed {margin-left: -2000px;}
      .sidebar-slide-wrap .sidebar-content {transition: opacity 0.15s ease; opacity: 1;}
      .sidebar-slide-wrap.collapsed .sidebar-content {opacity: 0;}

      .sidebar-toggle-tab {
        position: absolute;
        top: 50%;
        transform: translateY(-50%);
        right: -5px;
        width: 20px;
        height: 72px;
        background-color: #ffffff;
        color: #555;
        border: none;
        border-radius: 0 8px 8px 0;
        cursor: pointer;
        z-index: 20;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 16px;
        box-shadow: 1px 0 4px rgba(0, 0, 0, 0.2);
        transition: background-color 0.2s ease, color 0.2s ease;}

      .sidebar-toggle-tab:hover {
        background-color: #f5f5f5;
        color: #222;}

      .collapsible-main-panel {
        float: left;
        box-sizing: border-box;
        transition: none;
        padding-left: 10px !important;}

      .main-full-width {width: calc(100% - 24px) !important;}

      #MainNavBar .tab-content > .tab-pane[data-value=\"Map\"] > .row {margin: 0 !important;}
      #mapMain,
      #mapSidebar {padding-bottom: 0 !important; margin-bottom: 0 !important;}

      .selectize-control .selectize-input::after {
        margin-top: 0 !important;
        margin-right: 0 !important;}

      .selectize-control .selectize-input {
        position: relative;}
      .selectize-control .selectize-input::after {
        position: absolute;
        right: 1rem !important;
        transform: translateY(-50%);}
        
      @media (max-width: 630px) {
        #AboutMapPanel {
          top: 85px;
          right: 30px;
          width: 250px !important;
          height: 500px !important}
        #AboutMapPanel .panel-body {height: 375px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 85px;
          right: 3px;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}

        #IVOptionsPanel{
          top: 85px;
          right: 15px;
          width: 240px !important;
          height: 575px !important}
        #IVOptionsPanel .panel-body {height: 450px !important; overflow-y: auto;}}

      @media (min-width: 631px) and (max-width: 767px) {
        #AboutMapPanel {
          top: 85px;
          right: 30px;
          width: 300px !important;
          height: 500px !important;}
        #AboutMapPanel .panel-body {height: 375px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 85px;
          right: 30px;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 85px;
          right: 30px;
          width: 500px !important;
          height: 450px !important}
        #IVOptionsPanel .panel-body {height: 325px !important; overflow-y: auto;}}
        
      @media (min-width: 768px) and (max-width: 1000px) {
        #AboutMapPanel {
          top: 85px;
          left: 25vw;
          width: 450px !important;
          height: 675px !important;}
        #AboutMapPanel .panel-body {height: 550px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 150px;
          left: 27vw;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 150px;
          left: 27vw;
          width: 550px !important;
          height: 450px !important}
        #IVOptionsPanel .panel-body {height: 325px !important; overflow-y: auto;}}
      
      @media (min-width: 1001px) and (max-width: 1597px) {
        #AboutMapPanel {
          top: 85px;
          left: 22vw;
          width: 450px !important;
          height: 450px !important;}
        #AboutMapPanel .panel-body {height: 327px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 150px;
          left: 26.25vw;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 227px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 140px;
          left: 26.25vw;
          width: 725px !important;
          height: 345px !important}
        #IVOptionsPanel .panel-body {height: 223px !important; overflow-y: none;}}
      
      @media (min-width: 1598px) {
        #AboutMapPanel {
          top: 85px;
          left: 20vw;
          width: 600px !important;
          height: 450px !important;}
        #AboutMapPanel .panel-body {height: 327px !important; overflow-y: scroll;}
        #GraphOptionsPanel {
          top: 150px;
          left: 25.75vw;
          width: 375px !important;
          height: 350px !important}
        #GraphOptionsPanel .panel-body {height: 225px !important; overflow-y: auto;}
        #IVOptionsPanel{
          top: 140px;
          left: 25.75vw;
          width: 725px !important;
          height: 345px !important}
        #IVOptionsPanel .panel-body {height: 223px !important; overflow-y: none;}}
        
      @media (max-width: 1024px) {

      .sidebar-col-outer {
         width: 100% !important;
        float: none !important;
        overflow: visible !important;}

      .sidebar-col-outer.collapsed {
        width: 100% !important;
        height: auto !important;
        padding: 0 !important;
        min-height: 20px !important;
        height: 20px !important;}

      .collapsible-main-panel {
        width: 100% !important;
        float: none !important;
        padding-left: 10px !important;
        padding-top: 5px !important;}
  
      #mapMain {padding-left: 0 !important;}

      .sidebar-slide-wrap {
        margin-left: 0 !important;
        max-height: 2000px;
        overflow: hidden;
        transition: max-height 0.35s ease !important;}

      .sidebar-slide-wrap.collapsed {
        margin-left: 0 !important;
        max-height: 0 !important;}

      .sidebar-slide-wrap .sidebar-content,
      .sidebar-slide-wrap.collapsed .sidebar-content {
        opacity: 1 !important;}

      .sidebar-toggle-tab {
        position: absolute !important;
        top: auto !important;
        bottom: -2px !important;
        left: 50% !important;
        right: auto !important;
        transform: translateX(-50%) !important;
        width: 72px !important;
        height: 22px !important;
        border-radius: 0 0 8px 8px !important;
        box-shadow: 0 3px 5px rgba(0, 0, 0, 0.2) !important;
        z-index: 100 !important;}

      .sidebar-toggle-tab span {
        display: inline-block;
        transform: rotate(-270deg);}
  
      .report-body.open {
        max-height: 60vh !important;
      overflow-y: auto !important;}
}
        
        //#SpeciesControls .panel.panel-default,
        //#SpeciesControls .well.panel.panel-default,
        //#densDataPanel .panel.panel-default,
        //#densDataPanel .well.panel.panel-default,
        //#IVDataPanel .panel.panel-default,
        //#IVDataPanel .well.panel.panel-default {
          //border-color: #7c8f4f !important;}
        
        #densSlideWrap .panel.panel-default,
        #tsSlideWrap .panel.panel-default,
        #ivSlideWrap .panel.panel-default,
        #mapSlideWrap .panel.panel-default,
        #spSlideWrap .panel.panel-default {
          border-color: #7c8f4f !important;}
        #MapControlPanel.panel.panel-default,
        #ZoomPanel.panel.panel-default,
        #ExtraLayerPanel.panel.panel-default {
          border-radius: 4px !important;}
          
      .info-icon-btn {
        position: absolute;
        top: 8px;
        right: 8px;
        width: 28px;
        height: 28px;
        border-radius: 50%;
        background-color: #f0f0f0;
        color: #555;
        border: 1px solid #ddd;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 16px;
        font-weight: bold;
        font-style: italic;
        cursor: pointer;
        z-index: 30;
        transition: background-color 0.2s ease, color 0.2s ease;}
      .info-icon-btn:hover {
        background-color: #e0e0e0;
        color: #222;}
      .info-popup-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.4);
        z-index: 9990;
        display: none;}
      .info-popup-overlay.active {display: block;}
      .info-popup-box {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 90%;
        max-width: 700px;
        max-height: 80vh;
        overflow-y: auto;
        background-color: #ffffff;
        border-radius: 8px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
        z-index: 9991;
        padding: 24px;
        display: none;}
      .info-popup-box.active {display: block;}
      .info-popup-close {
        position: absolute;
        top: 12px;
        right: 16px;
        font-size: 24px;
        cursor: pointer;
        color: #888;
        border: none;
        background: none;}
      .info-popup-close:hover {color: #333;}

      .selectize-dropdown {z-index: 9000 !important;}

      .map-info-leaflet-control a {
        width: 30px !important;
        height: 30px !important;
        line-height: 30px !important;
        border-radius: 50%;
        background-color: #ffffff;
        color: #555;
        text-decoration: none;
        font-style: italic;
        outline: none;}
      .map-info-leaflet-control a:focus,
      .map-info-leaflet-control a:active {
        outline: none;
        box-shadow: none;}
      .map-info-leaflet-control {
        border-radius: 50% !important;
        overflow: hidden;}
      .map-info-leaflet-control a:hover {
        background-color: #f4f4f4;
        color: #222;}

      .report-body {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.3s ease;}
      .report-body.open {
        max-height: 2000px;
        transition: max-height 0.5s ease;}
        
      .report-caret {
        display: inline-block;
        transition: transform 0.3s ease;
        transform: rotate(-90deg);}
      .report-caret.open {transform: rotate(0deg);}

  ")),
                       
htmltools::tags$script(htmltools::HTML("
$(document).on('click', '.navbar-collapse.in a:not(.dropdown-toggle)', function () {
$(this).closest('.navbar-collapse').collapse('hide');
});

$(document).on('click', '.navbar-collapse .dropdown-menu a', function () {
$(this).closest('.navbar-collapse').collapse('hide');
});
    
$(function(){
$('.navbar-nav li.dropdown').each(function(){
var $a = $(this).find('> a.dropdown-toggle');
var $t = $a.find('[title]');
var title = $t.attr('title');
if (title) {
$(this).attr('title', title); 
$t.attr('title','');}});
});
    
$(document).on('show.bs.collapse', function(e) {
if ($(e.target).hasClass('navbar-collapse')) {
$('#navbar-overlay').fadeIn(200);}
});

$(document).on('hide.bs.collapse', function(e) {
if ($(e.target).hasClass('navbar-collapse')) {
$('#navbar-overlay').fadeOut(200);}
});

$(document).on('click', '#navbar-overlay', function() {
$('.navbar-collapse.in').collapse('hide');
$(this).fadeOut(200);
});

(function () {function getSelectize(el) { return el && el.selectize ? el.selectize : null; }

$(document).on('shiny:bound', function (ev) {
var $el = $(ev.target);
if (!$el.is('select') || !$el.hasClass('selectized')) return;
var sel = getSelectize($el[0]);
if (!sel) return;
sel.settings.openOnFocus = false;
var $control = $el.next('.selectize-control').find('.selectize-input');

//close menu upon selecting toggle
$control.on('pointerdown.selectizeToggle', function (e) {
if ($(e.target).is('input, textarea')) return;
e.preventDefault();
e.stopImmediatePropagation();
if (sel.isOpen) {sel.close(); sel.blur();} 
else {sel.open();sel.focus();}
});

//close menu upon item selection
sel.on('item_select', function () { sel.close(); sel.blur(); });
sel.on('dropdown_close', function () { sel.blur(); });
    
sel.on('dropdown_open', function() {
$el.closest('.sidebar-col-outer').css('overflow', 'visible');
$el.closest('.sidebar-slide-wrap').css('overflow', 'visible');
$el.closest('.well').css('overflow', 'visible');
});

sel.on('dropdown_close', function() {
$el.closest('.sidebar-col-outer').css('overflow', '');
$el.closest('.sidebar-slide-wrap').css('overflow', '');
$el.closest('.well').css('overflow', '');});
});

//close menu when clicking anywhere outside menu 
$(document).on('pointerdown.selectizeOutside', function (e) {
var $t = $(e.target);
if ($t.closest('.selectize-control').length) return;
$('.selectized').each(function () {
var sel = getSelectize(this);
if (sel && sel.isOpen) sel.close();});});
})();
    
$(document).on('click', '.report-header', function() {
var $body = $(this).next('.report-body');
var $caret = $(this).find('.report-caret');
$body.toggleClass('open');
caret.toggleClass('open');
});
    
  ")),

htmltools::tags$script(htmltools::HTML("
['dens', 'ts', 'iv', 'map', 'sp'].forEach(function(panelId) {
  $(document).on('click', '#toggle_' + panelId, function() {
    var sidebarOuterId = (panelId === 'sp') ? 'SpeciesControls' : (panelId + 'Sidebar');
    var wrapId = (panelId === 'sp') ? 'spSlideWrap' : (panelId + 'SlideWrap');

    var $sidebarOuter = $('#' + sidebarOuterId);
    var $wrap = $('#' + wrapId);
    var $main = $('#' + panelId + 'Main');
    var $toggleTab = $('#toggle_' + panelId);
    var $chevron = $toggleTab.find('span');
    var isMobile = $(window).width() <= 767;

    if ($wrap.hasClass('collapsed')) {
      $wrap.removeClass('collapsed');
      $sidebarOuter.removeClass('collapsed');
      if (!isMobile) $main.removeClass('main-full-width');
      $chevron.text('\u00ab');
      $toggleTab.attr('title', 'Close data control panel');
      if (!isMobile) {
        setTimeout(function() { $sidebarOuter.css('height', ''); }, 320);
      } else {
        $sidebarOuter.css('height', '');
      }
    } else {
      if (!isMobile) {
        var lockedHeight = $sidebarOuter.outerHeight();
        $sidebarOuter.css('height', lockedHeight + 'px');
      }
      $wrap.addClass('collapsed');
      $sidebarOuter.addClass('collapsed');
      $chevron.text('\u00bb');
      $toggleTab.attr('title', 'Open data control panel');
      if (!isMobile) {
        setTimeout(function() { $main.addClass('main-full-width'); }, 320);
      }
    }

    setTimeout(function() {
      $('.plotly').each(function() {
        if (typeof Plotly !== 'undefined') Plotly.Plots.resize(this);
      });
      $main.find('table.dataTable').each(function() {
        if ($.fn.dataTable && $.fn.dataTable.isDataTable(this)) {
          $(this).DataTable().columns.adjust();
        }
      });
      $(window).trigger('resize');
    }, 320);
  });
});

$(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
  var $parentUl = $(this).closest('ul');
  if ($parentUl.hasClass('navbar-nav')) {
    var isMobile = $(window).width() <= 767;
    $('.sidebar-slide-wrap.collapsed').removeClass('collapsed');
    $('.sidebar-col-outer.collapsed').removeClass('collapsed');
    if (!isMobile) $('.main-full-width').removeClass('main-full-width');
    $('.sidebar-toggle-tab span').text('\u00ab');
    $('.sidebar-toggle-tab').attr('title', 'Close data control panel');
    setTimeout(function() { $('.sidebar-col-outer').css('height', ''); }, 320);
  }
});
")),

htmltools::tags$script(htmltools::HTML("
$(document).on('click', '.info-icon-btn', function() {
  var targetId = $(this).data('target');
  $('#' + targetId + 'Overlay').addClass('active');
  $('#' + targetId + 'Box').addClass('active');
});

$(document).on('click', '.info-popup-close, .info-popup-overlay', function() {
  $('.info-popup-overlay').removeClass('active');
  $('.info-popup-box').removeClass('active');
});

$(document).on('click', '.info-popup-box', function(e) {
  e.stopPropagation();
});
")),

htmltools::tags$script(htmltools::HTML("
$(document).ready(function() {
  $('body').append('<div id=\"navbar-overlay\" style=\"display:none; position:fixed; top:0; left:0; width:100%; height:100%; background:rgba(0,0,0,0.35); z-index:1998;\"></div>');
});
")),
                       
htmltools::includeHTML("www/google-analytics.html")
  ),
  
  ######################################### Map Panel ####################################################################
  
  shiny::tabPanel(htmltools::tags$div(title="Map the data", "Map"), value = "Map", style="padding: 0",
                  shinyjs::useShinyjs(),
                  
                  #### Side Control Panel ####
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      id = "mapSidebar",
                      class = "sidebar-col-outer",
                      htmltools::tags$div(
                        id = "mapSlideWrap",
                        class = "sidebar-slide-wrap",
                        #### Map Controls ####
                        htmltools::div(id="MapControlPanel", class="panel panel-default well controls",
                                       shiny::h4("Map Controls", class="panel-heading"),
                                       htmltools::tags$div(title = "Showing every monitoring plot in the network. Change the inputs below to view the filtered data",
                                                           shiny::uiOutput("mapModeIndicator")),
                                       shiny::hr(),
                                       htmltools::tags$div(title="Filter by park so only species in a given park are listed",
                                                           shiny::uiOutput("MapParkControl")),
                                       htmltools::tags$div(title="Select the time period you want to work with", shiny::uiOutput("MapCycleControl")),
                                       htmltools::tags$div(title="Select the type of plant you want to work with", shiny::selectizeInput(inputId="MapGroup", 
                                                                                                                                         label="Type of plant:", choices=PLANTTYPES, selected = "", options = base::list(placeholder = "Select a plant type",
                                                                                                                                                                                                                         onInitialize = base::I('function() { this.setValue(""); }'))),
                                                           shiny::uiOutput("incompleteInputWarning")),
                                       htmltools::tags$div(title="Select a species of plants to map", shiny::uiOutput("MapSpeciesControl")),  
                                       htmltools::tags$div(title="Toggle between common and scientific names",
                                                           shiny::checkboxInput(inputId="mapCommon", label="Display common names?", value=TRUE )),
                                       htmltools::tags$div(title="Select live or dead", shiny::selectizeInput(inputId="TreeStatus", label="Alive or dead",
                                                                                                              choices=base::c("Alive"='alive',"Dead" = 'snag',"All"='all'), selected = NULL, 
                                                                                                              options = base::list(placeholder = "Select a tree status",
                                                                                                                                   onInitialize = base::I('function() { this.setValue(""); }')))),
                                       htmltools::tags$div(title="Select the type of data to map",shiny::uiOutput("PlantValueControl")),
                                       #htmltools::tags$div(title="Select the four year period you want to work with.", shiny::sliderInput(inputId="MapYear", 
                                       #     label="Display data from the 4 years ending:", min=YEARS$Start+YEARS$Range-1, max=YEARS$End, value=YEARS$End,
                                       #    sep="", step=1,ticks=T)),
                        ), 
                        
                        #### Zoom Controls ####
                        htmltools::div(id="ZoomPanel", class="panel panel-default well controls",
                                       shiny::h4("Map Graphics:", class="panel-heading"),
                                       htmltools::tags$div(title="Increases size of plots for easier viewing",
                                                           shiny::sliderInput(inputId = "PlotSize", label = "Scale plot markers (1x - 10x):", min = 1,
                                                                              max = 10, value = 5, step = 1, ticks = FALSE)),
                                       shiny::hr(),
                                       htmltools::tags$div(title="Overlay additional data onto the parks",
                                                           shiny::selectizeInput(inputId="MapLayer", label="Add a map layer:", choices=EXTRALAYERS)))
                      ),
                      htmltools::tags$div(
                        id = "toggle_map",
                        class = "sidebar-toggle-tab",
                        title = "Close data control panel",
                        htmltools::tags$span("\u00ab")
                      )
                    ), ## End of controls columns
                    
                    #### The Map ####
                    shiny::column(9, id = "mapMain", class = "collapsible-main-panel", style = "padding: 0 0 0 0px; position: relative;",
                                  htmltools::tags$div(id = "mapInfoOverlay", class = "info-popup-overlay"),
                                  htmltools::tags$div(id = "mapInfoBox", class = "info-popup-box",
                                                      htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                                      htmltools::includeHTML("www/AboutMap.html")
                                  ),
                                  htmltools::div(leaflet::leafletOutput("VegMap", height="calc(100vh - 80px)"))
                    )
                  )
  ),  ## end of map page


######################################## Graphs Panel ##########################################################

#############  densplot() based plots
shiny::tabPanel(
  htmltools::tags$div(title="Graph abundance, basal area, percent cover, etc","Data by Park and Species"),
  shiny::fluidRow(
    shiny::column(
      width = 3,
      id = "densSidebar",
      class = "sidebar-col-outer",
      htmltools::tags$div(
        id = "densSlideWrap",
        class = "sidebar-slide-wrap",
        shiny::wellPanel(
          class = "panel panel-default controls",
          shiny::h4("Data:", class = "panel-heading"),
          htmltools::tags$div(title = "Select the park you want to work with", shiny::uiOutput(outputId = "densParkControl")),
          htmltools::tags$div(title = "Select the time period you want to work with", shiny::uiOutput("densCycleControl")),
          htmltools::tags$div(title = "Select the type of plant you want to work with",
                              shiny::selectizeInput(inputId = "densGroup", label = "Type of plant:", choices = PLANTTYPES)
          ),
          htmltools::tags$div(title = "Toggle between common and scientific names",
                              shiny::checkboxInput(inputId = "densCommon", label = "Display common names?", value = TRUE)
          ),
          htmltools::tags$div(title = "Toggle summary statistics on or off",
                              shiny::checkboxInput(inputId = "plotlyText", label = "Display summary statistics?", value = FALSE)
          ),
          htmltools::tags$div(title = "Graph the most common species, species you select, or all species observed",
                              shiny::radioButtons(
                                inputId = "densSpeciesType",
                                label = "Which species?",
                                choices = base::c("Most common species" = "Common", "Pick individual species" = "Pick", "All species combined" = "All"),
                                inline = FALSE
                              )
          ),
          shiny::uiOutput(outputId = "densSpeciesControl"),
          htmltools::tags$div(title = "Select the type of data to graph",
                              shiny::uiOutput(outputId = "densValControl")
          ),
          shiny::conditionalPanel(
            condition = "input.densPanel=='Graph'",
            shiny::hr(),
            shiny::actionButton(inputId = "densGraphButton", label = "Display Options", class = "btn btn-primary btn-block action-button")
          ),
          shiny::conditionalPanel(
            condition = "input.densPanel=='Table'",
            shiny::hr(),
            shiny::downloadButton(outputId = "densTableDownload", label = "Save Table (.csv)", class = "btn btn-primary btn-block")
          )
        ),
        shiny::conditionalPanel(
          condition = "input.densPanel!='About'",
          shiny::wellPanel(
            class = "panel panel-default controls",
            shiny::h4("Comparison Data:", class = "panel-heading"),
            htmltools::tags$div(
              title = "Compare the base data with a different park, growth stage, or time period",
              shiny::radioButtons(
                inputId = "CompareType",
                label = "Compare to another:",
                choices = base::c("None", "Park", "Growth Stage", "Time"),
                selected = "None",
                inline = TRUE
              )
            ),
            shiny::uiOutput(outputId = "CompareSelect")
          )
        )
      ),
      htmltools::tags$div(
        id = "toggle_dens",
        class = "sidebar-toggle-tab",
        title = "Close data control panel",
        htmltools::tags$span("\u00ab")
      )
    ),
    shiny::column(
      width = 9,
      id = "densMain",
      class = "collapsible-main-panel",
      style = "position: relative;",
      htmltools::tags$div(class = "info-icon-btn", `data-target` = "densInfo",   title = "About this figure", "i"),
      htmltools::tags$div(id = "densInfoOverlay", class = "info-popup-overlay"),
      htmltools::tags$div(id = "densInfoBox", class = "info-popup-box",
                          htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                          htmltools::includeHTML("www/DensPlot.html")),
      shiny::tabsetPanel(
        id = "densPanel",
        type = "pills",
        shiny::tabPanel(
          title = htmltools::tags$div(title = "Graph the data", "Graph"),
          value = "Graph",
          htmltools::tags$div(
            id = "densPlotContainer",
            title = "Graph of Mean and 95% Confidence Interval",
            shiny::br(),
            shiny::uiOutput("densReportGraph"),
            shiny::uiOutput("densMissingWarningGraph"),
            shiny::uiOutput("densOnePlotWarningGraph"),
            shiny::uiOutput("DensLimitWarning"),
            plotly::plotlyOutput(outputId = "DensPlotly", height = "600px")
          ),
          shiny::fixedPanel(
            class = "panel panel-primary controls",
            draggable = TRUE,
            cursor = "auto",
            id = "GraphOptionsPanel",
            style = "padding: 0px; display: none; z-index: 1995;",
            title = "Display Options",
            htmltools::div(class = "panel-heading", shiny::h4("Display Options")),
            htmltools::div(
              class = "panel-body",
              shiny::flowLayout(
                cellArgs = base::list(style = "width: 160px"),
                shiny::selectizeInput("densBaseColor", "Base Data Color:", choices = COLORNAMES, selected = "blue", width = 150),
                shiny::selectizeInput("densCompareColor", "Comparison Data Color:", choices = COLORNAMES, selected = "red", width = 150)
              ),
              shiny::br(),
              shiny::flowLayout(
                cellArgs = base::list(style = "width: 160px"),
                shiny::sliderInput("densErrorThickness", "Error Bar Thickness", min = 0.5, max = 5, value = 1.5, step = 0.5, width = 150),
                shiny::sliderInput("densFontSize", "Change Font Size", min = 12, max = 24, value = 12, step = 2, width = 150)
              )
            ),
            htmltools::div(
              class = "panel-footer",
              shiny::actionButton(inputId = "CloseDisplayOptions", class = "btn btn-primary", label = "Close")
            )
          )
        ),
        shiny::tabPanel(
          htmltools::tags$div(title = "See all data in a table", "Data table"),
          value = "Table",
          htmltools::tags$div(style = "padding: 5px",
            style = "padding: 5px",
            shiny::br(),
            shiny::uiOutput("densReportTable"),
            shiny::h3(shiny::textOutput("densTableTitle")),
            shiny::hr(),
            shiny::uiOutput("densTableMessage"),
            shiny::uiOutput("densMissingWarningTable"),
            shiny::uiOutput("densOnePlotWarningTable"),
            shiny::uiOutput("densMessageTable"),
            DT::dataTableOutput("densTable")
          )
        )
      )
    )
  )
),

###############Time Series Plot
shiny::tabPanel(
  htmltools::tags$div(title="Graph data across monitoring cycles", "Data Trends Over Time"),
  shiny::fluidRow(
    
    # Side Panel
    shiny::column(
      width = 3,
      id = "tsSidebar",
      class = "sidebar-col-outer",
      htmltools::tags$div(
        id = "tsSlideWrap",
        class = "sidebar-slide-wrap",
        shiny::wellPanel(class="panel panel-default controls", shiny::h4("Data:", class="panel-heading"),
                         htmltools::tags$div(title="Select one or more parks to display. At least one park is required.", shiny::uiOutput(outputId="tsParkControl")),
                         htmltools::tags$div(title="Select the type of plant you want to work with", shiny::selectizeInput(inputId="tsGroup", label="Type of plant:", choices=PLANTTYPES)),
                         htmltools::tags$div(title="Toggle between common and scientific names", shiny::checkboxInput(inputId="tsCommon", label="Display common names?", value=TRUE)),
                         htmltools::tags$div(title="Toggle confidence interval ribbons on or off",
                                             shiny::checkboxInput(inputId = "tsShowCI", label = "Show 95% confidence intervals", value = TRUE)),
                         htmltools::tags$div(title="Graph the most common species, species you select, or all species observed", shiny::radioButtons(inputId="tsSpeciesType", label="Which species?",
                                                                                                                                                     choices=base::c("Most common species"="Common", "Pick individual species"="Pick", "All species combined"="All"), inline=FALSE)),
                         htmltools::tags$div(title="Select the measurement to display on the y-axis", shiny::uiOutput(outputId="tsValControl")),
                         shiny::uiOutput(outputId="tsSpeciesControl"),
                         htmltools::tags$div(title = "Select the range of monitoring cycles to display", shiny::uiOutput(outputId = "tsCycleControl")),
                         shiny::conditionalPanel(condition="input.tsPanel=='Graph'", shiny::hr(),
                                                 shiny::actionButton(inputId="tsGraphButton", label="Display Options", class="btn btn-primary btn-block action-button")),
                         shiny::conditionalPanel(condition="input.tsPanel=='Table'", shiny::hr(),
                                                 shiny::radioButtons(inputId = "tsTableOrder", label = "Order table by:", 
                                                                     choices = base::c("Species" = "species", "Cycle" = "cycle"),
                                                                     selected = "species", inline = TRUE),
                                                 shiny::downloadButton(outputId="tsTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block")))
      ),
      htmltools::tags$div(
        id = "toggle_ts",
        class = "sidebar-toggle-tab",
        title = "Close data control panel",
        htmltools::tags$span("\u00ab")
      )
    ), # close side column
    
    # Main Panel
    shiny::column(9, id = "tsMain", class = "collapsible-main-panel", style = "position: relative;",
                  htmltools::tags$div(class = "info-icon-btn", `data-target` = "tsInfo", title = "About this figure", "i"),
                  htmltools::tags$div(id = "tsInfoOverlay", class = "info-popup-overlay"),
                  htmltools::tags$div(id = "tsInfoBox", class = "info-popup-box",
                                      htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                      htmltools::includeHTML("www/AboutTS.html")),
                  shiny::tabsetPanel(id="tsPanel", type="pills",
                                     # Graph tab
                                     shiny::tabPanel(title=htmltools::tags$div(title="Graph the data", "Graph"), value="Graph",
                                                     htmltools::tags$div(id="tsPlotContainer", title="Time series of mean and 95% confidence interval by monitoring cycle",
                                                                         shiny::br(),
                                                                         shiny::uiOutput("tsReport"),
                                                                         shiny::uiOutput("tsMissingWarning"),
                                                                         shiny::uiOutput("tsSinglePlotWarning"),
                                                                         plotly::plotlyOutput(outputId="tsPlot", height="600px")),
                                                     # Display Options floater
                                                     shiny::fixedPanel(class="panel panel-primary controls", draggable=TRUE,
                                                                       cursor="auto", id="TSOptionsPanel", style="padding: 0px; display: none; z-index: 1995;",
                                                                       title="Display Options",
                                                                       htmltools::div(class="panel-heading", shiny::h4("Display Options")),
                                                                       htmltools::div(class="panel-body", shiny::flowLayout(cellArgs=base::list(style="width: 160px"),
                                                                                                                            shiny::sliderInput("tsLineThickness", "Line Thickness", min=0.5, max=5, value=1.5, step=0.5, width=150),
                                                                                                                            shiny::sliderInput("tsFontSize", "Font Size", min=12, max=24, value=12, step=2, width=150)),
                                                                                      shiny::br(),
                                                                                      shiny::flowLayout(cellArgs=base::list(style="width: 160px"),
                                                                                                        shiny::sliderInput("tsRibbonOpacity", "CI Ribbon Opacity", min=0.1, max=0.5, value=0.2, step=0.05, width=150),
                                                                                                        shiny::selectizeInput("tsColorPalette", "Color Palette:",
                                                                                                                              choices=base::c("Bright" = "set1",
                                                                                                                                              "Pastel" = "set2",
                                                                                                                                              "Dark" = "dark2",
                                                                                                                                              "Paired" = "paired"),
                                                                                                                              selected = "set1", width=150))),
                                                                       htmltools::div(class="panel-footer", shiny::actionButton(inputId="CloseTSDisplayOptions", class="btn btn-primary", label="Close")))
                                     ), # close Graph tabPanel
                                     
                                     # TABLE TAB
                                     shiny::tabPanel(title = htmltools::tags$div("Data table"), value = "Table",
                                                     htmltools::tags$div(style = "padding: 5px",
                                                                         shiny::br(),
                                                                         shiny::uiOutput("tsReportTable"),
                                                                         shiny::h3(shiny::textOutput("tsTableTitle")),
                                                                         shiny::hr(),
                                                                         shiny::uiOutput("tsMessageTable"),
                                                                         shiny::uiOutput("tsMissingWarningTable"),
                                                                         shiny::uiOutput("tsSinglePlotWarningTable"),
                                                                         DT::dataTableOutput("tsTable")))
                                     
                  ) # close tabsetPanel
    ) # close main column
    
  ) # close fluidRow
), # close tabPanel "Data Trends"

###############IV Plots
shiny::tabPanel(htmltools::tags$div(title="Graph Importance Values", "Forestry Importance Values (IV)"),
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    id = "ivSidebar",
                    class = "sidebar-col-outer",
                    htmltools::tags$div(
                      id = "ivSlideWrap",
                      class = "sidebar-slide-wrap",
                      shiny::wellPanel(class="panel panel-default controls",
                                       shiny::h4("Data:", class="panel-heading"),
                                       htmltools::tags$div(title="Select the park you want to work with",shiny::uiOutput("IVParkControl")),
                                       htmltools::tags$div(title="Select the time period you want to work with", shiny::uiOutput("IVCycleControl")),
                                       htmltools::tags$div(title="Select the type of plant you want to work with", 
                                                           shiny::selectizeInput(inputId="IVGroup", label="Type of plant:",choices=IVPLANTTYPES)),
                                       htmltools::tags$div(title="Toggle between common and scientific names",
                                                           shiny::checkboxInput(inputId="IVCommon", label="Display common names?", value=TRUE)),
                                       htmltools::tags$div(title="Toggle importance values on or off",
                                                           shiny::checkboxInput(inputId="IVPlotlyText", label="Display importance values?", value=FALSE)),
                                       htmltools::tags$div(title="Display density, size and disbribution separately",
                                                           shiny::checkboxInput(inputId="IVPart", label="Display components of the importance value?", value=FALSE)),
                                       # htmltools::tags$div(
                                       #   title="Pick the four year period you want to graph",
                                       #   shiny::sliderInput(inputId="IVYear", label="Display data from the 4 years ending:", min=YEARS$Start+YEARS$Range-1, 
                                       #               max=YEARS$End, value=YEARS$End, sep="", step=1,ticks=T)
                                       # ),
                                       htmltools::tags$div(title="Graph the most common species, species you select, or all species observed",
                                                           shiny::radioButtons(inputId="IVSpeciesType", label="Which species?", 
                                                                               choices=base::c("Most common species"="Common","Pick individual species"="Pick", 
                                                                                               "All species combined"="All"), inline=FALSE)),
                                       shiny::uiOutput(outputId="IVSpeciesControl"),
                                       shiny::conditionalPanel(
                                         condition="input.IVPanel=='Graph'",
                                         shiny::hr(),
                                         shiny::actionButton(inputId="IVGraphButton", label="Display Options", class="btn btn-primary btn-block action-button")
                                         #shiny::br(),
                                         #shiny::downloadButton(outputId="IVGraphDownload", label="Save Graph (.jpg)", class="btn btn-primary btn-block action-button"),
                                         #shiny::downloadButton(outputId="IVWmfDownload", label="Save Graph (.wmf)", class="btn btn-primary btn-block action-button")
                                       ),
                                       shiny::conditionalPanel(
                                         condition="input.IVPanel=='Table'",
                                         shiny::hr(),
                                         shiny::downloadButton(outputId="IVTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block")
                                       )
                      )
                    ),
                    htmltools::tags$div(
                      id = "toggle_iv",
                      class = "sidebar-toggle-tab",
                      title = "Close data control panel",
                      htmltools::tags$span("\u00ab")
                    )
                  ),
                  shiny::column(9, id = "ivMain", class = "collapsible-main-panel", style = "position: relative;",
                                htmltools::tags$div(class = "info-icon-btn", `data-target` = "ivInfo", title = "About this figure", "i"),
                                htmltools::tags$div(id = "ivInfoOverlay", class = "info-popup-overlay"),
                                htmltools::tags$div(id = "ivInfoBox", class = "info-popup-box",
                                                    htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                                    htmltools::includeHTML("www/IVPlot.html")),
                                shiny::tabsetPanel(id="IVPanel",type="pills",
                                                   shiny::tabPanel(value="Graph",
                                                                   htmltools::tags$div(title="Graph the data","Graph"),
                                                                   htmltools::tags$div(title="Graph of IV", 
                                                                                       shiny::br(),
                                                                                       shiny::uiOutput("ivReport"),
                                                                                       shiny::uiOutput("IVLimitWarning"),
                                                                                       plotly::plotlyOutput("IVPlot",height="600px")),
                                                                   
                                                                   shiny::fixedPanel(class="panel panel-primary controls",draggable=TRUE,
                                                                                     cursor="auto",id="IVOptionsPanel",style="padding: 0px; display: none; z-index: 1995;",title="Display Options",
                                                                                     htmltools::div(class="panel-heading", shiny::h4("Display Options")),
                                                                                     htmltools::div(class="panel-body",
                                                                                                    shiny::flowLayout(
                                                                                                      shiny::selectizeInput("IVBaseColor","Base Color:",choices=COLORNAMES, selected="green4",width="125px"),
                                                                                                      shiny::sliderInput("IVFontSize", "Change Font Size", min=12, max=24, value=12, step=2,width="175px")
                                                                                                    ),
                                                                                                    shiny::h5("Component Colors:"),
                                                                                                    shiny::flowLayout(
                                                                                                      shiny::selectizeInput("IVDensityColor","Density Color:",choices=COLORNAMES,
                                                                                                                            selected = if ("green4" %in% COLORNAMES) "green4" else COLORNAMES[[1]],
                                                                                                                            width="125px"),
                                                                                                      shiny::selectizeInput("IVSizeColor","Size Color:",choices=COLORNAMES,
                                                                                                                            selected = if ("chartreuse" %in% COLORNAMES) "chartreuse" else COLORNAMES[[1]],
                                                                                                                            width="125px"),
                                                                                                      shiny::selectizeInput("IVDistributionColor","Distribution Color:",choices=COLORNAMES,
                                                                                                                            selected = if ("yellow" %in% COLORNAMES) "yellow" else COLORNAMES[[1]],
                                                                                                                            width="125px")
                                                                                                    )
                                                                                     ),
                                                                                     htmltools::div(class="panel-footer", shiny::actionButton(inputId="CloseIVDisplayOptions",class="btn btn-primary",label="Close"))
                                                                   )
                                                   ),
                                                   shiny::tabPanel(htmltools::tags$div(title="See all data in a table","Data table"),
                                                                   value="Table",
                                                                   htmltools::tags$div(style = "padding: 5px", 
                                                                                       shiny::br(),
                                                                                       shiny::uiOutput("ivReportTable"),
                                                                                       shiny::h3(shiny::textOutput("IVTableTitle")),
                                                                                       shiny::hr(),
                                                                                       shiny::uiOutput("IVMessage"),
                                                                                       DT::dataTableOutput("IVData"),
                                                                   )
                                                   )
                                )
                  )
                )
),

############################## Species Lists
shiny::tabPanel(id="SpeciesPanel",
                htmltools::tags$div(
                  title="Lists of plants found in the parks", "Species Lists"
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    id = "SpeciesControls",
                    class = "sidebar-col-outer",
                    htmltools::tags$div(
                      id = "spSlideWrap",
                      class = "sidebar-slide-wrap",
                      shiny::wellPanel(id = "specDataPanel", class="panel panel-default controls",
                                       shiny::h4("Species Data:", class="panel-heading"),
                                       htmltools::tags$div(
                                         title="Select the type of species list", 
                                         shiny::radioButtons(inputId="SpListType", label="Select a species list:",
                                                             choices=base::c("Vascular plants in the monitorng plots"= "Monitoring", "All vascular plants known from the park"="NPSpecies"))
                                       ),
                                       htmltools::tags$div(
                                         title="Select a park to work with",shiny::uiOutput("SpListParkControl")
                                       ),
                                       shiny::conditionalPanel(condition="input.SpListType=='Monitoring'",
                                                               htmltools::tags$div(
                                                                 title="Select one or more plots, select and backspace to delete.", shiny::uiOutput("SpListPlotControl")
                                                               )
                                       ),
                                       shiny::conditionalPanel(
                                         shiny::hr(),
                                         shiny::downloadButton(outputId="SpeciesTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block")
                                       )
                      )
                    ),
                    htmltools::tags$div(
                      id = "toggle_sp",
                      class = "sidebar-toggle-tab",
                      title = "Close data control panel",
                      htmltools::tags$span("\u00ab")
                    )
                  ),
                  shiny::column(9, id = "spMain", class = "collapsible-main-panel", style = "position: relative;",
                                shiny::br(), shiny::br(),
                                htmltools::tags$div(class = "info-icon-btn", `data-target` = "spInfo", title = "About this figure", "i"),
                                htmltools::tags$div(id = "spInfoOverlay", class = "info-popup-overlay"),
                                htmltools::tags$div(id = "spInfoBox", class = "info-popup-box",
                                                    htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                                    htmltools::includeHTML("www/AboutLists.html")),
                                shiny::h3(shiny::textOutput("SpeciesTableTitle")),
                                shiny::uiOutput("NPSpeciesLink"),
                                shiny::hr(),
                                DT::dataTableOutput("SpeciesTable")
                                        
                  )
                )
),
##################### About
    tabPanel(
      tags$div(
        title="About the project", "About"),
      tags$base(target="_blank"),
      includeHTML("www/AboutTab.html") 
      )#end About menu
)#end shiny::navbarPage()

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
  /* global */
      .action-button {white-space: normal !important; word-wrap: break-word !important;}
 
      body {padding-top: 80px;}
      html, body {overflow-x: hidden; max-width: 100%;}
 
      .navbar {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 2000 !important;
        min-height: auto !important;
        padding-top: 1.5rem;
        padding-bottom: 1.5rem;}
 
      .navbar-inverse .navbar-brand {
        font-size: calc(1.5rem + 1.5vw);
        font-family: 'Times New Roman';
        color: #fff !important;
        line-height: 1.0;
        padding-left: 1rem;
        margin: 0;
        pointer-events:none;}
 
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
 
      #MainNavBar .tab-content {overflow-x: hidden;}
 
      .selectize-control .selectize-input {
        position: relative;}
      .selectize-control .selectize-input::after {
        margin-top: 0 !important;
        margin-right: 0 !important;
        position: absolute;
        right: 1rem !important;
        transform: translateY(-50%);}
      .selectize-dropdown {z-index: 10800 !important;}
      .selectize-dropdown-content {
        max-height: 45vh;
        overflow-y: auto;
        -webkit-overflow-scrolling: touch;}
 
/* shared features across tabs */

    /* display options popup (dens, ts, iv) */
    
      #densOptionsBox, #tsOptionsBox, #ivOptionsBox {width: min(92vw, 900px) !important;}
      #densOptionsBox .shiny-flow-layout, #tsOptionsBox .shiny-flow-layout, #ivOptionsBox .shiny-flow-layout {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        align-items: flex-start;
        width: 100%;}
      #densOptionsBox .shiny-flow-layout > *,
      #tsOptionsBox .shiny-flow-layout > * {
        padding-right: 0 !important;
        padding-bottom: 0 !important;
        display: flex !important;
        justify-content: center !important;}
      .info-popup-box .selectize-dropdown {
        position: static !important;
        width: 100% !important;
        margin-top: 4px;}
      .info-popup-box:has(.selectize-control.dropdown-active) {
        max-height: min(95vh, calc(100vh - 40px)) !important;
        overflow-y: auto !important;
        transition: max-height 0.2s ease;}
      
    /* info icon/popup (map, dens, ts, iv, sp list) */
    
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
        z-index: 10699;
        display: none;}
      .info-popup-overlay.active {display: block;}
      .info-popup-box {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: min(90vw, 700px);
        max-height: min(80vh, calc(100vh - 100px));
        overflow-y: auto;
        background-color: #ffffff;
        border-radius: 8px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
        z-index: 10700;
        padding: clamp(16px, 4vw, 24px);
        box-sizing: border-box;
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
 
    /* sidebar (dens, ts, iv, sp list) */
    
      .sidebar-col-outer {
        position: relative;
        float: left;
        box-sizing: border-box;
        transition: width 0.5s ease;
        overflow: visible;}
      .sidebar-col-outer.collapsed {
        width: 24px !important;
        padding-left: 0 !important;
        padding-right: 0 !important;}
      .sidebar-slide-wrap {
        position: relative;
        width: 100%;
        opacity: 1;
        max-height: 2000px;
        overflow: hidden;
        transition: margin-left 0.5s ease, max-height 0.5s ease;}
      .sidebar-slide-wrap.collapsed {
        margin-left: -2000px;
        max-height: 0;}
      .sidebar-slide-wrap.opening {opacity: 0;}
      .sidebar-toggle-tab {
        position: absolute;
        top: 50%;
        transform: translateY(-50%);
        right: -5px;
        width: 20px;
        height: 72px;
        background-color: #7c8f4f;
        color: #ffffff;
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
        background-color: #6b7d43;
        color: #ffffff;}
      .collapsible-main-panel {
        float: left;
        box-sizing: border-box;
        transition: none;
        padding-left: 10px !important;}
      .collapsible-main-panel.main-full-width {width: calc(100% - 24px) !important;}
        
    /* summary report (dens, ts, iv) */
    
      .report-body {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.3s ease;}
      .report-body.open {
        max-height: 70vh;
        overflow-y: auto;
        transition: max-height 0.5s ease;}
      .report-caret {
        display: inline-block;
        transition: transform 0.3s ease;
        transform: rotate(-90deg);}
      .report-caret.open {transform: rotate(0deg);}
 
    /* sidebar border across app */
    
      #densSlideWrap .panel.panel-default,
      #tsSlideWrap .panel.panel-default,
      #ivSlideWrap .panel.panel-default,
      #mapSlideWrap .panel.panel-default,
      #spSlideWrap .panel.panel-default {
        border-color: #7c8f4f !important;}
    
    /* sidebar picklist across app */
    
      #mapFiltersSidebar .selectize-dropdown,
      #densSlideWrap .selectize-dropdown,
      #tsSlideWrap .selectize-dropdown,
      #ivSlideWrap .selectize-dropdown,
      #spSlideWrap .selectize-dropdown {
        position: static !important;
        width: 100% !important;
        margin-top: 4px;}
 
  /* map */
    /* layout */
    
      #MainNavBar .tab-content > .tab-pane[data-value=\"Map\"] {
        padding: 0 !important;
        margin: 0 !important;
        overflow: hidden !important;}
      #mapMain {
        padding: 0 !important;
        margin: 0 !important;
        position: fixed !important;
        top: 80px !important;
        left: 0 !important;
        right: 0 !important;
        bottom: 0 !important;
        width: 100% !important;
        overflow: hidden !important;}
      #MapControlPanel.panel.panel-default {
        border-radius: 4px !important;}
 
    /* control panel side bar */
    
      .map-sidebar-overlay {
        position: fixed;
        top: 0; left: 0; width: 100%; height: 100%;
        background-color: rgba(0,0,0,0.25);
        z-index: 10500;
        display: none;}
      .map-sidebar-overlay.active { display: block; }
      .map-sidebar {
        position: fixed;
        top: 80px;
        left: -340px;
        bottom: 0;
        width: 340px;
        max-width: 85vw;
        background-color: #ffffff;
        box-shadow: 3px 0 15px rgba(0,0,0,0.3);
        z-index: 10600;
        transition: left 0.3s ease;
        display: flex;
        flex-direction: column;
        overflow: hidden; 
        will-change: left;
        backface-visibility: hidden;}
      .map-sidebar.active { left: 0; }
      .map-sidebar-scroll {
        overflow-y: auto;
        min-height: 0;
        padding: 16px;
        flex: 1;}
        .map-sidebar .selectize-dropdown,
        body > .selectize-dropdown.map-sidebar-dropdown {z-index: 10700 !important;}
      .map-sidebar-close {
        position: absolute;
        top: 8px; right: 12px;
        font-size: 24px;
        cursor: pointer;
        color: #888;
        border: none;
        background: none;
        z-index: 2;}
      .map-sidebar-close:hover { color: #333; }
 
    /* round icon buttons */

      .map-round-icon-btn {
        width: 34px; height: 34px;
        background-color: #ffffff;
        border: none;
        border-radius: 50%;
        box-shadow: 0 1px 5px rgba(0,0,0,0.4);
        cursor: pointer;
        display: flex; align-items: center; justify-content: center;
        font-size: 16px; color: #555;
        text-decoration: none;
        outline: none;}
      .map-round-icon-btn:hover { background-color: #f4f4f4; color: #222; }
      .map-round-icon-btn:focus,
      .map-round-icon-btn:active { outline: none; box-shadow: 0 1px 5px rgba(0,0,0,0.4); }

      .map-info-leaflet-control,
      .map-controls-leaflet-control {
        width: 34px;
        height: 34px;
        border-radius: 50% !important;
        overflow: hidden;
        box-shadow: none !important;}

      .map-info-leaflet-control a,
      .map-controls-leaflet-control a,
      .plotsize-picker a {
        border-bottom: none !important;
        border: none !important;}

    /* hamburger control */
      .map-controls-leaflet-control {
        width: 42px !important;
        height: 42px !important;
        border-radius: 4px !important;
        overflow: visible;
        border: none !important;
        box-shadow: none !important;
        background: transparent !important;
        margin-top: 13px !important;}

      .map-controls-leaflet-control .map-round-icon-btn {
        width: 42px !important;
        height: 42px !important;
        font-size: 24px !important;
        border-radius: 4px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        line-height: 1 !important;
        box-shadow: 0 0 0 2px rgba(0,0,0,0.2) !important;}
 
    /* bottom left tile layers selection */
    
      .gmaps-style-basepicker {
        display: flex;
        align-items: flex-end;
        gap: 6px;}
      .gmaps-toggle {
        width: 76px; height: 76px;
        border-radius: 6px;
        box-shadow: 0 1px 6px rgba(0,0,0,0.45);
        cursor: pointer;
        background-size: cover;
        background-position: center;
        background-color: #ddd;
        border: 2px solid #fff;
        display: flex; align-items: flex-end; justify-content: center;
        font-size: 11px; color: #fff; font-weight: bold;
        text-shadow: 0 1px 3px rgba(0,0,0,0.9);
        padding-bottom: 4px;}
      .gmaps-strip {
        display: none;
        gap: 6px;
        background: #fff;
        padding: 6px;
        border-radius: 6px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.4);}
      .gmaps-strip.open { display: flex; }
      .gmaps-swatch {
        width: 64px; height: 64px;
        border-radius: 4px;
        cursor: pointer;
        border: 2px solid transparent;
        background-size: cover;
        background-position: center;
        background-color: #ddd;
        display: flex; align-items: flex-end; justify-content: center;
        font-size: 10px; color: #fff; font-weight: bold;
        text-shadow: 0 1px 3px rgba(0,0,0,0.9);
        padding-bottom: 3px;}
      .gmaps-swatch.active { border-color: #4a90d9; }
      @media (max-width: 600px) {
        .gmaps-style-basepicker {
          flex-direction: column-reverse;
          align-items: flex-start;}
        .gmaps-strip {
          flex-direction: column;}}
 
    /* plot size slider */
    
      .plotsize-picker {
        display: flex;
        align-items: center;
        justify-content: flex-end;
        width: 34px;
        height: 34px;
        border-radius: 50% !important;
        overflow: hidden;
        box-shadow: none !important;}
      .plotsize-picker.plotsize-expanded {
        width: auto;
        height: auto;
        border-radius: 6px;
        overflow: visible;}
      .plotsize-toggle {
        gap: 3px;
        cursor: pointer;
        outline: none;}
      .plotsize-toggle:hover { background-color: #f4f4f4; color: #222; }
      .psdot {
        display: inline-block;
        border-radius: 50%;
        background-color: #555;}
      .psdot-sm { width: 3px; height: 3px; }
      .psdot-md { width: 5px; height: 5px; }
      .psdot-lg { width: 7px; height: 7px; }
      .plotsize-strip {
        display: none;
        flex-direction: column;
        justify-content: center;
        background: #fff;
        border-radius: 6px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.4);
        padding: 4px 10px;
        width: 220px;
        height: 52px;
        box-sizing: border-box;
        overflow: hidden;}
      .plotsize-expanded .plotsize-toggle { display: none; }
      .plotsize-expanded .plotsize-strip { display: flex; }
      .plotsize-endlabel-row {
        display: flex;
        justify-content: space-between;
        margin-top: 2px;}
      .plotsize-label {
        font-size: 11px;
        font-weight: bold;
        color: #555;
        white-space: nowrap;
        margin-bottom: 0;}
      .plotsize-slider {
        width: 100%;
        cursor: pointer;
        -webkit-appearance: none;
        appearance: none;
        height: 2px;
        background: #ddd;
        border-radius: 2px;
        outline: none;}
      .plotsize-slider::-webkit-slider-thumb {
        -webkit-appearance: none;
        appearance: none;
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background: #4a90d9;
        cursor: pointer;}
      .plotsize-slider::-moz-range-thumb {
        width: 10px;
        height: 10px;
        border: none;
        border-radius: 50%;
        background: #4a90d9;
        cursor: pointer;}
      .plotsize-slider::-moz-range-track {
        height: 4px;
        background: #ddd;
        border-radius: 2px;}
      .plotsize-endlabel {
        font-size: 8px;
        font-weight: bold;
        color: #777;
        white-space: nowrap;}
      
    /* layer transparency slider */

      .opacity-picker {
        display: flex;
        align-items: center;
        justify-content: flex-end;
        width: 34px;
        height: 34px;
        border-radius: 50% !important;
        overflow: hidden;
        box-shadow: none !important;}
      .opacity-picker.opacity-expanded {
        width: auto;
        height: auto;
        border-radius: 6px;
        overflow: visible;}
      .opacity-toggle {
        cursor: pointer;
        outline: none;
        font-size: 16px;}
      .opacity-toggle:hover { background-color: #f4f4f4; color: #222; }
      .opacity-expanded .opacity-toggle { display: none; }
      .opacity-expanded .opacity-strip { display: flex; }
      .opacity-strip {
        display: none;
        flex-direction: column;
        justify-content: center;
        background: #fff;
        border-radius: 6px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.4);
        padding: 4px 10px;
        width: 220px;
        height: 52px;
        box-sizing: border-box;
        overflow: hidden;}
      .opacity-label {
        font-size: 11px;
        font-weight: bold;
        color: #555;
        white-space: nowrap;
        margin-bottom: 0;}
            .opacity-slider {
        width: 100%;
        cursor: pointer;
        -webkit-appearance: none;
        appearance: none;
        height: 2px;
        background: #ddd;
        border-radius: 2px;
        outline: none;}
      .opacity-slider::-webkit-slider-thumb {
        -webkit-appearance: none;
        appearance: none;
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background: #4a90d9;
        cursor: pointer;}
      .opacity-slider::-moz-range-thumb {
        width: 10px;
        height: 10px;
        border: none;
        border-radius: 50%;
        background: #4a90d9;
        cursor: pointer;}
      .opacity-slider::-moz-range-track {
        height: 4px;
        background: #ddd;
        border-radius: 2px;}
      .opacity-endlabel-row {
        display: flex;
        justify-content: space-between;
        margin-top: 2px;}
      .opacity-endlabel {
        font-size: 8px;
        font-weight: bold;
        color: #777;
        white-space: nowrap;}
        
      .leaflet-top.leaflet-right .info.legend.legend-collapsed::after {content: '+';}
      .leaflet-top.leaflet-right .info.legend.soil-legend {
        max-width: 300px !important;
        white-space: nowrap;}
      .leaflet-bottom.leaflet-right .leaflet-control {
        filter: drop-shadow(0 0 2px rgba(255, 255, 255, 1)) 
        drop-shadow(0 0 2px rgba(255, 255, 255, 1)) 
        drop-shadow(0 0 6px rgba(255, 255, 255, 1)) 
        drop-shadow(0 0 12px rgba(255, 255, 255, 1)) 
        drop-shadow(0 0 24px rgba(255, 255, 255, 1));}
    @media (max-height: 880px) and (max-width: 600px) {
      .plotsize-strip {
        position: relative;
        overflow: visible;
        grid-template-columns: 20px 20px;
        grid-template-rows: auto 150px;
        grid-template-areas:
          'label label'
          'slider caps';
        gap: 4px;
        align-items: center;
        justify-items: center;
        width: auto;
        height: auto;
        padding: 10px 6px;}
      .plotsize-expanded .plotsize-strip {display: grid;}
      .plotsize-label {
        grid-area: label;
        writing-mode: horizontal-tb;
        white-space: normal;
        text-align: center;
        margin: 0;}
      .plotsize-slider {
        position: absolute;
        top: 65%;
        left: 20px;
        width: 150px !important;
        transform: translate(-50%, -50%) rotate(-90deg);
        transform-origin: center;
        margin: 0;}
      .plotsize-endlabel-row {
        grid-area: caps;
        flex-direction: column-reverse;
        justify-content: space-between;
        align-items: center;
        height: 150px;
        margin-top: 0;}
      .opacity-strip {
        position: relative;
        overflow: visible;
        grid-template-columns: 20px 20px;
        grid-template-rows: auto 150px;
        grid-template-areas:
          'label label'
          'slider caps';
        gap: 4px;
        align-items: center;
        justify-items: center;
        width: auto;
        height: auto;
        padding: 10px 6px;}
      .opacity-expanded .opacity-strip {display: grid;}
      .opacity-label {
        grid-area: label;
        writing-mode: horizontal-tb;
        white-space: normal;
        text-align: center;
        margin: 0;}
      .opacity-slider {
        position: absolute;
        top: 65%;
        left: 20px;
        width: 150px !important;
        transform: translate(-50%, -50%) rotate(-90deg);
        transform-origin: center;
        margin: 0;}
      .opacity-endlabel-row {
        grid-area: caps;
        flex-direction: column-reverse;
        justify-content: space-between;
        align-items: center;
        height: 150px;
        margin-top: 0;}}
    
    /* no observations toggle */
    
      .zero-toggle-btn {
        margin-top: 8px;
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 24 24%27 fill=%27none%27 stroke=%27%23555%27 stroke-width=%272%27 stroke-linecap=%27round%27 stroke-linejoin=%27round%27%3E%3Cpath d=%27M1 12s4-7 11-7 11 7 11 7-4 7-11 7-11-7-11-7z%27%3E%3C/path%3E%3Ccircle cx=%2712%27 cy=%2712%27 r=%273%27%3E%3C/circle%3E%3C/svg%3E');
        background-repeat: no-repeat;
        background-position: center;
        background-size: 18px 18px;}
      .zero-toggle-btn.zero-toggle-active {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 24 24%27 fill=%27none%27 stroke=%27%23c0392b%27 stroke-width=%272%27 stroke-linecap=%27round%27 stroke-linejoin=%27round%27%3E%3Cpath d=%27M17.94 17.94A10.94 10.94 0 0 1 12 19c-7 0-11-7-11-7a18.5 18.5 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 7 11 7a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24%27%3E%3C/path%3E%3Cline x1=%271%27 y1=%271%27 x2=%2723%27 y2=%2723%27%3E%3C/line%3E%3C/svg%3E');}
      
  /* other reactive features to screen sizes */
   
      @media (max-height: 880px) {
        .leaflet-bottom.leaflet-right {z-index: 1000;}
        .leaflet-bottom.leaflet-right {
          display: flex;
          flex-direction: row-reverse;
          align-items: flex-end;
          gap: 4px;}
        .leaflet-bottom.leaflet-right .leaflet-control {
          margin-bottom: 4px;
          margin-right: 4px;}}
        .leaflet-top.leaflet-right {
          max-height: calc(100vh - 100px);
          overflow-y: auto;
          overflow-x: hidden;
          z-index: 1000;}
        .leaflet-top.leaflet-right.corner-raised,
        .leaflet-bottom.leaflet-right.corner-raised {z-index: 2000 !important;}
        .leaflet-top.leaflet-right .info.legend::after {
          content: '-';
          position: absolute;
          top: 6px;
          right: 6px;
          font-size: 15px;
          font-weight: bold;
          color: #555;
          line-height: 1;}
        .leaflet-top.leaflet-right .info.legend.legend-collapsed {
          max-height: 25px;
          overflow: hidden;
          padding-bottom: 5px !important;}
        .leaflet-top.leaflet-right .info.legend.legend-collapsed::after {
          content: '+';}
      @media (max-width: 1024px) {
        .sidebar-col-outer {
          width: 100% !important;
          float: none !important;
          overflow: visible !important;}
        .sidebar-col-outer.collapsed {
          width: 100% !important;
          height: 20px !important;
          min-height: 20px !important;
          padding: 0 !important;}
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
          transition: max-height 0.5s ease !important;}
        .sidebar-slide-wrap.collapsed {
          margin-left: 0 !important;
          max-height: 0 !important;}
        .sidebar-slide-wrap .sidebar-content,
        .sidebar-slide-wrap.collapsed .sidebar-content {opacity: 1 !important;}
        .sidebar-toggle-tab {
          position: absolute !important;
          top: auto !important;
          bottom: -2px !important;
          left: 50% !important;
          right: auto !important;
          background-color: #7c8f4f;
          color: #ffffff;
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
          overflow-y: auto !important;}}
      @media (hover: none) {
        .leaflet-tooltip {
          display: none !important;}}
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
      $t.attr('title','');
    }
  });
});

$(document).on('show.bs.collapse', function(e) {
  if ($(e.target).hasClass('navbar-collapse')) {
    $('#navbar-overlay').fadeIn(200);
  }
});

$(document).on('hide.bs.collapse', function(e) {
  if ($(e.target).hasClass('navbar-collapse')) {
    $('#navbar-overlay').fadeOut(200);
  }
});

$(document).on('click', '#navbar-overlay', function() {
  $('.navbar-collapse.in').collapse('hide');
  $(this).fadeOut(200);
});

(function () {
  function getSelectize(el) { return el && el.selectize ? el.selectize : null; }

  $(document).on('shiny:bound', function (ev) {
    var $el = $(ev.target);
    if (!$el.is('select') || !$el.hasClass('selectized')) return;
    var sel = getSelectize($el[0]);
    if (!sel) return;
    sel.settings.openOnFocus = false;
    var $control = $el.next('.selectize-control').find('.selectize-input');

    $control.on('touchstart.selectizeToggle', function (e) {
  e.preventDefault();
});

$control.on('pointerdown.selectizeToggle', function (e) {
  if ($(e.target).is('input, textarea')) return;
  e.preventDefault();
  e.stopImmediatePropagation();
  if (sel.isOpen) { sel.close(); sel.blur(); }
  else { sel.open(); sel.focus(); }
});

$control.on('click.selectizeToggle', function (e) {
  e.preventDefault();
  e.stopPropagation();
});

$control.on('mousedown.selectizeToggle touchend.selectizeToggle', function (e) {
  e.stopPropagation();
});

sel.on('item_select', function () { sel.close(); sel.blur(); });
sel.on('dropdown_close', function () {sel.blur();
});
    sel.on('dropdown_open', function() {
      $el.closest('.sidebar-col-outer').css('overflow', 'visible');
      $el.closest('.sidebar-slide-wrap').css('overflow', 'visible');
      $el.closest('.well').css('overflow', 'visible');
    });

    sel.on('dropdown_close', function() {
      window.lastSelectizeCloseTime = Date.now();
      $el.closest('.sidebar-col-outer').css('overflow', '');
      $el.closest('.sidebar-slide-wrap').css('overflow', '');
      $el.closest('.well').css('overflow', '');
    });
  });

  $(document).on('pointerdown.selectizeOutside', function (e) {
    var $t = $(e.target);
    if ($t.closest('.selectize-control').length) return;
    if ($t.closest('.selectize-dropdown').length) return;
    $('.selectized').each(function () {
      var sel = getSelectize(this);
      if (sel && sel.isOpen) sel.close();
    });
  });
})();

$(document).on('click', '.report-header', function() {
  var $body = $(this).next('.report-body');
  var $caret = $(this).find('.report-caret');
  $body.toggleClass('open');
  $caret.toggleClass('open');
});
")),

htmltools::tags$script(htmltools::HTML("
$(document).on('click', '.info-icon-btn, .options-toggle-btn', function() {
  var targetId = $(this).data('target');
  $('#' + targetId + 'Overlay').addClass('active');
  $('#' + targetId + 'Box').addClass('active');
});

$(document).on('click', '.info-popup-close, .info-popup-overlay', function(e) {
  // ignore this click if a selectize dropdown is still open, OR if one
  // just closed within the last 300ms — covers touch devices where
  // selectize's own outside-tap handling can resolve before this handler
  // runs, so a live DOM check alone isn't reliable.
  if ($('.info-popup-box .selectize-control.dropdown-active').length > 0) return;
  if (Date.now() - (window.lastSelectizeCloseTime || 0) < 300) return;
  $('.info-popup-overlay').removeClass('active');
  $('.info-popup-box').removeClass('active');
});

$(document).on('click', '.info-popup-box', function(e) {
  e.stopPropagation();
});
")),

htmltools::tags$script(htmltools::HTML("
$(document).on('click', '.leaflet-top.leaflet-right .info.legend', function() {
  var $corner = $(this).closest('.leaflet-top.leaflet-right');
  if ($corner.data('justRaised')) {
    $corner.removeData('justRaised');
    return;
  }
  $(this).toggleClass('legend-collapsed');
});
")),

htmltools::tags$script(htmltools::HTML("
$(document).on('mousedown touchstart', '.leaflet-top.leaflet-right, .leaflet-bottom.leaflet-right', function() {
  var wasRaised = $(this).hasClass('corner-raised');
  $('.leaflet-top.leaflet-right, .leaflet-bottom.leaflet-right').removeClass('corner-raised');
  $(this).addClass('corner-raised');
  if (!wasRaised) {
    $(this).data('justRaised', true);
  }
});
")),

htmltools::tags$script(htmltools::HTML("
Shiny.addCustomMessageHandler('resetMapLayers', function(msg) {
  var map = window.vegMap;
  if (!map) return;

  $('.leaflet-control-layers-selector').each(function() {
    var label = $(this).closest('label').text().trim();
    if (label === 'None' && !this.checked) {
      this.click();
    }
  });

  var allBaseLayers = ['Map', 'Imagery', 'Light', 'Slate'];
  allBaseLayers.forEach(function(name) {
    if (name === 'Map') {
      if (!map.hasLayer(map.baseTileLayers[name])) map.addLayer(map.baseTileLayers[name]);
    } else {
      if (map.hasLayer(map.baseTileLayers[name])) map.removeLayer(map.baseTileLayers[name]);
    }
  });
  $('.gmaps-swatch').removeClass('active');
  $('.gmaps-swatch[data-layer=\"Map\"]').addClass('active');
  var toggleIconUrl = $('.gmaps-swatch[data-layer=\"Imagery\"]').css('background-image');
  $('.gmaps-toggle').css('background-image', toggleIconUrl);
});
")),
                       
htmltools::tags$script(htmltools::HTML("
function lockToggleTabPosition($sidebarOuter, $toggleTab) {
  if ($toggleTab.data('lockedTop') === undefined) {
    var outerH = $sidebarOuter.outerHeight();
    var tabH = $toggleTab.outerHeight();
    if (!outerH || !tabH) return;
    var top = (outerH - tabH) / 2;
    $toggleTab.data('lockedTop', top);
  }
  $toggleTab.css({ top: $toggleTab.data('lockedTop') + 'px', transform: 'none' });
}

$(document).on('shiny:connected', function() {
  ['dens', 'ts', 'iv', 'sp'].forEach(function(panelId) {
    var sidebarOuterId = (panelId === 'sp') ? 'SpeciesControls' : (panelId + 'Sidebar');
    var $sidebarOuter = $('#' + sidebarOuterId);
    var $toggleTab = $('#toggle_' + panelId);
    if ($sidebarOuter.length && $toggleTab.length) {
      lockToggleTabPosition($sidebarOuter, $toggleTab);
    }
  });
});

['dens', 'ts', 'iv', 'sp'].forEach(function(panelId) {
  $(document).on('click', '#toggle_' + panelId, function() {
    var sidebarOuterId = (panelId === 'sp') ? 'SpeciesControls' : (panelId + 'Sidebar');
    var wrapId = (panelId === 'sp') ? 'spSlideWrap' : (panelId + 'SlideWrap');

    var $sidebarOuter = $('#' + sidebarOuterId);
    var $wrap = $('#' + wrapId);
    var $main = $('#' + panelId + 'Main');
    var $toggleTab = $('#toggle_' + panelId);
    var $chevron = $toggleTab.find('span');
    var isMobile = $(window).width() <= 1024;

    if ($wrap.hasClass('collapsed')) {
      $wrap.removeClass('collapsed');
      $sidebarOuter.removeClass('collapsed');
      if (!isMobile) $main.removeClass('main-full-width');
      $chevron.text('\u00ab');
      $toggleTab.attr('title', 'Close data control panel');
      if (!isMobile) {
        setTimeout(function() {
          $wrap.removeClass('opening');
          $sidebarOuter.find('input.js-range-slider').each(function() {
            var inst = $(this).data('ionRangeSlider');
            if (inst) inst.update();
          });
        }, 120);
      }
    } else {
      $wrap.addClass('collapsed');
      $chevron.text('\u00bb');
      $toggleTab.attr('title', 'Open data control panel');
      if (!isMobile) {
        $sidebarOuter.addClass('collapsed');
        setTimeout(function() { $main.addClass('main-full-width'); }, 500);
      } else {
        setTimeout(function() { $sidebarOuter.addClass('collapsed'); }, 500);
      }
    }

    if (!isMobile) lockToggleTabPosition($sidebarOuter, $toggleTab);

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
    var isMobile = $(window).width() <= 1024;
    $('.sidebar-slide-wrap.collapsed').removeClass('collapsed');
    $('.sidebar-col-outer.collapsed').removeClass('collapsed');
    if (!isMobile) $('.main-full-width').removeClass('main-full-width');
    $('.sidebar-toggle-tab span').text('\u00ab');
    $('.sidebar-toggle-tab').attr('title', 'Close data control panel');
  }
});
")),

htmltools::tags$script(htmltools::HTML("
$(document).ready(function() {
  $('body').append('<div id=\"navbar-overlay\" style=\"display:none; position:fixed; top:0; left:0; width:100%; height:100%; background:rgba(0,0,0,0.35); z-index:1998;\"></div>');
});
$(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
  var isMapTab = $(this).attr('data-value') === 'Map';
  $('body').toggleClass('on-map-tab', isMapTab);
});
function checkMapTabActive() {
  var isMapTab = $('.tab-content > .tab-pane.active[data-value=\"Map\"]').length > 0;
  $('body').toggleClass('on-map-tab', isMapTab);
}
$(document).on('shiny:connected', function() {
  checkMapTabActive();
  setTimeout(checkMapTabActive, 500);
});
$(document).on('click', '.map-sidebar-close, .map-sidebar-overlay', function() {
  $('.map-sidebar, .map-sidebar-overlay').removeClass('active');
});
$(document).on('click', '.map-sidebar', function(e) { e.stopPropagation(); });
 
")),

htmltools::tags$script(htmltools::HTML("
$(document).on('shiny:connected', function() {
  var isMobile = $(window).width() <= 1024;
  if (!isMobile) {
    $('#mapFiltersSidebar').addClass('active');
    $('#mapFiltersOverlay').addClass('active');
  }
});
")),
                       
htmltools::tags$script(htmltools::HTML("
function reportW() { Shiny.setInputValue('screenW', document.documentElement.clientWidth, {priority: 'event'}); }
$(document).on('shiny:connected', reportW);
$(window).on('resize', function() { clearTimeout(window._wT); window._wT = setTimeout(reportW, 200); });
")),

htmltools::tags$script(htmltools::HTML("
Shiny.addCustomMessageHandler('toggleSliderDisable', function(msg) {
  msg.ids.forEach(function(id) {
    var $el = $('#' + id);
    var inst = $el.data('ionRangeSlider');
    if (inst) {
      inst.update({ disable: msg.disable });
    }
  });
});
")),

htmltools::tags$script(htmltools::HTML("
function trackContainerWidth(id) {
  var el = document.getElementById(id);
  if (!el || !window.ResizeObserver) return;
  var ro = new ResizeObserver(function(entries) {
    for (var entry of entries) {
      Shiny.setInputValue(id + '_width', Math.round(entry.contentRect.width), {priority: 'event'});
    }
  });
  ro.observe(el);
}
$(document).on('shiny:connected', function() {
  trackContainerWidth('densPlotContainer');
  trackContainerWidth('tsPlotContainer');
  trackContainerWidth('ivPlotContainer');
});
")),
                       
htmltools::includeHTML("www/google-analytics.html")
  ),
  
  ######################################### Map Panel ####################################################################
  
shiny::tabPanel(htmltools::tags$div(title="Map the data", "Map"), value = "Map", style="padding: 0",
                shinyjs::useShinyjs(),
                
                htmltools::tags$div(id = "mapMain", style = "position: relative; width: 100%; padding: 0; margin: 0;",
                  
                  # sidebar filters
                  htmltools::tags$div(id = "mapFiltersOverlay", class = "map-sidebar-overlay"),
                  htmltools::tags$div(id = "mapFiltersSidebar", class = "map-sidebar",
                    htmltools::tags$div(id = "mapSlideWrap", class = "map-sidebar-scroll",
                      htmltools::div(id="MapControlPanel", class="panel panel-default well controls",
                                     shiny::h4("Map Controls", class="panel-heading"),
                                     htmltools::tags$div(title = "Showing every monitoring plot in the network. Change the inputs below to view the filtered data", shiny::uiOutput("mapModeIndicator")),
                                     shiny::hr(),
                                     htmltools::tags$div(title="Filter by park so only species in a given park are listed", shiny::uiOutput("MapParkControl")),
                                     htmltools::tags$div(title="Select the time period you want to work with", shiny::uiOutput("MapCycleControl")),
                                     htmltools::tags$div(title="Select the type of plant you want to work with", 
                                                         shiny::selectizeInput(inputId="MapGroup", label="Type of plant:", choices=PLANTTYPES, selected = "", 
                                                                               options = base::list(placeholder = "Select a plant type", onInitialize = base::I('function() { this.setValue(""); }')))),
                                     htmltools::tags$div(title="Select a species of plants to map", shiny::uiOutput("MapSpeciesControl")),
                                     htmltools::tags$div(title="Toggle between common and scientific names", shiny::checkboxInput(inputId="mapCommon", label="Display common names?", value=TRUE )),
                                     htmltools::tags$div(title="Select live or dead", shiny::selectizeInput(inputId="TreeStatus", label="Alive or dead",
                                                                                                            choices=base::c("Alive"='alive',"Dead" = 'snag',"All"='all'), selected = NULL,
                                                                                                            options = base::list(placeholder = "Select a tree status",
                                                                                                                                 onInitialize = base::I('function() { this.setValue(""); }')))),
                                     htmltools::tags$div(title="Select the type of data to map",shiny::uiOutput("PlantValueControl"))))),
                  
              
                  
                  # map
                  #### The Map, full width/height ####
                  htmltools::div(leaflet::leafletOutput("VegMap", height="100%"), style="position:absolute; top:0; left:0; right:0; bottom:0;"),
                  htmltools::tags$div(style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0; display: flex; align-items: center; justify-content: center;pointer-events: none; z-index: 9989;",
                    shiny::uiOutput("incompleteInputWarning"),
                    htmltools::tags$div(style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0; display: flex; align-items: flex-end; justify-content: center; padding-bottom: clamp(20px, 4vh, 40px);
                                                padding-left: clamp(100px, 20vw, 360px); padding-right: clamp(90px, 15vw, 150px); box-sizing: border-box; pointer-events: none; z-index: 9989;",
                                        htmltools::tags$div(style = "pointer-events: auto; max-width: min(600px, 100%);", 
                                                            # shiny::uiOutput("customMapNotification")
                                                            )))),
                htmltools::tags$div(id = "mapInfoOverlay", class = "info-popup-overlay"),
                htmltools::tags$div(id = "mapInfoBox", class = "info-popup-box",
                                    htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                    htmltools::includeHTML("www/AboutMap.html"))
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
          shiny::actionButton(inputId = "densResetData", label = "\u21ba Reset Data", class = "btn btn-default btn-block", style = "margin-bottom: 10px;"),
          htmltools::tags$div(title = "Select the park you want to work with", shiny::uiOutput(outputId = "densParkControl")),
          htmltools::tags$div(title = "Select the time period you want to work with", shiny::uiOutput("densCycleControl")),
          htmltools::tags$div(title = "Select the type of plant you want to work with", shiny::selectizeInput(inputId = "densGroup", label = "Type of plant:", choices = PLANTTYPES)),
          htmltools::tags$div(title = "Toggle between common and scientific names", shiny::checkboxInput(inputId = "densCommon", label = "Display common names?", value = TRUE)),
          htmltools::tags$div(title = "Toggle summary statistics on or off", shiny::checkboxInput(inputId = "plotlyText", label = "Display summary statistics?", value = FALSE)),
          htmltools::tags$div(title = "Graph the most common species, species you select, or all species observed",
                              shiny::radioButtons(
                                inputId = "densSpeciesType",
                                label = "Which species?",
                                choices = base::c("Most common species" = "Common", "Pick individual species" = "Pick", "All species combined" = "All"),
                                inline = FALSE)),
          shiny::uiOutput(outputId = "densSpeciesControl"),
          htmltools::tags$div(title = "Select the type of data to graph",shiny::uiOutput(outputId = "densValControl")),
          shiny::conditionalPanel(
            condition = "input.densPanel=='Graph'",
            shiny::hr(),
            shiny::actionButton(inputId = "densGraphButton", label = "Display Options", class = "btn btn-primary btn-block action-button options-toggle-btn", `data-target` = "densOptions")),
          shiny::conditionalPanel(
            condition = "input.densPanel=='Table'",
            shiny::hr(),
            shiny::downloadButton(outputId = "densTableDownload", label = "Save Table (.csv)", class = "btn btn-primary btn-block"))),
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
                inline = TRUE)),
            shiny::uiOutput(outputId = "CompareSelect")))),
      htmltools::tags$div(
        id = "toggle_dens",
        class = "sidebar-toggle-tab",
        title = "Close data control panel",
        htmltools::tags$span("\u00ab"))),
    shiny::column(
      width = 9,
      id = "densMain",
      class = "collapsible-main-panel",
      style = "position: relative;",
      htmltools::tags$div(class = "info-icon-btn", `data-target` = "densInfo",   title = "About this figure", "?"),
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
            shiny::conditionalPanel(
              condition = "input.densPark == null || input.densPark == '' || (input.densSpeciesType == 'Pick' && (input.densSpecies == null || input.densSpecies.length == 0))",
              htmltools::tags$div(
                style = "text-align:center; display:flex; flex-direction:column; align-items:center; padding:20px; color:#555; font-size:15px;",
                htmltools::tags$p("There are no results for this combination of choices. Please select a park, species, or plant type."),
                shiny::uiOutput("densGraphImage"))),
            shiny::conditionalPanel(
              condition = "!(input.densPark == null || input.densPark == '' || (input.densSpeciesType == 'Pick' && (input.densSpecies == null || input.densSpecies.length == 0)))",
              plotly::plotlyOutput(outputId = "DensPlotly", height = "auto")),
            shiny::uiOutput("DensLimitWarning")),
          htmltools::tags$div(id = "densOptionsOverlay", class = "info-popup-overlay"),
          htmltools::tags$div(id = "densOptionsBox", class = "info-popup-box",
                              htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                              htmltools::tags$div(
                                style = "display: flex; flex-wrap: wrap; align-items: center; justify-content: flex-start; gap: 12px; margin-bottom: 10px;",
                                shiny::h4("Display Options", style = "margin: 0;"),
                                shiny::actionButton(inputId = "densResetDisplay", label = "\u21ba Reset Display Options",
                                                    class = "btn btn-default btn-sm")),
                              shiny::flowLayout(
                                cellArgs = base::list(style = "width: 160px"),
                                shiny::selectizeInput("densBaseColor", "Base Data Color:", choices = COLORNAMES, selected = "blue", width = 150),
                                shiny::tags$div(
                                  shiny::selectizeInput("densCompareColor", "Comparison Data Color:", choices = COLORNAMES, selected = "red", width = 150),
                                  shiny::uiOutput("densCompareColorNotice")),
                                shiny::sliderInput("densErrorThickness", "Error Bar Thickness", min = 0.5, max = 5, value = 1.5, step = 0.5, width = 150),
                                shiny::tags$div(
                                  shiny::sliderInput("densFontSize", "Change Font Size", min = 6, max = 18, value = 12, step = 2, width = 150),
                                  shiny::uiOutput("densFontSizeNotice"))))),
        shiny::tabPanel(
          htmltools::tags$div(title = "See all data in a table", "Data table"),
          value = "Table",
          htmltools::tags$div(style = "padding: 5px",
            style = "padding: 5px",
            shiny::br(),
            shiny::uiOutput("densReportTable"),
            shiny::h3(shiny::textOutput("densTableTitle")),
            shiny::hr(),
            shiny::uiOutput("densMissingWarningTable"),
            shiny::uiOutput("densOnePlotWarningTable"),
            shiny::conditionalPanel(
              condition = "input.densPark == null || input.densPark == '' || (input.densSpeciesType == 'Pick' && (input.densSpecies == null || input.densSpecies.length == 0))",
              htmltools::tags$div(
                style = "text-align:center; display:flex; flex-direction:column; align-items:center; padding:20px; color:#555; font-size:15px;",
                htmltools::tags$p("There are no results for this combination of choices. Please select a park, species, or plant type."),
                shiny::uiOutput("densTableImage"))),
            shiny::conditionalPanel(
              condition = "!(input.densPark == null || input.densPark == '' || (input.densSpeciesType == 'Pick' && (input.densSpecies == null || input.densSpecies.length == 0)))",
              DT::dataTableOutput("densTable")))))))
), # end of dens()

###############Time Series Plot
shiny::tabPanel(
  htmltools::tags$div(title="Graph data across monitoring cycles", "Data Trends Over Time"),
  shiny::fluidRow(
    
    # side Panel
    shiny::column(
      width = 3,
      id = "tsSidebar",
      class = "sidebar-col-outer",
      htmltools::tags$div(
        id = "tsSlideWrap",
        class = "sidebar-slide-wrap",
        shiny::wellPanel(class="panel panel-default controls", shiny::h4("Data:", class="panel-heading"),
                         shiny::actionButton(inputId = "tsResetData", label = "\u21ba Reset Data",
                                             class = "btn btn-default btn-block", style = "margin-bottom: 10px;"),
                         htmltools::tags$div(title="Select one or more parks to display. At least one park is required.", shiny::uiOutput(outputId="tsParkControl")),
                         htmltools::tags$div(title="Select the type of plant you want to work with", shiny::selectizeInput(inputId="tsGroup", label="Type of plant:", choices=PLANTTYPES)),
                         htmltools::tags$div(title="Toggle between common and scientific names", shiny::checkboxInput(inputId="tsCommon", label="Display common names?", value=TRUE)),
                         htmltools::tags$div(title="Toggle confidence interval ribbons on or off", shiny::checkboxInput(inputId = "tsShowCI", label = "Show 95% confidence intervals", value = FALSE)),
                         htmltools::tags$div(title="Graph the most common species, species you select, or all species observed", 
                                             shiny::radioButtons(inputId="tsSpeciesType", label="Which species?", 
                                                                 choices=base::c("Most common species"="Common", "Pick individual species"="Pick", "All species combined"="All"), inline=FALSE)),
                         htmltools::tags$div(title="Select the measurement to display on the y-axis", shiny::uiOutput(outputId="tsValControl")),
                         shiny::uiOutput(outputId="tsSpeciesControl"),
                         htmltools::tags$div(title = "Select the range of monitoring cycles to display", shiny::uiOutput(outputId = "tsCycleControl")),
                         shiny::conditionalPanel(condition="input.tsPanel=='Graph'", shiny::hr(), shiny::actionButton(inputId="tsGraphButton", label="Display Options", class="btn btn-primary btn-block action-button options-toggle-btn", `data-target` = "tsOptions")),
                         shiny::conditionalPanel(condition="input.tsPanel=='Table'", shiny::hr(),
                                                 shiny::radioButtons(inputId = "tsTableOrder", label = "Order table by:", 
                                                                     choices = base::c("Species" = "species", "Cycle" = "cycle"),
                                                                     selected = "species", inline = TRUE),
                                                 shiny::downloadButton(outputId="tsTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block")))),
      htmltools::tags$div(
        id = "toggle_ts",
        class = "sidebar-toggle-tab",
        title = "Close data control panel",
        htmltools::tags$span("\u00ab"))
    ), # close side column
    
    # main Panel
    shiny::column(9, id = "tsMain", class = "collapsible-main-panel", style = "position: relative;",
                  htmltools::tags$div(class = "info-icon-btn", `data-target` = "tsInfo", title = "About this figure", "?"),
                  htmltools::tags$div(id = "tsInfoOverlay", class = "info-popup-overlay"),
                  htmltools::tags$div(id = "tsInfoBox", class = "info-popup-box",
                                      htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                      htmltools::includeHTML("www/AboutTS.html")),
                  shiny::tabsetPanel(id="tsPanel", type="pills",
                                     
                                     # graph tab
                                     shiny::tabPanel(title=htmltools::tags$div(title="Graph the data", "Graph"), value="Graph",
                                                     htmltools::tags$div(id="tsPlotContainer", title="Time series of mean and 95% confidence interval by monitoring cycle",
                                                                         shiny::br(),
                                                                         shiny::uiOutput("tsReport"),
                                                                         shiny::uiOutput("tsMissingWarning"),
                                                                         shiny::uiOutput("tsSinglePlotWarning"),
                                                                         shiny::conditionalPanel(
                                                                           condition = "input.tsPark == null || input.tsPark == '' || (input.tsSpeciesType == 'Pick' && (input.tsSpecies == null || input.tsSpecies.length == 0))",
                                                                           htmltools::tags$div(
                                                                             style = "text-align:center; display:flex; flex-direction:column; align-items:center; padding:20px; color:#555; font-size:15px;",
                                                                             htmltools::tags$p("There are no results for this combination of choices. Please select a park, species, or plant type."),
                                                                             shiny::uiOutput("tsGraphImage"))),
                                                                         shiny::conditionalPanel(
                                                                           condition = "!(input.tsPark == null || input.tsPark == '' || (input.tsSpeciesType == 'Pick' && (input.tsSpecies == null || input.tsSpecies.length == 0)))",
                                                                           plotly::plotlyOutput(outputId="tsPlot", height="auto")),
                                                                         shiny::uiOutput("TSLimitWarning")),

                                                     # floating display options panel
                                                     htmltools::tags$div(id = "tsOptionsOverlay", class = "info-popup-overlay"),
                                                     htmltools::tags$div(id = "tsOptionsBox", class = "info-popup-box",
                                                                         htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                                                         htmltools::tags$div(
                                                                           style = "display: flex; flex-wrap: wrap; align-items: center; justify-content: flex-start; gap: 12px; margin-bottom: 10px;",
                                                                           shiny::h4("Display Options", style = "margin: 0;"),
                                                                           shiny::actionButton(inputId = "tsResetDisplay", label = "\u21ba Reset Display Options",
                                                                                               class = "btn btn-default btn-sm")),
                                                                         shiny::flowLayout(cellArgs=base::list(style="width: 160px"),
                                                                                           shiny::sliderInput("tsLineThickness", "Line Thickness", min=0.5, max=5, value=1.5, step=0.5, width=150),
                                                                                           shiny::tags$div(
                                                                                             shiny::sliderInput("tsFontSize", "Font Size", min=6, max=18, value=12, step=2, width=150),
                                                                                             shiny::uiOutput("tsFontSizeNotice")),
                                                                                           shiny::sliderInput("tsRibbonOpacity", "CI Ribbon Opacity", min=0.1, max=0.5, value=0.2, step=0.05, width=150),
                                                                                           shiny::selectizeInput("tsColorPalette", "Color Palette:",
                                                                                                                 choices=base::c("Bright" = "set1",
                                                                                                                                 "Pastel" = "set2",
                                                                                                                                 "Dark" = "dark2",
                                                                                                                                 "Paired" = "paired"),
                                                                                                                 selected = "set1", width=150)))
                                     ), # close graph tabPanel
                                     
                                     # table tab
                                     shiny::tabPanel(title = htmltools::tags$div("Data table"), value = "Table",
                                                     htmltools::tags$div(style = "padding: 5px",
                                                                         shiny::br(),
                                                                         shiny::uiOutput("tsReportTable"),
                                                                         shiny::h3(shiny::textOutput("tsTableTitle")),
                                                                         shiny::hr(),
                                                                         shiny::uiOutput("tsMissingWarningTable"),
                                                                         shiny::uiOutput("tsSinglePlotWarningTable"),
                                                                         shiny::conditionalPanel(
                                                                           condition = "input.tsPark == null || input.tsPark == '' || (input.tsSpeciesType == 'Pick' && (input.tsSpecies == null || input.tsSpecies.length == 0))",
                                                                           htmltools::tags$div(
                                                                             style = "text-align:center; display:flex; flex-direction:column; align-items:center; padding:20px; color:#555; font-size:15px;",
                                                                             htmltools::tags$p("There are no results for this combination of choices. Please select a park, species, or plant type."),
                                                                             shiny::uiOutput("tsTableImage"))),
                                                                         shiny::conditionalPanel(
                                                                           condition = "!(input.tsPark == null || input.tsPark == '' || (input.tsSpeciesType == 'Pick' && (input.tsSpecies == null || input.tsSpecies.length == 0)))",
                                                                           DT::dataTableOutput("tsTable"))))
                                     
                  ) # close tabsetpanel
    ) # close main column
  ) # close fluidrow
), # close ts

###############IV Plots
shiny::tabPanel(htmltools::tags$div(title="Graph Importance Values", "Importance Values (IV)"),
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
                                       shiny::actionButton(inputId = "IVResetData", label = "\u21ba Reset Data",
                                                           class = "btn btn-default btn-block", style = "margin-bottom: 10px;"),
                                       htmltools::tags$div(title="Select the park you want to work with",shiny::uiOutput("IVParkControl")),
                                       htmltools::tags$div(title="Select the time period you want to work with", shiny::uiOutput("IVCycleControl")),
                                       htmltools::tags$div(title="Select the type of plant you want to work with", shiny::selectizeInput(inputId="IVGroup", label="Type of plant:",choices=IVPLANTTYPES)),
                                       htmltools::tags$div(title="Toggle between common and scientific names", shiny::checkboxInput(inputId="IVCommon", label="Display common names?", value=TRUE)),
                                       htmltools::tags$div(title="Toggle importance values on or off", shiny::checkboxInput(inputId="IVPlotlyText", label="Display importance values?", value=FALSE)),
                                       htmltools::tags$div(title="Display density, size and disbribution separately", shiny::checkboxInput(inputId="IVPart", label="Display components of the importance value?", value=FALSE)),
                                       htmltools::tags$div(title="Graph the most common species, species you select, or all species observed", 
                                                           shiny::radioButtons(inputId="IVSpeciesType", label="Which species?", 
                                                                               choices=base::c("Most common species"="Common","Pick individual species"="Pick", "All species combined"="All"), inline=FALSE)),
                                       shiny::uiOutput(outputId="IVSpeciesControl"),
                                       shiny::conditionalPanel(
                                         condition="input.IVPanel=='Graph'",
                                         shiny::hr(),
                                         shiny::actionButton(inputId="IVGraphButton", label="Display Options", class="btn btn-primary btn-block action-button options-toggle-btn", `data-target` = "ivOptions")),
                                       shiny::conditionalPanel(
                                         condition="input.IVPanel=='Table'",
                                         shiny::hr(),
                                         shiny::downloadButton(outputId="IVTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block")))),
                    htmltools::tags$div(
                      id = "toggle_iv",
                      class = "sidebar-toggle-tab",
                      title = "Close data control panel",
                      htmltools::tags$span("\u00ab"))),
                  shiny::column(9, id = "ivMain", class = "collapsible-main-panel", style = "position: relative;",
                                htmltools::tags$div(class = "info-icon-btn", `data-target` = "ivInfo", title = "About this figure", "?"),
                                htmltools::tags$div(id = "ivInfoOverlay", class = "info-popup-overlay"),
                                htmltools::tags$div(id = "ivInfoBox", class = "info-popup-box",
                                                    htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                                    htmltools::includeHTML("www/IVPlot.html")),
                                shiny::tabsetPanel(id="IVPanel",type="pills",
                                                   shiny::tabPanel(value="Graph",
                                                                   htmltools::tags$div(title="Graph the data","Graph"),
                                                                   htmltools::tags$div(id = "ivPlotContainer", title="Graph of IV",
                                                                                       shiny::br(),
                                                                                       shiny::uiOutput("ivReport"),
                                                                                       shiny::conditionalPanel(
                                                                                         condition = "input.IVPark == null || input.IVPark == '' || (input.IVSpeciesType == 'Pick' && (input.IVSpecies == null || input.IVSpecies.length == 0))",
                                                                                         htmltools::tags$div(
                                                                                           style = "text-align:center; display:flex; flex-direction:column; align-items:center; padding:20px; color:#555; font-size:15px;",
                                                                                           htmltools::tags$p("There are no results for this combination of choices. Please select a park, species, or plant type."),
                                                                                           shiny::uiOutput("ivGraphImage"))),
                                                                                       shiny::conditionalPanel(
                                                                                         condition = "!(input.IVPark == null || input.IVPark == '' || (input.IVSpeciesType == 'Pick' && (input.IVSpecies == null || input.IVSpecies.length == 0)))",
                                                                                         plotly::plotlyOutput("IVPlot",height="auto")),
                                                                                       shiny::uiOutput("IVLimitWarning")),
                                                                   
                                                                   htmltools::tags$div(id = "ivOptionsOverlay", class = "info-popup-overlay"),
                                                                   htmltools::tags$div(id = "ivOptionsBox", class = "info-popup-box",
                                                                                       htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                                                                       htmltools::tags$div(
                                                                                         style = "display: flex; flex-wrap: wrap; align-items: center; justify-content: flex-start; gap: 12px; margin-bottom: 10px;",
                                                                                         shiny::h4("Display Options", style = "margin: 0;"),
                                                                                         shiny::actionButton(inputId = "IVResetDisplay", label = "\u21ba Reset Display Options",
                                                                                                             class = "btn btn-default btn-sm")),
                                                                                       shiny::tags$div(
                                                                                         style = "display: flex; flex-direction: column; align-items: center; gap: 16px;",
                                                                                         # row 1: base color + font size
                                                                                         shiny::tags$div(
                                                                                           style = "display: flex; flex-wrap: wrap; gap: 15px; justify-content: center;",
                                                                                           shiny::tags$div(
                                                                                             shiny::selectizeInput("IVBaseColor","Base Color:",choices=COLORNAMES, selected="green4",width="125px")),
                                                                                           shiny::tags$div(
                                                                                             shiny::sliderInput("IVFontSize", "Change Font Size", min=6, max=18, value=12, step=2,width="175px"),
                                                                                             shiny::uiOutput("IVFontSizeNotice"))),
                                                                                         # row 2: component colors + shared notice
                                                                                         shiny::tags$div(
                                                                                           style = "display: flex; flex-direction: column; align-items: center; gap: 6px;",
                                                                                           shiny::tags$div(
                                                                                             style = "display: flex; flex-wrap: wrap; gap: 15px; justify-content: center;",
                                                                                             shiny::selectizeInput("IVDensityColor","Density Color:",choices=COLORNAMES,
                                                                                                                   selected = if ("green4" %in% COLORNAMES) "green4" else COLORNAMES[[1]],
                                                                                                                   width="125px"),
                                                                                             shiny::selectizeInput("IVSizeColor","Size Color:",choices=COLORNAMES,
                                                                                                                   selected = if ("chartreuse" %in% COLORNAMES) "chartreuse" else COLORNAMES[[1]],
                                                                                                                   width="125px"),
                                                                                             shiny::selectizeInput("IVDistributionColor","Distribution Color:",choices=COLORNAMES,
                                                                                                                   selected = if ("yellow" %in% COLORNAMES) "yellow" else COLORNAMES[[1]],
                                                                                                                   width="125px")),
                                                                                           shiny::tags$div(
                                                                                             style = "text-align: center;",
                                                                                             shiny::uiOutput("IVComponentColorNotice")))))),
                                                   shiny::tabPanel(htmltools::tags$div(title="See all data in a table","Data table"),
                                                                   value="Table",
                                                                   htmltools::tags$div(style = "padding: 5px", 
                                                                                       shiny::br(),
                                                                                       shiny::uiOutput("ivReportTable"),
                                                                                       shiny::h3(shiny::textOutput("IVTableTitle")),
                                                                                       shiny::hr(),
                                                                                       shiny::conditionalPanel(
                                                                                         condition = "input.IVPark == null || input.IVPark == '' || (input.IVSpeciesType == 'Pick' && (input.IVSpecies == null || input.IVSpecies.length == 0))",
                                                                                         htmltools::tags$div(
                                                                                           style = "text-align:center; display:flex; flex-direction:column; align-items:center; padding:20px; color:#555; font-size:15px;",
                                                                                           htmltools::tags$p("There are no results for this combination of choices. Please select a park, species, or plant type."),
                                                                                           shiny::uiOutput("ivTableImage"))),
                                                                                       shiny::conditionalPanel(
                                                                                         condition = "!(input.IVPark == null || input.IVPark == '' || (input.IVSpeciesType == 'Pick' && (input.IVSpecies == null || input.IVSpecies.length == 0)))",
                                                                                         DT::dataTableOutput("IVData"))
                                                                   )
                                                   ) # close data table
                                ) # close tabsetpanel
                  ) # close main panel
                ) # close fluidrow
), # close iv

############################## Species Lists
shiny::tabPanel(id="SpeciesPanel",
                htmltools::tags$div(title="Lists of plants found in the parks", "Species Lists"),
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
                                                             choices=base::c("Vascular plants in the monitorng plots"= "Monitoring", "All vascular plants known from the park"="NPSpecies"))),
                                       htmltools::tags$div(title="Select a park to work with",shiny::uiOutput("SpListParkControl")),
                                       shiny::conditionalPanel(condition="input.SpListType=='Monitoring'",
                                                               htmltools::tags$div(title="Select one or more plots, select and backspace to delete.", shiny::uiOutput("SpListPlotControl"))),
                                       shiny::conditionalPanel(
                                         condition = "output.hasSpPark",
                                         shiny::hr(),
                                         shiny::downloadButton(outputId="SpeciesTableDownload", label="Save Table (.csv)", class="btn btn-primary btn-block")))),
                    htmltools::tags$div(
                      id = "toggle_sp",
                      class = "sidebar-toggle-tab",
                      title = "Close data control panel",
                      htmltools::tags$span("\u00ab"))),
                  shiny::column(9, id = "spMain", class = "collapsible-main-panel", style = "position: relative;",
                                shiny::br(), shiny::br(),
                                htmltools::tags$div(class = "info-icon-btn", `data-target` = "spInfo", title = "About this figure", "?"),
                                htmltools::tags$div(id = "spInfoOverlay", class = "info-popup-overlay"),
                                htmltools::tags$div(id = "spInfoBox", class = "info-popup-box",
                                                    htmltools::tags$button(class = "info-popup-close", "\u00d7"),
                                                    htmltools::includeHTML("www/AboutLists.html")),
                                shiny::h3(shiny::textOutput("SpeciesTableTitle")),
                                shiny::uiOutput("NPSpeciesLink"),
                                shiny::hr(),
                                shiny::conditionalPanel(
                                  condition = "!output.hasSpPark",
                                  htmltools::tags$div(
                                    style = "text-align:center; display:flex; flex-direction:column; align-items:center; padding:20px; color:#555; font-size:15px;",
                                    htmltools::tags$p("Please select a park to view its vascular species data."),
                                    shiny::uiOutput("spTableImage"))),
                                shiny::conditionalPanel(
                                  condition = "output.hasSpPark",
                                  DT::dataTableOutput("SpeciesTable"))
                                        
                  ) # close main panel
                ) # close fluidrow
), # close sp list

##################### About
    tabPanel(
      tags$div(title="About the project", "About"),
      tags$base(target="_blank"),
      includeHTML("www/AboutTab.html")
      )# close about 

)#end shiny::navbarPage()

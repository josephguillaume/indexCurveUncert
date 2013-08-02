library(shiny)
library(indexCurveUncert)

shinyUI(pageWithSidebar(
                        headerPanel('Ecological suitability indices',
                                    singleton(
                                              tags$head(
                                                        tags$link(rel = 'stylesheet',
                                                                  type = 'text/css',
                                                                  href = 'tableinput/tableinput.css'),
                                                        tags$script(src = 'tableinput/tableinput.js'),
                                                        tags$script(HTML("
//TODO: generalise
//jQuery(function($) {
$( document ).ready(function(){
$(\".control-label[for='assets_shown']\").click(function ( event ) {
    event.preventDefault();
      $(\".control-label[for='assets_shown']~*\").toggle();
if ($(\".control-label[for='assets_shown']~*\").css('display') == 'none') {
$(\".control-label[for='assets_shown']\").html($(\".control-label[for='assets_shown']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='assets_shown']\").html($(\".control-label[for='assets_shown']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //assets_shown

$(\".control-label[for='species_shown']\").click(function ( event ) {
    event.preventDefault();
      $(\".control-label[for='species_shown']~*\").toggle();
if ($(\".control-label[for='species_shown']~*\").css('display') == 'none') {
$(\".control-label[for='species_shown']\").html($(\".control-label[for='species_shown']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='species_shown']\").html($(\".control-label[for='species_shown']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //species_shown

$(\".control-label[for='attrib_shown']\").click(function ( event ) {
    event.preventDefault();
      $(\".control-label[for='attrib_shown']~*\").toggle();
if ($(\".control-label[for='attrib_shown']~*\").css('display') == 'none') {
$(\".control-label[for='attrib_shown']\").html($(\".control-label[for='attrib_shown']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='attrib_shown']\").html($(\".control-label[for='attrib_shown']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //attrib_shown


$(\".control-label[for='use_duration']\").click(function ( event ) {
    event.preventDefault();
      $(\".control-label[for='use_duration']~*\").toggle();
if ($(\".control-label[for='use_duration']~*\").css('display') == 'none') {
$(\".control-label[for='use_duration']\").html($(\".control-label[for='use_duration']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='use_duration']\").html($(\".control-label[for='use_duration']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //use_duration


$(\".control-label[for='attribs']\").click(function ( event ) {
    event.preventDefault();
      $(\".control-label[for='attribs']~*\").toggle();
if ($(\".control-label[for='attribs']~*\").css('display') == 'none') {
$(\".control-label[for='attribs']\").html($(\".control-label[for='attribs']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='attribs']\").html($(\".control-label[for='attribs']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //attribs

$(\".control-label[for='pref_bounds']\").click(function(event) {
    event.preventDefault();
$(\".control-label[for='pref_bounds']~*\").toggle()
$(\".control-label[for='pref_bounds'] > .tableinput-buttons\").toggle()
if ($(\".control-label[for='pref_bounds']~*\").css('display') == 'none') {
$(\".control-label[for='pref_bounds']\").html($(\".control-label[for='pref_bounds']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='pref_bounds']\").html($(\".control-label[for='pref_bounds']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
}); //pref_bounds

$(\".control-label[for='pref_monoton']\").click(function(event) {
    event.preventDefault();
$(\".control-label[for='pref_monoton']~*\").toggle()
$(\".control-label[for='pref_monoton'] > .tableinput-buttons\").toggle()
if ($(\".control-label[for='pref_monoton']~*\").css('display') == 'none') {
$(\".control-label[for='pref_monoton']\").html($(\".control-label[for='pref_monoton']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='pref_monoton']\").html($(\".control-label[for='pref_monoton']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
}); //pref_monoton


$(\".control-label[for='pref_smooth']\").click(function(event) {
    event.preventDefault();
$(\".control-label[for='pref_smooth']~*\").toggle()
$(\".control-label[for='pref_smooth'] > .tableinput-buttons\").toggle()
if ($(\".control-label[for='pref_smooth']~*\").css('display') == 'none') {
$(\".control-label[for='pref_smooth']\").html($(\".control-label[for='pref_smooth']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\".control-label[for='pref_smooth']\").html($(\".control-label[for='pref_smooth']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
}); //pref_smooth


$('#toggle_all_attribs').on('click',function(evt){
 $('#attribs input:checkbox').attr('checked', ! $('#attribs input:checkbox').attr('checked'));
 $('#attribs input:checkbox').trigger('change');
});

$('#toggle_all_ctf_assets').on('click',function(evt){
 $('#ctf_assets input:checkbox').attr('checked', ! $('#ctf_assets input:checkbox').attr('checked'));
 $('#ctf_assets input:checkbox').trigger('change');
});

$('#toggle_all_ctf_species').on('click',function(evt){
 $('#ctf_species input:checkbox').attr('checked', ! $('#ctf_species input:checkbox').attr('checked'));
 $('#ctf_species input:checkbox').trigger('change');
});


$(\"label[for='ctf_shown']\").click(function ( event ) {
    event.preventDefault();
      $(\"#ctf_shown\").toggle();
if ($(\"#ctf_shown\").css('display') == 'none') {
$(\"label[for='ctf_shown']\").html($(\"label[for='ctf_shown']\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\"label[for='ctf_shown']\").html($(\"label[for='ctf_shown']\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //ctf_shown

$('#label_edit_species').click(function ( event ) {
    event.preventDefault();
      $(\"#label_edit_species~*\").toggle();
if ($(\"#label_edit_species~*\").css('display') == 'none') {
$(\"#label_edit_species\").html($(\"#label_edit_species\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\"#label_edit_species\").html($(\"#label_edit_species\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //label_edit_species

$('#label_edit_attribs').click(function ( event ) {
    event.preventDefault();
      $(\"#label_edit_attribs~*\").toggle();
if ($(\"#label_edit_attribs~*\").css('display') == 'none') {
$(\"#label_edit_attribs\").html($(\"#label_edit_attribs\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\"#label_edit_attribs\").html($(\"#label_edit_attribs\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //label_edit_attribs


$('#label_edit_assets').click(function ( event ) {
    event.preventDefault();
      $(\"#label_edit_assets~*\").toggle();
if ($(\"#label_edit_assets~*\").css('display') == 'none') {
$(\"#label_edit_assets\").html($(\"#label_edit_assets\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\"#label_edit_assets\").html($(\"#label_edit_assets\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //label_edit_assets


////////////////////////////////////////////////////////////////////////////////
$('#label_inputs').click(function ( event ) {
    event.preventDefault();
      $(\"#label_inputs~*\").toggle();
if ($(\"#label_inputs~*\").css('display') == 'none') {
$(\"#label_inputs\").html($(\"#label_inputs\").html().replace(\"<b>-</b>\",\"<b>+</b>\"));
} else {
$(\"#label_inputs\").html($(\"#label_inputs\").html().replace(\"<b>+</b>\",\"<b>-</b>\"));
}
    }); //label_inputs


//https://github.com/twitter/bootstrap/issues/2764
//http://stackoverflow.com/questions/14484445/disabling-tabs-in-bootstrap
$('ul.nav > li').click(function () {
   if ($(this).hasClass('disabled')) {
        return false;
    }
});

//http://stackoverflow.com/questions/11093852/fading-out-everything-but-this-while-honoring-a-click
function disable_input(evt){
$(\".well > :not(#pref_edit,#weight_edit)\").filter(':visible').fadeTo('fast',0.3)
$(\".tabbable\").fadeTo('fast',0.3)
  $(\"#assets_shown input:radio\").attr('disabled',true);
 $(\"#attrib_shown  input:radio\").attr('disabled',true);
 $(\"#species_shown  input:radio\").attr('disabled',true);
 $(\"#use_duration input:checkbox\").attr('disabled',true);
 $('#attribs input:checkbox').attr('disabled',true);
$(\"#ctf_shown\").attr('disabled',true);
$('#tabs > li').addClass('disabled')
};

function enable_input(evt){
 $('.well > :not(#pref_edit,#weight_edit)').filter(':visible').fadeTo('fast',1)
 $('.tabbable').fadeTo('fast',1)
 $(\"#assets_shown input:radio\").attr('disabled',false);
 $(\"#attrib_shown  input:radio\").attr('disabled',false);
 $(\"#species_shown  input:radio\").attr('disabled',false);
 $(\"#use_duration input:checkbox\").attr('disabled',false);
 $('#attribs input:checkbox').attr('disabled',false);
 $(\"#ctf_shown\").attr('disabled',false);
 $('#tabs > li').removeClass('disabled')
};

$('#pref_bounds').on('change.tableinput','#pref_bounds tbody',disable_input);
$('#pref_monoton').on('change.tableinput','#pref_monoton tbody',disable_input);
$('#pref_smooth').on('change.tableinput','#pref_smooth tbody',disable_input);
$('#weight_bounds').on('change.tableinput','#weight_bounds tbody',disable_input);
$('#weight_comp').on('change.tableinput','#weight_comp tbody',disable_input);


  $(document).on('click', '#btn_update_pref', enable_input);
  $(document).on('click', '#btn_discard_pref', enable_input);
  $(document).on('click', '#btn_update_weights', enable_input);
  $(document).on('click', '#btn_discard_weights', enable_input);

//http://www.highcharts.com/demo/dynamic-click-to-add
  $('#breakpoint_plot > img').click(function(e) {
    var offset = $(this).offset();
    alert(e.clientX - offset.left+\" \"+e.clientY - offset.top);
  });

//TODO: make it easier to reload same file. Currently need to dummy load to reset form
// $('#file1').val('') //doesn't reset complete.

//TODO: want this input to be hidden
//$('#current_weight_run').toggle();
$(\"label[for='which_weight_run']\").on('click',function(evt){
 $('#current_weight_run').toggle();
});

$('#download_prefs').click(function (e) {
   if ($(this).attr('disabled')) {
       return false;
    }
});

////////////////////////////////////////////////////////////////////////////////
function enable_species_input(evt) {
// $('#add_species').val('');
// $('#add_species').change();
$('.tab-pane.active > :not(#panel_edit_species)').filter(':visible').fadeTo('fast',1);
$('.span4').fadeTo('fast',1);
  $('#file1').attr('disabled',false);
  $('#download_prefs').attr('disabled',false);
$('#tabs > li').removeClass('disabled');
};

function disable_species_input(evt){
$('.tab-pane.active > :not(#panel_edit_species)').filter(':visible').fadeTo('fast',0.3);
$('.span4').fadeTo('fast',0.3);
  $('#file1').attr('disabled',true);
  $('#download_prefs').attr('disabled',true);
$('#tabs > li').addClass('disabled');
};

$('#btn_update_species').on('click',enable_species_input);
$('#btn_discard_species').on('click',enable_species_input);
//TODO: should really be on select even if not click
$('#delete_species').on('click',disable_species_input);
$('#add_species').on('focus',disable_species_input);

////////////////////////////////////////////////////////////////////////////////

function enable_assets_input(evt) {
$('.tab-pane.active > :not(#panel_edit_assets)').filter(':visible').fadeTo('fast',1);
$('.span4').fadeTo('fast',1);
  $('#file1').attr('disabled',false);
  $('#download_prefs').attr('disabled',false);
$('#tabs > li').removeClass('disabled');
};

function disable_assets_input(evt){
//TODO: disable other matrix inputs
$('.tab-pane.active > :not(#panel_edit_assets)').filter(':visible').fadeTo('fast',0.3);
$('.span4').fadeTo('fast',0.3);
  $('#file1').attr('disabled',true);
  $('#download_prefs').attr('disabled',true);
$('#tabs > li').addClass('disabled');
};

$('#btn_update_assets').on('click',enable_assets_input);
$('#btn_discard_assets').on('click',enable_assets_input);
$('#matrix_assets').on('change.tableinput','#matrix_assets tbody',disable_assets_input);

////////////////////////////////////////////////////////////////////////////////
function enable_bkpt_input(evt) {
//TODO: also disable scenario choosers
//TODO: race condition with save - either follow on to existing call or piggy back receive of sendInputMessage on table
// $('#bkpt_from').val('');
// $('#bkpt_from').change();
// $('#bkpt_to').val('');
// $('#bkpt_to').change();
// $('#bkpt_by').val('');
// $('#bkpt_by').change();
$('.tab-pane.active > :not(#panel_edit_bkpt)').filter(':visible').fadeTo('fast',1);
$('.span4').fadeTo('fast',1);
$('#tabs > li').removeClass('disabled');
 $(\"#assets_shown input:radio\").attr('disabled',false);
 $(\"#attrib_shown  input:radio\").attr('disabled',false);
 $(\"#species_shown  input:radio\").attr('disabled',false);
 $(\"#use_duration input:checkbox\").attr('disabled',false);
 $('#attribs input:checkbox').attr('disabled',false);
 $(\"#ctf_shown\").attr('disabled',false);
};

function disable_bkpt_input(evt){
$('.tab-pane.active > :not(#panel_edit_bkpt)').filter(':visible').fadeTo('fast',0.3);
$('.span4').fadeTo('fast',0.3);
$('#tabs > li').addClass('disabled');
 $(\"#assets_shown input:radio\").attr('disabled',true);
 $(\"#attrib_shown  input:radio\").attr('disabled',true);
 $(\"#species_shown  input:radio\").attr('disabled',true);
 $(\"#use_duration input:checkbox\").attr('disabled',true);
 $('#attribs input:checkbox').attr('disabled',true);
 $(\"#ctf_shown\").attr('disabled',true);
};

$('#btn_update_bkpts').on('click',enable_bkpt_input);
$('#btn_discard_bkpts').on('click',enable_bkpt_input);
$('#bkpt_from').on('click',disable_bkpt_input);
$('#bkpt_to').on('click',disable_bkpt_input);
$('#bkpt_by').on('click',disable_bkpt_input);
//$('#matrix_bkpts').on('change.tableinput',disable_bkpt_input); //FIXME: triggered by programmatic change?

$('#btn_clear_bkpt').on('click',function(evt){
 $('#bkpt_from').val('');
 $('#bkpt_from').change();
 $('#bkpt_to').val('');
 $('#bkpt_to').change();
 $('#bkpt_by').val('');
 $('#bkpt_by').change();
});
////////////////////////////////////////////////////////////////////////////////
});

"                                                                        )) #script
                                                        ) #head
                                              )           #singleton
                                    ),                    #headerPanel
                        sidebarPanel(
                                     ## TODO: modal dialog to select which side panels are shown?
                                     ## Editing of constraints always at top of sidebar
                                     conditionalPanel("input.tabs=='pref'",id="pref_edit",
                                                      strong(textOutput("pref_title")),
                                                      matrixInput("pref_bounds",
                                                                  HTML("<b>-</b> Bounds"),
                                                                  data.frame(min.x=NA,max.x=NA,min.y=NA,max.y=NA),types=rep("numeric",4)),
                                                      matrixInput("pref_monoton",
                                                                  HTML("<b>-</b> Up or down"),
                                                                  data.frame(min.x=NA,max.x=NA,dir=NA,min.step=NA),types=rep("numeric",4)),
                                                      matrixInput("pref_smooth",
                                                                  HTML("<b>-</b> Smoothness"),
                                                                  data.frame(min.x=NA,max.x=NA,min.step=NA,max.step=NA),types=rep("numeric",4)),
                                                      p(
                                                        actionButton("btn_update_pref","Update changes"),
                                                        actionButton("btn_discard_pref","Discard changes")
                                                        ) ##p
                                                      ),
                                     conditionalPanel("input.tabs=='weights'",id="weight_edit",
                                                      strong(textOutput("weight_title")),
                                                      matrixInput("weight_bounds",
                                                                  "Bounds",
                                                                  data.frame(attrib=NA,min.weight=NA,max.weight=NA,stringsAsFactors=FALSE),
                                                                  types=c("character","numeric","numeric")),
                                                      matrixInput("weight_comp",
                                                                  "Comparison",
                                                                  data.frame(attrib1=NA,attrib2=NA,dir=NA,min.gap=NA),
                                                                  types=c("character","character","character","numeric")),
                                                      p(
                                                        actionButton("btn_update_weights","Update changes"),
                                                        actionButton("btn_discard_weights","Discard changes")
                                                        )
                                                      ),
                                     ## Species and attributes most important (except in ctf, where assets more important)
                                     conditionalPanel("input.tabs=='pref' || input.tabs=='weights' || input.tabs=='bkpts' || input.tabs=='single_asset'",
                                                      strong("Model properties"),
                                                      radioButtons("species_shown",
                                                                   HTML("<b>-</b> Species for which to show preference curve/weights/breakpoints"),
                                                                   c("placeholder_species"),
                                                                   selected=NULL
                                                                   )
                                                      ),
                                     conditionalPanel("input.tabs=='pref' || input.tabs=='bkpts'",
                                                      radioButtons("attrib_shown",
                                                                   HTML("<b>-</b> Attribute for which to show preference curve/breakpoints"),
                                                                   c("placeholder_attribute"),
                                                                   selected=NULL
                                                                   )
                                                      ),
                                     conditionalPanel("input.tabs=='ctf'",
                                                      strong("Model runs"),
                                                      checkboxGroupInput("ctf_species",
                                                                         "Species",
                                                                         "Placeholder species",
                                                                         selected=NULL),
                                                      HTML("<a id='toggle_all_ctf_species'>Toggle all</a>"),
                                                      checkboxGroupInput("ctf_assets",
                                                                         "Assets",
                                                                         c(1)
                                                                         ),
                                                      HTML("<a id='toggle_all_ctf_assets'>Toggle all</a>")

                                                      ),
                                     conditionalPanel("input.tabs=='ctf' || input.tabs =='weights' ||  input.tabs=='single_asset'",
                                                      checkboxGroupInput("attribs",
                                                                         HTML("<b>-</b> Attributes to use in model"),
                                                                         c("Placeholder attribs"),
                                                                         selected=NULL),
                                                      HTML("<a id='toggle_all_attribs'>Toggle all</a>")
                                                      ),
                                     ## Assets and ctf less important than species and attribs
                                     conditionalPanel("input.tabs=='pref' || input.tabs=='weights' || input.tabs=='bkpts' || input.tabs=='single_asset'",
                                                      radioButtons("assets_shown",
                                                                   HTML("<b>-</b> Assets"),
                                                                   c(1)
                                                                   ),
                                                      numericInput("ctf_shown",
                                                                   HTML("<b>-</b> CTF"),
                                                                   0,100e3,value=1000)
                                                      ),
                                     ## Use duration 2nd least important
                                     conditionalPanel("input.tabs=='ctf' || input.tabs=='pref' || input.tabs=='weights'",
                                                      checkboxGroupInput("use_duration",
                                                                         HTML("<b>-</b> Use duration?"),
                                                                         c(T,F),
                                                                         selected=c(T,F)
                                                                         )),
                                     ## Input at bottom - less important for time being,
                                     ##  and everything else can be collapsed as needed
                                     div(
                                         strong(HTML("<b>-</b> Inputs"),id="label_inputs"),
                                         selectInput("baseline","Baseline",choices="",selected=NULL),
                                         selectInput("scenario","Scenario",choices="",selected=NULL)
                                         )
                                     ),
                        mainPanel(
                                  tabsetPanel(id="tabs",
                                              tabPanel("Load/save",value="load",id="tab_load",
                                                       fileInput('file1', 'Choose settings Rdata file',
                                                                 accept=""),
                                                       downloadButton('download_prefs', 'Download settings'),
                                                       p(),
                                                       wellPanel(id='panel_edit_assets',
                                                                 strong(HTML("<b>-</b> Edit asset table"),id="label_edit_assets"),
                                                                 matrixInput("matrix_assets",
                                                                             "Assets",
                                                                             data.frame(ID=NA,Gauge=NA,Name=NA,Event_threshold=NA),
                                                                             types=c("numeric","character","character","numeric")),
                                                                 p(
                                                                   actionButton("btn_update_assets","Update changes"),
                                                                   actionButton("btn_discard_assets","Discard changes")
                                                                   ) ##p
                                                                 ),
                                                       wellPanel(id="panel_edit_species",
                                                                 strong(HTML("<b>-</b> Edit species"),id="label_edit_species"),
                                                                 checkboxGroupInput("delete_species",
                                                                                    HTML("Check species to delete"),
                                                                                    "",selected=NULL
                                                                                    ),
                                                                 textInput("add_species","Species to add"),
                                                                 p(
                                                                   actionButton("btn_update_species","Update changes"),
                                                                   actionButton("btn_discard_species","Discard changes")
                                                                   ) ##p
                                                                 ),
                                                       wellPanel(id="panel_edit_attribs",
                                                                 strong(HTML("<b>-</b> Edit attributes"),id="label_edit_attribs"),
                                                                 matrixInput("matrix_attribs",
                                                                             "Attributes",
                                                                             data.frame(Attribute=NA,Duration=NA),types=c("character","character")),
                                                                 p(
                                                                   actionButton("btn_update_attribs","Update changes"),
                                                                   actionButton("btn_discard_attribs","Discard changes")
                                                                   ) ##p
                                                                 )
                                                       ),
                                              tabPanel("Breakpoints",value="bkpts",
                                                       h3("Setting breakpoints"),
                                                       plotOutput("breakpoint_plot"),
                                                       numericInput("range2","Range",0,25e3,value=1),
                                                       ##TODO: avoid duplicating?
                                                       wellPanel(id="panel_edit_bkpt",
                                                                 ## TODO: all on same line. display:inline-block not working
                                                                 numericInput("bkpt_from","From",0,25e3),
                                                                 numericInput("bkpt_to","To",0,25e3),
                                                                 numericInput("bkpt_by","by",0,25e3),
                                                                 HTML("<button id='btn_clear_bkpt' class='btn' type='button'>Clear</button>"),
                                                                 ##TODO: graph input instead
                                                                 matrixInput("matrix_bkpts",
                                                                             "Breakpoints",
                                                                             data.frame(Input=NA),types="numeric"),
                                                                 p(
                                                                   actionButton("btn_update_bkpts","Update changes"),
                                                                   actionButton("btn_discard_bkpts","Discard")
                                                                   ) ##p
                                                                 )
                                                       ),
                                              tabPanel("Preference curve",value="pref",
                                                       h3("Visualisation of preference curve"),
                                                       plotOutput(outputId='pref'),
                                                       ## TODO: use javascript plot with zooming instead
                                                       ## TODO: set default from server for selected radio button
                                                       ## TODO: set min as well as max
                                                       ##sliderInput("range","Range",0,25e2,value=25e2,step=1)
                                                       numericInput("range","Range",0,25e3,value=1),
                                                       verbatimTextOutput('pref_text')
                                                       ##tableOutput("pref_text")
                                                       ),
                                              tabPanel("Weights",value="weights",
                                                       h3("Visualisation of weights"),
                                                       tableOutput("weights"),
                                                       ## TODO: better to have line with tableOutput
                                                       textInput("current_weight_run",""),
                                                       selectInput("which_weight_run","Which run to plot",
                                                                   choices="",selected=NULL),
                                                       plotOutput("plot_weight_classes")
                                                       ),
                                              ## tabPanel("Asset result",value="single_asset",
                                              ##          h3("Results for single asset"),
                                              ##          ##plotOutput("traffic_asset")
                                              ##          ),
                                              tabPanel("CTF results",value="ctf",
                                                       h3("Results for ctf values"),
                                                       p(
                                                         actionButton("btn_update_traffic_ctf","Update CTF results")
                                                         ),
                                                       tabsetPanel(
                                                                   tabPanel("Traffic diagram",
                                                                            plotOutput("traffic_ctf"),
                                                                            radioButtons("treatment_zero",
                                                                                         HTML("<b>-</b> How is zero difference treated?"),
                                                                                         c("As scenario"="scen",
                                                                                           "As baseline"="base",
                                                                                           "As scenario or baseline"="both"
                                                                                           ),
                                                                                         selected="As scenario or baseline"
                                                                                         )
                                                                            ),
                                                                   tabPanel("Preference curves",
                                                                            p("Preference curves for first attribute selected"),
                                                                            plotOutput("pref_asset")
                                                                            )
                                                                   )
                                                       ),
                                              tabPanel("Debug",
                                                       tabsetPanel(
                                                                   tabPanel("Weights",verbatimTextOutput("print_weights")),
                                                                   tabPanel("Prefs",verbatimTextOutput("print_prefs")),
                                                                   tabPanel("Breakpts",verbatimTextOutput('print_bkpts')),
                                                                   tabPanel("Other",verbatimTextOutput('summary'))
                                                                   )
                                                       )
                                              )
                                  )
                        ))

## TODO: tab to set species, attribs, breakpoints
## TODO: update attrib radio buttons etc from tab

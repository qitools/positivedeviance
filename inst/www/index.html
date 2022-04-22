<!DOCTYPE html>
<html lang="en">
<head>
<title>QI Tools: Positive deviants</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

<!-- jquery -->
<script src="//code.jquery.com/jquery-2.1.3.min.js"> </script>

<!-- tooltip -->
<script type="text/javascript" src="qtip/jquery.qtip.min.js"></script>
<link rel="stylesheet" type="text/css" href="qtip/jquery.qtip.min.css" />

<!-- ocpu library -->
<script src="opencpu/opencpu-0.4.js"> </script>

<!-- some optional styling stuff -->
<!-- 
<link href="bootstrap/css/bootstrap.min.css" rel="stylesheet" media="screen">
-->
<link href="jqueryui/css/ui-lightness/jquery-ui-1.10.3.custom.css" rel="stylesheet" media="screen"> 

<script src="bootstrap/js/bootstrap.js"> </script>
<script src="jqueryui/jquery-ui-1.10.3.custom.js"> </script>

<!-- jquery-csv from http://code.google.com/p/jquery-csv/ -->
<!-- <script src="https://jquery-csv.googlecode.com/git/src/jquery.csv.js"></script> -->
<script src="jquery-csv/jquery.csv.js"></script>

<!-- http://jqueryui.com/dialog/ -->
<script src="https://code.jquery.com/ui/1.10.3/jquery-ui.js"></script>

<!-- Ajax.org Cloud9 Editor from http://ace.c9.io/#nav=api&api=editor -->
<script src="src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>

<script> 
$(document).ready(function(){

	//optional, requires jquery-ui.
	$("#plotdiv").resizable()

	if(isAPIAvailable()) {
	  $('#files').bind('change', handleFileSelect);
	}

  //For Ajax.org Cloud9 Editor
  var editor = ace.edit("editor");
  editor.setTheme("ace/theme/github");
  editor.getSession().setMode("ace/mode/r");
  editor.setFontSize("14px");
  editor.getSession().setUseWrapMode(true);
  
  // drawplot
  function drawplot(){
    $("#plotbutton").attr("disabled", "disabled")
  	temp = editor.getSession().getValue()
	//Three replacements below are not required on a local server, but are at https://public.opencpu.org/ocpu/github/
	//temp = temp.replace(/\r?\n/g, '')
	temp = temp.replace(/\s+$/g, '')
	temp = temp.replace(/$\s+/g, '')
	temp = temp.replace(/,$/g, '')

	var req = $('#plotdiv').rplot("positivedeviance", {
		content:        temp,
		topic :          $("#topic").val(),
		outcome_label :  $("#outcome_label").val(),
		subject_label :  $("#subject_label").val(),
		subgroup		:  $("#subgroup").val(),
		outcome_type :   $("#outcome_type").val(),
		benchmark_value : $("#benchmark_value").val(),
		benchmark_label : $("#benchmark_label").val(),
		threshold_observations : $("#threshold_observations").val(),
		threshold_value : $("#threshold_value").val(),
		data_type: $("#data_type").val(),
		output_type: $("#output_type").val(),
		x_min:           $("#x_min").val(),
		x_max:           $("#x_max").val(),
		theme:           $("#theme").val()
    }).always(function(){
      $("#plotbutton").removeAttr("disabled");
    }).fail(function(){
      alert("HTTP error " + req.status + ": " + req.responseText);
    });
  }

  // Click handlers
  $("#plotbutton").on("click", function(e){
    e.preventDefault();
    drawplot();
  });

  $("#addcommas").on("click", function(e){
    e.preventDefault();
    temp = editor.getValue()
	temp = temp.replace(/\n/g, ',\n')
	temp = temp.replace(/$\s+/g, '')
	temp = temp.replace(/\s+$/g, '')
	temp = temp.replace(/ +/g, ', ')
	temp = temp.replace(/\t/g, ', ')
	temp = temp + ","
	temp = temp.replace(/,,/g, ',')
    editor.setValue(temp,1);
  });	  

  //Examples	
    $(".example").click(function(){
	if($(this).val()=="ex_1") {$("#distribution_type").val("p");$("#totalencounters").css("display","none");;editor.setValue("1,1,12,30,\n2,1,8,25,\n3,1,5,20,\n4,1,3,15,\n5,2,6,14,\n6,2,5,13,\n7,2,6,12,\n8,2,5,12,\n10,3,4,11,\n11,3,5,11,\n12,3,2,11,\n13,4,5,11,\n14,4,3,9,\n15,4,2,8,\n16,4,3,8,\n17,5,3,7,\n18,5,1,6,\n19,6,1,4,\n20,6,4,7,\n21,6,2,3,",1);}
	if($(this).val()=="ex_2") {$("#distribution_type").val("p");$("#totalencounters").css("display","none");;editor.setValue("'Non-parsable', 7,\n'Refused (cost)', 4,\n'Refused (other)', 4,\n'No documentation', 8,\n'Not PCP encounter', 9",1);}
	if($(this).val()=="ex_3") {$("#distribution_type").val("b");$("#totalencounters").css("display","inline");$("#totalencounters").val(24);;editor.setValue("'Non-parsable', 7,\n'Refused (cost)', 4,\n'Refused (other)', 4,\n'No documentation', 8,\n'Not PCP encounter', 9",1);}
  });

  function isAPIAvailable() {
    // Check for the various File API support.
    if (window.File && window.FileReader && window.FileList && window.Blob) {
      // Great success! All the File APIs are supported.
      return true;
    } else {
      // source: File API availability - http://caniuse.com/#feat=fileapi
      // source: <output> availability - http://html5doctor.com/the-output-element/
      document.writeln('The HTML5 APIs used in this form are only available in the following browsers:<br />');
      // 6.0 File API & 13.0 <output>
      document.writeln(' - Google Chrome: 13.0 or later<br />');
      // 3.6 File API & 6.0 <output>
      document.writeln(' - Mozilla Firefox: 6.0 or later<br />');
      // 10.0 File API & 10.0 <output>
      document.writeln(' - Internet Explorer: Not supported (partial support expected in 10.0)<br />');
      // ? File API & 5.1 <output>
      document.writeln(' - Safari: Not supported<br />');
      // ? File API & 9.2 <output>
      document.writeln(' - Opera: Not supported');
      return false;
    }
  }

  function handleFileSelect(evt) {
	    var files = evt.target.files; // FileList object
	    var file = files[0];
	    // read the file contents
	    printTable(file);
	  }

	function printTable(file) {
 		var reader = new FileReader();
		reader.readAsText(file);
		reader.onload = function(event){
			var csv = event.target.result;
			var data = $.csv.toArrays(csv);
			var html = '';
			for(var row in data) {
				if($("#header").val() == "FALSE" || [row] > 0 ){
			for(var item in data[row]) {
				html += data[row][item] + ', ';
				}
			html += '\r\n';
			}
		}
		editor.setValue(html,1);
	};
	reader.onerror = function(){ alert('Unable to read ' + file.fileName); };
	}

  //init on start
  //drawplot();
  
});
</script>
<style>

.container
{
/* width: 860px; 01/26/2019 */
margin-top: 0px;
margin-right: auto;
margin-bottom: 0px;
margin-left: auto;
}
fieldset.options{
	border: 3px solid #6DC6E7;
	background-color: #FFFFFF;
	width:575px;height:100px;
}
#editor { 
  position: relative;
  width: 450px;
  height: 275px;
}
    
#plotdiv {
  width: 1000px;
  height: 400px;
  border: 1px solid #e3e3e3;
  border-radius: 4px;
}

#plotbutton{
  width: 120px;
  margin-left: 20px;
}

.twocolumns
{
columns:100px 2;
-webkit-columns:100px 2; /* Safari and Chrome */
-moz-columns:100px 2; /* Firefox */
}

#menu
{
list-style-type:none;
margin:0;
padding:0;
overflow:hidden;
} 

.listitem
{
display:inline;
float:left;
}

a.menuitem:link,a.menuitem:visited
{
display:block;
width:130px;
height:40px;
font-weight:bold;
text-align:center;
padding:4px;
text-decoration:none;background-color:#6DC6E7;
color:#FFFFFF;
}

a.menuitem:hover,a.menuitem:active
{
background-color:#0022B4;
color:#FFFFFF;
}

</style>
</head>

<body>

  <div class="container" style="width:1000px">

	<div class="page-header"> 
		<div  style="float:left;color:#0022B4">
			<h1>QI Tools</h1>
			<h2>Positive deviance: identifying deviants with probability distributions</h2>
			<h3 style=" color:red;font-style:italic">Under construction</h3>
		</div>
		<div style="float:right;color:#0022B4; margin-top: 19px">
			<img src="images/SoMWich_1C_UnitHorz_72dpi.jpg" alt="KUSM-W logo"/>
		</div>
	</div>
	<div style="clear:both"></div>

	<ul id="menu">
	<li class="listitem"><a href="../../home/www/" class="menuitem">Home</a></li>
	<li class="listitem"><a href="../../ishikawa/www/" class="menuitem">Ishikawa<br/>diagram</a></li>
	<li class="listitem"><a href="../../pareto/www/" class="menuitem">Pareto &amp; Sorted<br/>bar charts</a></li>
	<li class="listitem"><a href="../../charts/www/" class="menuitem">Process control &amp; Run charts</a></li>
	<li class="listitem"><a href="../../positivedeviance/www/" class="menuitem">Positive<br/>deviance</a></li>
	<li class="listitem"><a href="../../processmap/www/" class="menuitem">Process maps</a></li>
	<li class="listitem"><a href="../../home/www/measures.html" class="menuitem">Specific <br/>quality measures</a></li>
	</ul> 
	<div id="">&nbsp;</div> 
	
		<!-- Left column -->
		<div style="width:500px;float:left">
			<form class="well" id="paramform" target="target" style="width:500px;">
			<fieldset style="border: 3px solid #6DC6E7; background-color: #FFFFFF; width:500px;height:850px;">
			<legend style="font-weight:bold">Enter inputs</legend>
			<ul>
				<li>Before using this type of plot:
					<ul>
						<li>Have consent from participants to share data, else anonimize the data. Some groups may choose not to attribute data, or only to attribute the positive deviants' data on the first improvement cycle.</li>
						<li>Conduct the positive deviance seminar at the time, or soon after sharing data. The rational is that feedback can generate much emotion (Payne, 2016 PMID <a href="http://pubmed.gov/27412170">27412170</a>) and is more effective if the solution to improvement is provided(Hysong et al, 2009 PMID <a href="http://pubmed.gov/19194332">19194332</a>). However, these approaches are uncommonly done(Hysong et al, 2021 PMID <a href="http://pubmed.gov/33830786">33830786</a>). </li>
					</ul>
				</li>
				<li>Replace the data below in this example <a href="https://en.wikipedia.org/wiki/Probability_distribution">probability distribution</a> with your observations or.</li>
				<li>Upload a csv file formatted as in this <a href="example.csv">example file</a>.
				<input type=file id=files name=files style="width:200px"/>
				1st row is column names? <select id="header" name="header"><option value=TRUE selected> TRUE </option> <option value=FALSE> FALSE </option> </select>
				</li>
				<li>Separate columns with commas. <a id="addcommas" href="#">Click here</a> to add commas.</li>
			</ul>
			<div id="editor">subject,group,count,observations,
 1,1, 3,30,
 2,1, 8,25,
 3,1, 5,20,
 4,1, 3,15,
 5,2, 6,14,
 6,2, 5,13,
 7,2, 6,12,
 8,2, 5,12,
10,3, 4,11,
11,3, 5,11,
12,3, 2,11,
13,3, 5,11,
14,4, 3, 9,
15,4, 2, 8,
16,4, 3, 8,
17,5, 3, 7,
18,5, 1, 6,
19,5, 1, 4,
20,6, 14, 21,
21,6, 3, 4,
			</div>	
            <br />
            <label>Topic (for title - optional):</label> 
            <input type="text" id="topic" value="this field not used"/> <br/>

            <label>Subjects (people,sites,units,etc):</label> 
            <input type="text" id="subject_label" value="Site"/> <br/>
				
	    <label>Subgroup by group (column 2)? </label> <select id="subgroup" name="subgroup"><option value=NO selected> No </option> <option value=YES> YES </option> </select><br/>

            <label>Outcome:</label> 
            <input type="text" id="outcome_label" value="Example bad outcome"/>

            <label>Type</label> 
            <select id="outcome_type">
              <option value="NA" selected>NA</option>
              <option value="b">Bad</option>
              <option value="g">Good</option>
            </select> <br />

            <label>Benchmark:</label> 
            <input type="text" id="benchmark_label" value="Best published report"/> Value <input type="text" id="benchmark_value" style="width:25px" value="0.70"/>  <br />

	<label>Data type</label> 
            <select id="data_type">
              <option value="p" selected>Proportions</option>
              <option value="m">Means</option>
            </select><br />
	<label>Plot type</label> 
            <select id="output_type">
              <option value="d">Distribution</option>
              <option value="f" selected>Forest plot</option>
            </select> <span>X-axis limits: <input type="text" id="x_min" value="0" style="width:25px" /> to <input type="text" id="x_max" value="1" style="width:25px" /></span><br />
	<label>Threshold (#observations per row required for plotting on histogram):</label> 
            <input type="text" id="threshold_observations" style="width:25px" value="4"/> <br/>

	<label>Threshold for PDs (colors PDs green when outcome type not NA):</label>  
            <input type="text" id="threshold_value" style="width:25px" value="0.75"/> <br/>

            <div style="display:none"><label>Plot logo:</label> 
            <select id="theme">
              <option value="white" selected>None</option>
              <option value="KU">KU</option>
            </select></div>

            <br />
            <button class="btn btn-small btn-primary" id="plotbutton"><i class="icon-ok icon-white"></i> Update Plot</button>
          </fieldset>
        </form>
	</div>
	
	<!-- Right column-->
    <div style="width:450px;float:right;background-color: #FFFFFF;">
	<fieldset style="border: 3px solid #6DC6E7; background-color: #FFFFFF; width:450px;">
		<legend style="font-weight:bold">Example scenarios</legend>
		<h3 style="margin-bottom:0px">Scenario 1</h3>
		<div><input type="radio" name="example" class="example" value="ex_1" id="ex_1" checked /><label for="ex_1">A review of bad outcomes at 20 sites with 237 observations (patient encounters).</label></div>
			<ul>
				<li>In this analysis, was the mean of the 20 sites better or worse than the published benchmark?</li>
				<li>In this analysis, which points (indicate the points by their percentages on the x axis) would you choose to be positive deviants?</li>
			</ul>
		<h3 style="margin-bottom:0px">Scenario 2</h3>
		<div><input type="radio" name="example" class="example" value="ex_2" id="ex_2"/><label for="ex_2">The same 20 sites with 237 observations (patient encounters).</label></div>
			<ul>
				<li>Pending content...</li>
			</ul>
		<div><input type="radio" name="example" class="example" value="ex_3" id="ex_3"/><label for="ex_3">The same anlaysis, but...</label></div>
			<ul>
				<li>Pending content...</li>
			</ul>
		</fieldset>
	  </div>
	
	<div style="clear:both">&nbsp;</div>
	<div >Hint: if bad spacing between title/footer text and the plot, click 'png' file type in upper right of plot. Then after the image opens in a new browser window, alter the image's dimensions in the url of the window. <a href="images/plot.resize.png" title="Click to display example in a new window" target="_blank">Click here to see screen capture of an example</a>.&nbsp;<img src="images/External.svg.png" alt="opens in new window" width="13"></div>
      <div class="span9">
        <div id="plotdiv"></div>
      </div>
	      <div class="span12">
        <h3>About</h3>
        <h4>Technical details</h4>
        <p>This is an OpenCPU application.</p>
		<p>Please help improve this application <br/>by editing the source code.</p>
        <table class="table table-striped">
          <tr><th>Resource</th> <th>link</th></tr>
          <tr><td>Package Info</td> <td><a href=".." target="blank">link</a></td></tr>       
          <tr><td>Function Source</td><td><a href="../R/positivedeviance/print" target="blank">link</a></td><td></tr>
          <tr><td>Source Code</td><td><a href="https://github.com/qitools/positivedeviance" target="blank">link</a></td><td></tr>
          <tr><td>Help Page (html)</td><td><a href="../man/qitools/html" target="blank">link</a></td><td></tr>
        </table>
               
      </div>
</div> <!-- end of container -->
<script>
//For gh-pages
//Page history and edit
var pagename = location.pathname.split('/').slice(-1);
if (pagename.length < 1){pagename = "index.html"}
document.write("<div style='text-align:center'><a href='https://github.com/qitools/positivedeviance/blob/master/inst/www/" + pagename + "'>Edit this page</a> - <a href='https://github.com/qitools/positivedeviance/commits/master/inst/www/" + pagename + "'>Page history</a></div>")
</script>
</div>
<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>	
</body>
</html>

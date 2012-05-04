
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
    <style type="text/css">

        html {
            height: 100%;
        }
        
        body {
            background-color: inherit; 
            color: inherit; 
            font-family: Verdana; 
            font-size: x-small; 
            margin: 0; 
            height: 100%;
        
        }
        
        a:active {
            color: #CC0000; 
        }
        
        a:link {
            color: #CC0000; 
        }
        
        a:visited {
            color: #CC0000; 
        }
        
        .msmMismatchAllele {
            background-color: #CC0000;
            color: white;
            font-weight: bold;
        }
        
        .msmMissingAllele {
            background-color: #FFCCCC;
        }
        
        .msmInterpolatedAllele {
            background-color: blue;
            color: white;
        }
        
        .msmGrid {
            border-collapse: separate;
        }
        
        .msmGridContent {
            padding: 0;	
            border: 1px solid #7EACB1; 			
        }
        

        .msmGridUpperPanel, .msmGridLowerPanel {
            padding: 3px;	
            border-left: 0;
            border-right: 0;	
            background-color: #F4FAFB; 
            color: #2A769D;	 
            font-family: Verdana; 
            font-size: x-small; 	
        }
        
        .msmGridUpperPanel {
            border-top: 0px;
            border-bottom: 1px solid;
            border-color: #7EACB1; 
        }
        
        .msmGridMiddlePanel {
            border: 0;	
        }
        
        .msmGridLowerPanel {
            border-top: 1px solid;
            border-bottom: 0px; 
            border-color: #C2D4DA; 
        }
        
        .msmGridUpperPanel td, .msmGridLowerPanel td {
            color: #2A769D;	 
            font-family: Verdana; 
            font-size: x-small; 		
        }
        
        
        .msmTable {
            border: 0;
            border-spacing: 0;
            border-collapse: collapse;
            empty-cells: show;
            width: 100%;
            font-family: Verdana; 
            font-size: x-small; 			
        }
        
        .msmTableSeparate {	
            border-collapse: separate;		
        }
        
        .msmTable td {
            padding: 3px; 
            border-bottom: 1px solid; 
            border-top: 0px;
            border-left: 0px;
            border-right: 1px solid; 
            border-color: #C2D4DA;  
            white-space:nowrap;
        }
        
            
        .msmTable .msmTableHeader, .msmTable .msmTableHeader td {
            background-color: #B7D8DC;	
            color: #000000; 
            border-bottom: 1px solid; 
            border-right: 1px solid; 
            border-color: #7EACB1; 
            background-repeat: repeat-x;		
            vertical-align: top;
            white-space:nowrap;
        }
        
        .msmPointer {
            cursor: pointer;
        }
        
        
        .msmTableHeaderBtn {
            width: 100%;
            font-family: Verdana; 
            font-size: x-small; 		
        }
        
        .msmTableHeader .msmTableHeaderBtn td {
            background: transparent;
            padding: 0;
            border: 0;
            white-space: nowrap;		
        }
        
        .msmTableSelectRow {
            background-color: #FFFF66; 
            color: #000000;
        }

</style>
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>

        <div style="margin-left:5%; margin-right:5%"><br>
        <table cellspacing="0" class="msmGrid">
                <tr>
                        <td class="msmGridContent">
                                <div class="msmGridUpperPanel">
                                        <div style="font-size:x-small;">
                                                <table>
                                                        <tr>
                                                                <td style="width:500px;">
                                                                <span style="font-size:15px;"><b>grainscape package</b></span><br>
                                                                
                                                                <br><span style="font-size:12px;">
                                                                Grains of connectivity and minimum planar graph modelling of landscape connectivity using resistance surfaces<br><br>
                                                                Given a landscape resistance surface, functions in this package create grains of connectivity and minimum planar graph models that can be used to calculate
                                                                effective distances for landscape connectivity at multiple scales. Distributed with \code{SELES}
                                                                (Spatially Explicit Landscape Event Simulator; Fall and Fall, 2001) software. The package will currently run only on a Windows-based platform.
                                                                </span><br><br>
                                                                <span style="font-size:11px;"<br />Paul Galpern<br>Natural Resources Institute<br> University of Manitoba, 
                                                                Winnipeg, Canada<br>Spring 2012<br>
                                                                </span>
                                                                </td>
                                                        </tr>
                                                        <tr>
                                                                <td>&nbsp;
                                                                </td>
                                                        </tr>
                                                </table>
                                        </div>
                                </div>
                        </td>
                </tr>
        </table>
        <br>
        <br>
        <table cellspacing="0" class="msmGrid">
                <tr>
                        <td class="msmGridContent">
                                <div class="msmGridUpperPanel">
                                        <span style="font-size:small;">If you are using the most recent version of R</span>
                                </div>
                                <div class="msmGridMiddlePanel">
                                        <table cellspacing="0" class="msmTable msmTableSeparate">
                                                
                                                <tr>
                                                        <td width=100>left</td>
                                                        <td width=400>right</td>
                                                </tr>
                                                <tr>
                                                        <td width=100>left</td>
                                                        <td width=400>right</td>
                                                </tr>
                                            
                                        </table>
                                </div>
                        </td>
                </tr>
        </table>
        <br>
        <br>
         <table cellspacing="0" class="msmGrid">
                <tr>
                    <td>
                        Paul Galpern<br>Email: <a href="mailto:pgalpern@gmail.com">pgalpern@gmail.com</a><br>Web: <a href="http://borealscape.ca">borealscape.ca</a>
                    </td>
                </tr>
         </table>
        </div>
</body>
</html>

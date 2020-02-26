<?php

date_default_timezone_set('PST8PDT');  // set default timezone to 'Pacific with Daylight Savings Time (PDT)'
$optionsForJsonEncode = JSON_HEX_TAG+JSON_HEX_AMP+JSON_HEX_APOS+JSON_HEX_QUOT+JSON_PRETTY_PRINT+JSON_UNESCAPED_SLASHES;


//============================
//======= ROOMS ============
$numberOfRooms = 130; //approx.
$roomDataTemplateJSONFile = "roomData-template.json";
$roomDataTemplate = [];
for($i=1; $i<=$numberOfRooms; $i++)
{
	$roomDataTemplate[ "room" . sprintf("%03u",$i) ] = 
		[
			"ceilingHeight" => "unspecified",
			"roomName" => "unspecified",
			"notes" => ""
		];
}
$roomDataTemplateJSONOutputStream = fopen($roomDataTemplateJSONFile,'w');
fwrite(
	$roomDataTemplateJSONOutputStream,
	json_encode($roomDataTemplate, $optionsForJsonEncode)
);
fclose($roomDataTemplateJSONOutputStream);






//============================
//======= DOORS ==============
$numberOfDoors = 178;
$doorDataTemplateJSONFile = "doorData-template.json";
$doorDataTemplate = [];
for($i=1; $i<=$numberOfDoors; $i++)
{
	$doorDataTemplate[ "door" . $i ] = 
		[
			"doorType" => "unspecified", //options are "standard" "double" and "doubleOpposing"
			"widthOfOpening" => "anchorLength",
			"doorHeight" => "unspecified",
			"doorThickness" => "unspecified",
			"chirality" => "unspecified"
		];
}
$doorDataTemplateJSONOutputStream = fopen($doorDataTemplateJSONFile,'w');
fwrite(
	$doorDataTemplateJSONOutputStream,
	json_encode($doorDataTemplate, $optionsForJsonEncode)
);
fclose($doorDataTemplateJSONOutputStream);








//============================
//======= WINDOWS ============
$numberOfWindows = 63;
$windowDataTemplateJSONFile = "windowData-template.json";
$windowDataTemplate = [];
for($i=1; $i<=$numberOfWindows; $i++)
{
	$windowDataTemplate[ "window" . $i ] = 
		[
			"widthOfOpening" => "anchorLength",
			"sillHeight" => "unspecified",
			"windowVerticalExtent" => "unspecified"
		];
}
$windowDataTemplateJSONOutputStream = fopen($windowDataTemplateJSONFile,'w');
fwrite(
	$windowDataTemplateJSONOutputStream,
	json_encode($windowDataTemplate, $optionsForJsonEncode)
);
fclose($windowDataTemplateJSONOutputStream);




?>
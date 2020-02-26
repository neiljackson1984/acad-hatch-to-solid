<?php
date_default_timezone_set('PST8PDT');  // set default timezone to 'Pacific with Daylight Savings Time (PDT)'
$doorDataJSONFile = "doorData.json";
$windowDataJSONFile = "windowData.json";
$roomDataJSONFile = "roomData.json";

$outputLispFile = "data.lsp";
$outputLispStream = fopen($outputLispFile,'w');



/*
	Strips c-style comments (i.e. comments of the form  //...(EOL)     OR      /* ... * /) from the string argument.
 * 
 */
function stripCStyleComments($x)
{
	$y = $x;
	$y = preg_replace('!/\*.*?\*/!s', '', $y);  //strips block comments of the form /*...*/
	$y = preg_replace('/\/\/.*$/m', "", $y); //strips single line comments of the form //...
	return $y;
};

$doorData = json_decode(stripCStyleComments(file_get_contents($doorDataJSONFile)));
echo json_last_error_msg(); echo "\n";

$windowData = json_decode(stripCStyleComments(file_get_contents($windowDataJSONFile)));
echo json_last_error_msg(); echo "\n";

$roomData = json_decode(stripCStyleComments(file_get_contents($roomDataJSONFile)));
echo json_last_error_msg(); echo "\n";

fwrite(
	$outputLispStream,
	";;;  " .    "============================================================================="       . "\n" .
	";;;  " .    "This lisp script defines the variables doorData, windowData and roomData, each an"       . "\n" .
	";;;  " .    " associative array of data structures describing the doors and windows."        . "\n" .
	";;;  " .    " This file was generated at " . date("Y/m/d H:i") . " by " . basename(__FILE__) . ","   . "\n" .
	";;;  " .    "using " . $doorDataJSONFile . " and " . $windowDataJSONFile . " as sources. " . "\n" .
	";;;  " .    "============================================================================="       . "\n" .
	
	"\n\n\n\n" .
	"(setq doorData" . "\n" .
	toLisp($doorData, 1) . 
	")" . 
	
	"\n\n\n" .
	"(setq windowData" . "\n" .
	toLisp($windowData, 1) . 
	")" .

	"\n\n\n" .
	"(setq roomData" . "\n" .
	toLisp($roomData, 1) . 
	")" .
	
	
	"\n" .
	""
);


fclose($outputLispStream);


function toLisp($x, $tabLevel=0)
{
	if(is_string($x))
	{
		$returnValue = str_repeat("\t", $tabLevel) . "\"" . addslashes($x) . "\"";
	} 
	elseif (is_object($x) || is_array($x))
	{
		$returnValue = "";
		$returnValue .= str_repeat("\t", $tabLevel) . "(list " . "\n";
		
		foreach($x as $key => $value)
		{
			$returnValue .= 
				str_repeat("\t", $tabLevel+1) . "(cons " . toLisp($key) . "\n" . 
				toLisp($value, $tabLevel + 2) . "\n" . 
				str_repeat("\t", $tabLevel+1) . ")" . "\n"; //I am really assuming that $key will always be a string.
		}
		
		$returnValue .= str_repeat("\t", $tabLevel) . ")" . "\n";
	} 
	else
	{
		$returnValue = $x;
	}
	
	return $returnValue;
}

?>
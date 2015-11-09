<?php

/********* SUBMIT DATA TO mySQL DATABASE ***********/

include('dbConnect.php');
require("../../includes/config.php");

/*******UTILITIES********/
function mysql_insert($table, $inserts) {
    $values = array_map('mysql_real_escape_string', array_values($inserts));
    $keys = array_keys($inserts);
    return mysql_query('INSERT INTO `'.$table.'` (`'.implode('`,`', $keys).'`) VALUES (\''.implode('\',\'', $values).'\')');
}


/* GET TABLE NAME */
$tab = $_POST['table'];

/* DECODE DATA FROM submitDATA FUNCTINO */
$trials = json_decode($_POST['json'],true);
$order = json_decode($_POST['order'],true);
//$cond = json_decode($_POST['cond'],true);



// for each element in the trials array, insert the row into the mysql table
// we start with the first screen that isn't a consent or intro
// CHECK the jspsych data structure to make sure this is right for you!!!
for($i=3;$i<(count($trials)-1);$i++)
{
    $to_insert["rt"] = $trials[$i]["rt"];
    // here, i'm grabbing literally the 6th character of the string, because i can't 
    // get the key -> value to work. there is probably a better way.
    $to_insert["response"] = (int) $trials[$i]["responses"][6];
    $to_insert["scenNum"] = $order[$i-3]+1; //remember, it started with zero.
    $to_insert["trialNum"] = $i-2; //we want the first trial to be 1
    //$to_insert["cond"] = $cond;
    
    
    // this loop just checks whether or not there's a user or visitorid set
    // make sure your db table has a column for each of these values! at least
    // one should always be zero.
    if(empty($_SESSION["userid"]) && !empty($_SESSION["visitorid"]))
    {
    	$to_insert["visitorid"] = $_SESSION["visitorid"];
    	$to_insert["userid"] = 0;
    	
    }
    elseif(empty($_SESSION["visitorid"]) && !empty($_SESSION["userid"]))
    {
    	$to_insert["visitorid"] = 0;
    	$to_insert["userid"] = $_SESSION["userid"];
    	
    }
    else
    {
    	$to_insert["visitorid"] = 0;
    	$to_insert["userid"] = 0;
    	
    }
    
    // insert everything we populated!
    $result = mysql_insert($tab, $to_insert);
}




// confirm the results
if (!$result) {
	// if there was an error...
    die('Invalid query: ' . mysql_error());
} else {
	// if no error, check whether they're a user
	if (!empty($_SESSION["email"]))
	{
		// if so, make sure to update the emails db with a 1 in this column
		// DON'T FORGET to make a column for your study in the table useremails
		$try = query("UPDATE useremails SET studyname = ? WHERE email = ?",true, $_SESSION["email"]);

     }  

	
}

?>



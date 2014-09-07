<html>
<head>
	<title>JON HEJ JON</title>
</head>
<body>
<pre>
<?php
var_dump("\nRequest",getallheaders());
var_dump("\n\nGET\n",$_GET);
var_dump("\n\nPOST\n",$_POST);
var_dump("\n\nFILES\n",$_FILES);
var_dump("\n\nResponse\n",apache_response_headers());
foreach($_FILES as $key => $val) 
	var_dump(file_get_contents($val['tmp_name']));
// curl -i --data "uri=ni:///sha-256;somehash&msgid=1337&loc=1&loc=2&ext=" http://localhost/index.php


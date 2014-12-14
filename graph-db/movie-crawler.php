<?php

$page = file_get_contents("http://www.imdb.com/chart/top");
//$page = file_get_contents("imdb.txt");

//preg_match_all('/\/title\/tt([0-9]+)\/[^"]+"\stitle[^>]+>([^<]+)/si', $page, $matches);
preg_match_all('/\/title\/tt([0-9]+)\/[^"]+"\stitle[^>]+>([^<]+)<[^<]+<[^>]+>\(([0-9]+)\)/si', $page, $matches);


$ids = $matches[1];
$titles = $matches[2];

$movies = [];

for ($i = 0; $i < count($ids); $i++){
	$movies[$ids[$i]] = Array('title' => $titles[$i]);
}

$fh = fopen('movies-with-directors.csv', 'w');
$header = ['movie_id', 'movie_title', 'movie_url', 'director_id', 'director_name', 'actor_id', 'actor_name', 'actor_url'];
fputcsv($fh, $header);

$count = 1;
foreach ($movies as $id => $data) {
	$html = file_get_contents('http://www.imdb.com/title/tt'.$id.'/fullcredits');
	preg_match_all('/<a\shref="\/name\/nm([0-9]+)\/\?ref_=ttfc_fc_dr1"[^>]>([^<]+)</si', $html, $matches);
	$directorId = trim($matches[1][0]);
	$directorName = trim($matches[2][0]);
	preg_match_all('/itemprop="actor"[^<]+<a href="\/name\/nm([0-9]+)\/[^<]+<[^>]+>([^<]+)/si', $html, $matches);

	$ids = $matches[1];
	$name = $matches[2];

	for ($i = 0; $i < count($ids); $i++){
		$row = [
			$id,
			$data['title'],
			'http://www.imdb.com/title/tt'.$id.'/',
			$directorId,
			$directorName,
			$ids[$i],
			$name[$i],
			'http://www.imdb.com/name/nm'.$ids[$i].'/'
		];

		fputcsv($fh, $row);
	}
	echo $count." - ".$data['title']."\n";
	$count++;
	sleep(2);
	//break;
}

fclose($fh);

?>
<?php

$file = 'movies-with-directors.csv';

define('MOVIE_ID', 		0);
define('MOVIE_NAME', 	1);
define('DIRECTOR_ID', 	3);
define('DIRECTOR_NAME', 4);
define('ACTOR_ID', 		5);
define('ACTOR_NAME', 	6);

echo "connect remote:localhost root root;

create database plocal:/home/diego/Descargas/orientdb-community-1.7.10/orientdb-community-1.7.10/databases/movies root root;

create class Pelicula extends V;
create class Director extends V;
create class Actor extends V;

create class Actuo extends E;
create class DirigidaPor extends E;
create class DirigioActor extends E;


";
#--------------------------------------------------------------------
# Genero los vertices de peliculas
#--------------------------------------------------------------------
$fh = fopen($file, "r");
$movies = Array();
while (($row = fgetcsv($fh, 1000, ",")) !== FALSE) {
    $movies[$row[MOVIE_ID]] = $row[MOVIE_NAME];
}
fclose($fh);

foreach ($movies as $id => $name){
	echo "create vertex Pelicula set name='".addslashes($name)."', id='".$id."';\n";
}

echo "\n\n\n";

#--------------------------------------------------------------------
# Genero los vertices de directores
#--------------------------------------------------------------------
$fh = fopen($file, "r");
$actors = Array();
while (($row = fgetcsv($fh, 1000, ",")) !== FALSE) {
    $actors[$row[DIRECTOR_ID]] = $row[DIRECTOR_NAME];
}
fclose($fh);

foreach ($actors as $id => $name){
	echo "create vertex Director set name='".addslashes($name)."', id='".$id."';\n";
}

echo "\n\n\n";

#--------------------------------------------------------------------
# Genero los vertices de actores
#--------------------------------------------------------------------
$fh = fopen($file, "r");
$actors = Array();
while (($row = fgetcsv($fh, 1000, ",")) !== FALSE) {
    $actors[$row[ACTOR_ID]] = $row[ACTOR_NAME];
}
fclose($fh);

foreach ($actors as $id => $name){
	echo "create vertex Actor set name='".addslashes($name)."', id='".$id."';\n";
}

echo "\n\n\n";

#--------------------------------------------------------------------
# Genero las aristas entre Actor ---> Pelicula
#--------------------------------------------------------------------
$fh = fopen($file, "r");
$actorsToMovies = Array();
while (($row = fgetcsv($fh, 1000, ",")) !== FALSE) {
	if (array_key_exists($row[ACTOR_ID], $actorsToMovies)){
		$actorsToMovies[$row[ACTOR_ID]][] = $row[MOVIE_ID];
	}else{
		$actorsToMovies[$row[ACTOR_ID]] = Array($row[MOVIE_ID]);
	}
}
fclose($fh);

foreach ($actorsToMovies as $actorId => $movies){
	foreach ($movies as $movieId) {
		echo "create edge Actuo from (select from Actor where id = '".$actorId."') to (select from Pelicula where id = '".$movieId."')\n";
	}
}

echo "\n\n\n";

#--------------------------------------------------------------------
# Genero las aristas entre Pelicula ---> Director
#--------------------------------------------------------------------
$fh = fopen($file, "r");
$movieToDirector = Array();
while (($row = fgetcsv($fh, 1000, ",")) !== FALSE) {
	if (!array_key_exists($row[MOVIE_ID], $movieToDirector)){
		$movieToDirector[$row[MOVIE_ID]] = $row[DIRECTOR_ID];
	}
}
fclose($fh);

foreach ($movieToDirector as $movieId => $directorId){
	echo "create edge DirigidaPor from (select from Pelicula where id = '".$movieId."') to (select from Director where id = '".$directorId."')\n";
}

echo "\n\n\n";

#--------------------------------------------------------------------
# Genero las aristas entre Director ---> Actor
#--------------------------------------------------------------------
$fh = fopen($file, "r");
$directorToActor = Array();
while (($row = fgetcsv($fh, 1000, ",")) !== FALSE) {
	if (array_key_exists($row[DIRECTOR_ID], $directorToActor)){
		$directorToActor[$row[DIRECTOR_ID]][] = $row[ACTOR_ID];
	}else{
		$directorToActor[$row[DIRECTOR_ID]] = Array($row[ACTOR_ID]);
	}
}
fclose($fh);

foreach ($directorToActor as $directorId => $actors){
	foreach ($actors as $actorId) {
		echo "create edge DirigioActor from (select from Director where id = '".$directorId."') to (select from Actor where id = '".$actorId."')\n";
	}
}

?>
casjobs run -t DR1/500 -n "my query" "select top 10 ra into mydb.this1 from photoobj"
casjobs run -t MyDB/3 -n "my query" "drop table this1"

PASS=$1

if [ "" == "$PASS" ] 
then 
  echo "need password"
  exit
fi

for i in {10..20}
do
# get the lists of cnf files
  filename="results-innodb-$i.txt"
  curl --user santolucito:$PASS "https://api.github.com/search/code?q=innodb+extension:cnf+size:>100&page=$i&per_page=100" | jq '.items | map(.html_url)' > $filename
  
# if the api call fails, you end up with an empty file, and dont do any wgets, just wait 5m 
# clean the format of the lists
  sed -i -e 's/github/raw\.githubusercontent/g' "$filename"
  sed -i -e 's/blob\///g' "$filename"
  sed -i -e 's/\"//g' "$filename"
  sed -i -e 's/\,//g' "$filename"

# wget the files
  while read l; do
    NEW_ID=$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 32 | head -n 1)
    wget -O "downloads/$NEW_ID.cnf" $l
  done <$filename
  sleep 5m #api has rate limit
done

# this will generate duplicates, remove duplicates with 'fdupes -d -N'

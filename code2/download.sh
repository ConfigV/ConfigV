PASS=$1

if [ "" == "$PASS" ] 
then 
  echo "need password"
  exit
fi

for i in {1..2}
do
# get the lists of cnf files
  filename="results$i.txt"
  curl --user santolucito:$PASS "https://api.github.com/search/code?q=mysql+extension:cnf+size:>100&page=$i&per_page=100" | jq '.items | map(.html_url)' > $filename
  
 
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
  #sleep 5m #api has rate limit
done


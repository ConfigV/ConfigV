cnt=1
for filename in downloads/*.cnf 
do
  clear
  cat $filename
  echo
  echo "-------------------------"
  echo "file $cnt"
  cnt=$[cnt + 1]
  echo "Press Enter to Accept, or N to delete"
  read input </dev/tty
  if [ "$input" == "N" ]
  then
    rm $filename
  fi
done

echo "## Contents" ; echo ; 
cat README.md | grep '^## ' | grep -v Contents | sed 's/^## //' | 
  while read -r title ; do 
    link=$(echo $title | tr 'A-Z ' 'a-z-') ; 
    echo "- [$title](#$link)" ; 
    done
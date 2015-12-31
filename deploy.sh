cp tutorial.html index.html
rsync -v *.html ec2:~
ssh ec2 'sudo mv *.html /srv/http/fun/'
rm -f index.html

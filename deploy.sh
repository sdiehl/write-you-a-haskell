cp tutorial.html index.html
rsync -v *.html ec2:~
ssh ec2 'sudo mv *.html /srv/http/fun/'
rsync -v WYAH.pdf ec2:~
ssh ec2 'sudo mv WYAH.pdf /srv/http/fun/'
rm -f index.html

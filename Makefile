all: update restart

update:
	@echo "Pulling the latest changes from the git repository"
	git pull origin master
	chown -R www-data.www-data .

restart: stop start
	
status:
	supervisorctl status

stop:
	service supervisord stop
	sleep 30
	
start:	
	service supervisord start
	supervisorctl status
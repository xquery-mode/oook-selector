# Install MarkLogic Server

* Goto to MarkLogic's products download page: https://developer.marklogic.com/products
* For Red Hat, CentOS, Fedora:
** download the RPM for Red Hat Enterprise Linux / CentOS and
** install by invoking:
```
rpm -i MarkLogic-RHEL6-8.0-6.5.x86_64.rpm
```
* Debian GNU/Linux is not officially supported, you can install it in this way:
** downloading the RPM for Red Hat Enterprise Linux / CentOS and
** and convert it to a Debian package and install this by:
```
sudo apt-get install alien
fakeroot alien --to-deb --verbose MarkLogic-RHEL6-8.0-6.5.x86_64.rpm
sudo dpkg -i marklogic_8.0-6.5_amd64.deb
```
* for other platforms and more information please refer to
[MarkLogic's Installation Guide for All Platforms — Chapter 2, Procedures](https://docs.marklogic.com/guide/installation/procedures)

# Start MarkLogic Server
* On Linux by invoking:
```
sudo /etc/init.d/MarkLogic start
```
* For all other platforms please refer to section
["Starting MarkLogic Server"](https://docs.marklogic.com/guide/installation/procedures#id_92457)
of MarkLogic's Installation Guide.

# Configure MarkLogic
You'll find a detailed description in MarkLogic's Guide in the section
[Configuring a Single Host or the First Host in a Cluster](https://docs.marklogic.com/guide/installation/procedures#id_60220).

Short version:
* Log into the Admin Interface in a browser. It is on port 8001 of the host on which MarkLogic is running. For example, if it's running on localhost, just go to http://localhost:8001.
* On the "Server Install" page, click "OK".
* After a while, on the "Join a Cluster" page, click "Skip".
* After a while, on the "Security Setup" page:
** in the "Admin" input box enter "admin",
** in the "Password" input box enter "admin",
** in the "Confirm Password" input box enter "admin",
** in the "Realm" input box keep the text "public", and
** click "OK".
* You will be prompted to log in with your admin username and password.

# Remove MarkLogic
You'll find a detailed description in MarkLogic's Guide in the section
[Removing MarkLogic Server](https://docs.marklogic.com/guide/installation/procedures#id_53295).

* On Debian GNU/Linux do this:
```
sudo /etc/init.d/MarkLogic stop
sudo dpkg -r marklogic
# or even:
# dpkg --purge marklogic
```
* You might also want to remove all stored data and databases of the MarkLogic server, by invoking:
```
sudo rm -R /var/opt/MarkLogic
```

# Setup an intial database and REST server
To configure a minimal MarkLogic REST server
including a content database and a modules database, together with forests,
run:
```
  cd ~/src/oook-selector/doc/example-db
  . create-rest-instance.sh 
```

Note: This script expects MarkLogic to be running on localhost with an admin:admin account,
that is, the username is "admin" and the password is "admin".

To remove this REST server together with its content database, modules database, and the forests,
run:
```
  cd ~/src/oook-selector/doc/example-db
  . delete-rest-instance.sh
```

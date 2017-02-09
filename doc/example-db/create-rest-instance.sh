curl --anyauth --user admin:admin \
     -X POST \
     -i \
     -d@"rest-instance.xml" \
     -H "Content-type: application/xml" \
     http://localhost:8002/v1/rest-apis
